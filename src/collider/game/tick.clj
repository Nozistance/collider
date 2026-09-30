(ns collider.game.tick
  "The tick: its phases and how they run."
  (:require [clojure.data.int-map :as i]
            [collider.game.state :as state]
            [collider.game.schema :as schema]
            [collider.game.cost :as cost]
            [collider.game.deltas :as deltas]
            [collider.game.deltas.record :as types]
            [collider.game.detector :as detector]
            [collider.log :as log]
            [collider.game.systems.block.updates :as block-updates]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.brewing :as brewing]
            [collider.game.systems.camera :as camera]
            [collider.game.systems.campfires :as campfires]
            [collider.game.systems.chat :as chat]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.compasses :as compasses]
            [collider.game.systems.consume :as consume]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.daynight :as daynight]
            [collider.game.systems.dripleaf :as dripleaf]
            [collider.game.systems.effects :as effects]
            [collider.game.systems.experience :as experience]
            [collider.game.systems.explosions :as explosions]
            [collider.game.systems.falling :as falling]
            [collider.game.systems.furnaces :as furnaces]
            [collider.game.systems.hanging :as hanging]
            [collider.game.systems.geysers :as geysers]
            [collider.game.systems.inventory :as inventory]
            [collider.game.systems.items :as items]
            [collider.game.systems.jukebox :as jukebox]
            [collider.game.systems.keepalive :as keepalive]
            [collider.game.systems.mobs :as mobs]
            [collider.game.systems.orbs :as orbs]
            [collider.game.systems.packets :as packets]
            [collider.game.systems.players :as players]
            [collider.game.systems.pose :as pose]
            [collider.game.systems.projectiles :as projectiles]
            [collider.game.systems.random.tick :as random-tick]
            [collider.game.systems.signs :as signs]
            [collider.game.systems.sleep :as sleep]
            [collider.game.systems.spawning :as spawning]
            [collider.game.systems.tnt :as tnt]
            [collider.game.systems.weather :as weather-system]
            [collider.game.systems.damage :as damage])
  (:import (collider.game.deltas.record Deltas)))

(set! *warn-on-reflection* true)

(def packet-systems
  "The systems a player event drives.
  They all run before the level tick."
  [#'chunks/chunk-streaming
   #'players/player-list
   #'players/players
   #'camera/camera
   #'blocks/block-edits
   #'packets/by-player
   #'sleep/sleep
   #'inventory/inventory
   #'containers/containers
   #'chat/chat
   #'keepalive/keepalive])

(def entity-systems
  "The systems that step the entities of the level."
  [#'items/items
   #'orbs/orbs
   #'falling/falling-blocks
   #'damage/countdown
   #'mobs/mobs-system
   #'tnt/tnt-system
   #'projectiles/projectiles
   #'hanging/hanging-checks
   #'damage/damage])

(def systems
  "Every system the level tick drives off the world it is given."
  (into packet-systems entity-systems))

(def phases [[#'spawning/placing]
             [#'chunks/chunk-loading]
             packet-systems
             [#'hanging/hanging-uses]
             [#'chunks/arrival-streaming]
             [#'effects/effects]
             [#'consume/consume #'compasses/compasses]
             [#'pose/pose]
             [#'daynight/daynight]
             [#'block-updates/block-updates
              #'dripleaf/dripleaf-tilt
              #'containers/rechecks]
             [#'block-updates/fluid-updates]
             [#'chunks/unloading
              #'random-tick/random-ticks
              #'weather-system/weather]
             [#'block-updates/block-flush
              #'players/late-tracking]
             entity-systems
             [#'explosions/blasts]
             [#'items/pickups #'orbs/pickups #'containers/broadcast]
             [#'furnaces/furnace-cooking
              #'campfires/campfire-cooking
              #'brewing/brewing
              #'geysers/geysers
              #'jukebox/jukebox-songs
              #'signs/sign-editors]
             [#'blocks/acks #'experience/experience]
             [#'detector/observe]])

(def server-systems
  "The systems that run once for the whole server, not per level.
  They see the players of every level."
  #{#'players/player-list #'daynight/daynight})

(defn- server-phase? [phase]
  (boolean (some server-systems phase)))

(def dims
  "The dimensions the tick runs, in a fixed order."
  schema/dims)

(def ^:private home (first dims))

(defn- event-dim [world [tag x]]
  (case tag
    :player-join home
    :chunk-loaded x
    (or (state/dim-of world x) home)))

(defn- input-of [dim events]
  (if (= home dim)
    (deltas/input events)
    (assoc deltas/empty-deltas :input (vec events))))

(defn- entered [ds [w views] dim]
  (let [[w' lv] (state/entered w dim (ds dim))]
    [w' (if lv {dim lv} views)]))

(defn- awaited? [world dim]
  (let [spawning (:spawning world)]
    (and (not (deltas/vacant? spawning))
         (some #(= dim (:dim (val %))) spawning))))

(defn- asleep? [world dim]
  (and (not (identical? home dim))
       (state/idle? (get (:levels world) dim))
       (not (awaited? world dim))))

(defn- awake-views [world views]
  (reduce (fn [vs dim]
            (cond (asleep? world dim) (dissoc vs dim)
                  (contains? vs dim) vs
                  :else (assoc vs dim (state/level world dim))))
          (or views {}) dims))

(defn- begin [world events]
  (let [by (group-by #(event-dim world %) events)
        ds (into {} (map (fn [dim] [dim (input-of dim (by dim))]))
                 dims)
        [w views] (reduce #(entered ds %1 %2) [world nil] dims)
        heeded (fn [[dim d]] [dim (state/heeded w dim d)])]
    [w (into {} (map heeded) ds) (awake-views w views)]))

(defn- skipped! [world s dim ^Throwable t]
  (let [unit (log/name-of s)
        msg (str "system " unit " failed in " (name (or dim :server))
                 ", its deltas this tick are dropped")]
    (log/failure! unit msg t)
    (some-> (:failures world) (swap! conj [unit t]))))

(defn- guarded [world s dim f]
  (fn []
    (try (f)
         (catch Throwable t
           (skipped! world s dim t)
           deltas/empty-deltas))))

(defn- server-thunk [world s [view d]]
  (let [f (guarded world s nil #(s view d))]
    #(deltas/with-dim (f) nil)))

(defn- thunk [world lv d server dim s]
  (if (server-systems s)
    (server-thunk world s @server)
    (guarded world s dim #(s lv d))))

(defn- runs-in? [dim s]
  (or (identical? home dim) (not (server-systems s))))

(defn- held? [v]
  (if (coll? v) (not (empty? v)) (boolean v)))

(defn- tagged? [tags ds]
  (loop [i (dec (count ds))]
    (cond (neg? i) false
          (contains? tags (nth (nth ds i) 0)) true
          :else (recur (dec i)))))

(defn- kept? [lv ks]
  (loop [i (dec (count ks))]
    (cond (neg? i) false
          (let [k (nth ks i)]
            (held? (if (vector? k) (get-in lv k) (get lv k))))
          true
          :else (recur (dec i)))))

(defn- due? [lv n]
  (zero? (rem (long (:tick lv)) (long n))))

(defn- woken? [w lv d]
  (let [types (:types w) events (:events w) ds (:deltas w)
        ks (:keys w) every (:every w)]
    (boolean
      (or (and every (due? lv every))
          (and types (state/holds-types? lv types))
          (and events (tagged? events (deltas/input-of d)))
          (and ds (tagged? ds (deltas/world-of d)))
          (and ks (kept? lv ks))))))

(defn awake?
  "Returns true when system s may have work in level lv on deltas d.
  Its :wake names the events, the deltas, the entity types, the keys
  of the level and the period in ticks that are its work. A system
  with no :wake, or the :wake :always, never sleeps."
  [s lv d]
  (let [w (:wake (meta s))]
    (or (not (map? w)) (woken? w lv d))))

(defn- awake-in [lv d dim phase]
  (filterv #(and (runs-in? dim %) (awake? % lv d)) phase))

(defn- level-deltas [world lv ds server phase dim]
  (let [d (get ds dim)
        ks (awake-in lv d dim phase)]
    (when (pos? (count ks))
      (let [lv (assoc lv :server world)
            fs (mapv #(thunk world lv d server dim %) ks)
            timed (cost/timer (:tick world))]
        (deltas/with-dim
         (deltas/run-weighed cost/heavy? timed ks fs)
         dim)))))

(defn- view [world views dim]
  (or (get views dim) (state/level world dim)))

(defn- merged [ds] (reduce deltas/merge (map ds dims)))

(defn- away [world eid]
  (let [dim (state/dim-of world eid)]
    (when (and dim (not= home dim)) dim)))

(defn- stray-dim [world delta]
  (when (identical? :remove-entity (nth delta 0))
    (away world (nth delta 1))))

(defn- part [ws es]
  (types/->Deltas (vec ws) (into (i/int-map) es) [] []))

(defn- rehomed [world ^Deltas d]
  (let [ws (group-by #(stray-dim world %) (deltas/world-of d))
        es (group-by #(away world (key %)) (deltas/entities-of d))
        dims' (disj (into (set (keys ws)) (keys es)) nil)]
    (into {home (assoc (part (ws nil) (es nil))
                  :out (deltas/out-of d) :input (deltas/input-of d))}
          (map (fn [dim] [dim (part (ws dim) (es dim))]))
          dims')))

(defn- strays? [world ^Deltas d]
  (or (some #(away world %) (keys (deltas/entities-of d)))
      (some #(stray-dim world %) (deltas/world-of d))))

(defn- relocated [world pd]
  (let [d (pd home)]
    (if (strays? world d)
      (merge-with deltas/merge (assoc pd home deltas/empty-deltas)
                  (rehomed world d))
      pd)))

(defn- phase-deltas
  ([world ds phase]
   (phase-deltas world (awake-views world nil) ds phase))
  ([world views ds phase]
   (let [server (delay [(state/server-view world) (merged ds)])
         of (fn [pd dim]
              (let [lv (get views dim)
                    d (when lv
                        (level-deltas world lv ds server phase dim))]
                (if d (assoc pd dim d) pd)))
         pd (reduce of {} dims)]
     (if (and (contains? pd home) (server-phase? phase))
       (relocated world pd)
       pd))))

(def ^:private left-behind #{:chunks-sent :tracking})

(defn- left-behind? [d]
  (let [tag (nth d 0)]
    (or (contains? left-behind tag)
        (and (identical? :merge-entity tag)
             (contains? (nth d 2) :chunk-quota)))))

(defn- stay [ds]
  (filterv #(not (left-behind? %)) ds))

(defn- departed ^Deltas [^Deltas d changes]
  (let [gone (map #(nth % 1) changes)
        es (deltas/entities-of d)
        kept (fn [m eid]
               (if-let [v (get m eid)] (assoc m eid (stay v)) m))
        es' (reduce kept es gone)]
    (assoc d :entities es')))

(defn- arrivals [ds changes]
  (reduce (fn [ds c]
            (update ds (nth c 2) deltas/merge
                    (types/->Deltas [c] (i/int-map) [] [])))
          ds changes))

(defn- crossing [[world ds] dim ^Deltas d changes]
  (let [d (departed d changes)]
    [(state/cross (state/apply-in world dim d) dim changes)
     (arrivals (update ds dim deltas/merge d) changes)]))

(declare step)

(defn- handed [acc ^Deltas d]
  (reduce (fn [acc [dim sub]]
            (let [sd (deltas/of-vec sub)]
              (step acc dim (deltas/with-dim sd dim))))
          acc (state/handoffs-of d)))

(defn- own-step [[world ds views] dim d]
  (let [changes (state/changes-of d)
        ds' (update ds dim deltas/merge d)]
    (cond
      (seq changes) (crossing [world ds] dim d changes)
      (deltas/inert? d) [world ds' views]
      :else (let [lv (view world views dim)
                  [w lv'] (state/applied-in world dim lv d)]
              [w ds' {dim lv'}]))))

(defn- step [acc dim d]
  (if (or (nil? d) (identical? deltas/empty-deltas d))
    acc
    (handed (own-step acc dim d) d)))

(defn- fresh [[w ds vs :as acc] world views]
  (if (and (identical? w world) (identical? vs views))
    acc
    [w ds (awake-views w vs)]))

(defn- run-phase [[world ds views :as acc] phase]
  (let [pd (phase-deltas world views ds phase)]
    (if (zero? (count pd))
      acc
      (fresh (reduce (fn [acc dim] (step acc dim (get pd dim)))
                     acc dims)
             world views))))

(defn tick
  "Returns the world and the deltas after one tick of the events.
  A system that fails gives no deltas this tick, the others go on."
  ([world events] (tick world events phases))
  ([world events phases]
   (deltas/in-pool
     #(let [acc (begin world events)
            [world' ds] (reduce run-phase acc phases)]
        [world' (merged ds)]))))
