(ns collider.game.tick
  "The tick: its phases and how they run."
  (:require [collider.game.apply :as apply]
            [collider.game.level :as level]
            [collider.game.schema :as schema]
            [collider.game.cost :as cost]
            [collider.game.deltas :as deltas]
            [collider.game.detector :as detector]
            [collider.log :as log]
            [collider.game.systems.attacks :as attacks]
            [collider.game.systems.block.entities :as block-entities]
            [collider.game.systems.block.events :as block-events]
            [collider.game.systems.block.updates :as block-updates]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.camera :as camera]
            [collider.game.systems.chat :as chat]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.daynight :as daynight]
            [collider.game.systems.entities :as entities]
            [collider.game.systems.hanging :as hanging]
            [collider.game.systems.inventory :as inventory]
            [collider.game.systems.natural :as natural]
            [collider.game.systems.packets :as packets]
            [collider.game.systems.players :as players]
            [collider.game.systems.players.tick :as player-tick]
            [collider.game.systems.random.tick :as random-tick]
            [collider.game.systems.sleep :as sleep]
            [collider.game.systems.spawning :as spawning]
            [collider.game.systems.weather :as weather-system]
            [collider.game.systems.damage :as damage]))

(set! *warn-on-reflection* true)

(def packet-systems
  "The systems a player event drives.
  They all run before the level tick."
  [#'players/player-list
   #'camera/camera
   #'blocks/block-edits
   #'packets/by-player
   #'inventory/inventory
   #'containers/containers
   #'chat/chat])

(def entity-systems
  "The systems that step the entities of the level."
  [#'entities/entities
   #'damage/burning
   #'hanging/hanging-checks
   #'damage/damage])

(def systems
  "Every system the level tick drives off the world it is given."
  (into packet-systems entity-systems))

(def phases [[#'spawning/placing]
             [#'chunks/chunk-loading]
             packet-systems
             [#'hanging/hanging-uses #'attacks/attacks]
             [#'daynight/daynight #'weather-system/weather]
             [#'sleep/sleep]
             [#'block-updates/block-updates]
             [#'block-updates/fluid-updates]
             [#'chunks/unloading
              #'natural/natural-spawns
              #'random-tick/random-ticks
              #'chunks/chunk-views]
             [#'block-updates/block-flush #'players/players]
             [#'block-events/block-events]
             entity-systems
             [#'block-entities/block-entities]
             [#'player-tick/player-tick]
             [#'players/late-tracking]
             [#'chunks/chunk-streaming]
             [#'detector/observe]])

(def dims
  "The dimensions the tick runs, in a fixed order."
  schema/dims)

(def ^:private home (first dims))

(defn- event-dim [world [tag x]]
  (case tag
    :player-join home
    :chunk-loaded x
    (or (level/dim-of world x) home)))

(defn- input-of [dim events]
  (if (= home dim)
    (deltas/input events)
    (assoc deltas/empty-deltas :input (vec events))))

(defn- awaited? [world dim]
  (let [spawning (:spawning world)]
    (and (not (deltas/vacant? spawning))
         (some #(= dim (:dim (val %))) spawning))))

(defn- asleep? [world dim]
  (and (not (identical? home dim))
       (level/idle? (get (:levels world) dim))
       (not (awaited? world dim))))

(defn- begin [world events]
  (let [world (level/synced world)
        by (group-by #(event-dim world %) events)
        ds (into {} (map (fn [dim] [dim (input-of dim (by dim))]))
                 dims)
        w (reduce #(apply/entered %1 %2 (ds %2)) world dims)
        heeded (fn [[dim d]] [dim (apply/heeded w dim d)])]
    [w (into {} (map heeded) ds)]))

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

(defn- merged [ds] (reduce deltas/merge (map ds dims)))

(defn- thunk [world lv d ds dim s]
  (if (:once (meta s))
    (let [f (guarded world s nil
                     #(s (level/server-view world) (merged ds)))]
      #(deltas/with-dim (f) nil))
    (guarded world s dim #(s lv d))))

(defn- runs-in? [dim s]
  (or (identical? home dim) (not (:once (meta s)))))

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
          (and types (level/holds-types? lv types))
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

(defn- level-deltas [world ds phase dim]
  (let [lv (get (:levels world) dim)
        d (get ds dim)
        ks (awake-in lv d dim phase)]
    (when (pos? (count ks))
      (let [lv (assoc lv :server world)
            fs (mapv #(thunk world lv d ds dim %) ks)
            timed (cost/timer (:tick world))]
        (deltas/with-dim
         (deltas/run-weighed cost/heavy? timed ks fs)
         dim)))))

(defn- phase-deltas [world ds phase]
  (reduce (fn [pd dim]
            (if-let [d (when-not (asleep? world dim)
                         (level-deltas world ds phase dim))]
              (assoc pd dim d)
              pd))
          {} dims))

(defn- unfiltered! [world f ^Throwable t]
  (let [unit (log/name-of f)
        msg (str "filter " unit " failed, passed over this time")]
    (log/failure! unit msg t)
    (some-> (:failures world) (swap! conj [unit t]))))

(defn- passed [world fs at x]
  (reduce (fn [x f]
            (try (vec (f at x))
                 (catch Throwable t (unfiltered! world f t) x)))
          x fs))

(defn- filtered [world fs pd]
  (reduce-kv
    (fn [m dim d]
      (let [lv (assoc (get (:levels world) dim) :server world)
            v (passed world fs lv (deltas/as-vec d))]
        (assoc m dim (deltas/with-dim (deltas/of-vec v) dim))))
    pd pd))

(defn- step [[world ds] dim d]
  (apply/in world dim d ds))

(defn- hooked [world k]
  (seq (get-in world [:hooks k])))

(defn- run-phase [[world ds :as acc] phase]
  (let [pd (phase-deltas world ds phase)
        pd (if-let [fs (hooked world :delta-filters)]
             (filtered world fs pd)
             pd)]
    (reduce #(step %1 %2 (get pd %2)) acc dims)))

(defn tick
  "Returns the world and the deltas after one tick of the events.
  A system that fails gives no deltas this tick, the others go on.
  The event filters of world under [:hooks :event-filters] pass
  the events in turn before the tick, each as (f world events). Its
  delta filters under [:hooks :delta-filters] pass the deltas of
  each phase in turn before they apply, each as (f level deltas). A
  filter that fails passes on what it was given."
  ([world events] (tick world events phases))
  ([world events phases]
   (deltas/in-pool
     #(let [evs (if-let [fs (hooked world :event-filters)]
                  (passed world fs world events)
                  events)
            acc (begin world evs)
            [world' ds] (reduce run-phase acc phases)]
        [world' (merged ds)]))))
