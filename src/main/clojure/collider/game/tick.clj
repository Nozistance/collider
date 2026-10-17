(ns collider.game.tick
  "The tick, its phases and how they run."
  (:require [collider.data.long-map :as lm]
            [collider.game.apply :as apply]
            [collider.game.cost :as cost]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.detector :as detector]
            [collider.game.level :as level]
            [collider.game.schema :as schema]
            [collider.game.systems.block.entities :as block-entities]
            [collider.game.systems.block.triggers :as triggers]
            [collider.game.systems.block.updates :as block-updates]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.damage :as damage]
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
            [collider.game.systems.tracker :as tracker]
            [collider.game.systems.waypoints :as waypoints]
            [collider.game.systems.weather :as weather-system]
            [collider.parallel :as par]))

(set! *warn-on-reflection* true)

(def packet-systems
  "The systems a player event drives.
  They all run before the level tick."
  [#'players/player-list
   #'containers/containers
   #'packets/packets
   #'inventory/inventory])

(def entity-systems
  "The systems that step the entities of the level."
  [#'entities/entities
   #'hanging/hanging-checks
   #'damage/damage])

(def systems
  "Every system the level tick drives off the world it is given."
  (into packet-systems entity-systems))

(def phases
  "The phases of the tick in the order they run. The systems of one
  phase see the same world."
  [[#'spawning/placing]
   [#'chunks/chunk-loading]
   packet-systems
   [#'daynight/daynight #'weather-system/weather]
   [#'sleep/sleep]
   [#'block-updates/block-updates]
   [#'block-updates/fluid-updates]
   [#'chunks/unloading
    #'natural/natural-spawns
    #'random-tick/random-ticks
    #'chunks/chunk-views]
   [#'block-updates/block-flush #'tracker/tracker]
   [#'triggers/block-events]
   entity-systems
   [#'block-entities/block-entities]
   [#'player-tick/player-tick]
   [#'waypoints/waypoints]
   [#'tracker/late-tracking]
   [#'chunks/chunk-streaming]
   [#'players/tab-updates]
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
    (and (not (lm/empty? spawning))
         (some #(let [req (val %)]
                  (or (= dim (:dim req)) (contains? (:need req) dim)))
               spawning))))

(defn- asleep? [world dim]
  (and (not (identical? home dim))
       (level/idle? (get (:levels world) dim))
       (not (awaited? world dim))))

(defn- arrived [events]
  (into [] (map-indexed #(vary-meta %2 assoc :at %1)) events))

(defn- begin [world events]
  (let [world (level/synced world)
        by (group-by #(event-dim world %) (arrived events))
        ds (into {} (map (fn [dim] [dim (input-of dim (by dim))]))
                 dims)
        w (reduce #(apply/entered %1 %2 (ds %2)) world dims)
        heeded (fn [[dim d]] [dim (apply/heeded w dim d)])]
    [w (into {} (map heeded) ds)]))

(defn- merged [ds] (reduce deltas/merge (map ds dims)))

(defn- thunk [world lv d ds s]
  (let [m (meta s)]
    (cond (:whole m) #(deltas/with-dim (s world ds) nil)
          (:once m) #(deltas/with-dim
                       (s (level/server-view world) (merged ds)) nil)
          :else #(s lv d))))

(defn- runs-in? [dim s]
  (or (identical? home dim) (not (:once (meta s)))))

(defn- held? [v]
  (if (coll? v) (boolean (seq v)) (boolean v)))

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
            fs (mapv #(thunk world lv d ds %) ks)
            timed (cost/timer (:tick world))]
        (deltas/with-dim
         (deltas/run-weighed cost/heavy? timed ks fs)
         dim)))))

(defn- awake-deltas [world ds phase dim]
  (when-not (asleep? world dim)
    (level-deltas world ds phase dim)))

(defn- phase-deltas [world ds phase]
  (reduce (fn [pd dim]
            (if-let [d (awake-deltas world ds phase dim)]
              (assoc pd dim d)
              pd))
          {} dims))

(defn- checked [ok? v]
  (if (every? ok? v)
    v
    (throw (ex-info "the filter gave a bad delta"
                    {:delta (first (remove ok? v))}))))

(defn- passed [fs at x ok?]
  (reduce (fn [x f] (checked ok? (vec (f at x)))) x fs))

(defn- tagged-known? [plugin-tags tag x]
  (cond (identical? :fx tag) (map? x)
        (contains? delta/entity-apply tag) (integer? x)
        :else (or (contains? delta/registry tag)
                  (contains? plugin-tags tag))))

(defn- known? [plugin-tags d]
  (and (vector? d) (pos? (count d))
       (tagged-known? plugin-tags (nth d 0) (nth d 1 nil))))

(defn- passed-in [world fs dim v]
  (let [lv (assoc (get (:levels world) dim) :server world)
        ok? (partial known? (:deltas (:hooks world)))
        handed #(if (identical? :level-deltas (nth % 0))
                  [:level-deltas (nth % 1)
                   (passed-in world fs (nth % 1) (nth % 2))]
                  %)]
    (passed fs lv (mapv handed v) ok?)))

(defn- filtered [world fs pd]
  (reduce-kv
    (fn [m dim d]
      (let [v (passed-in world fs dim (deltas/as-vec d))]
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

(def ^:private before-level
  (into #{#'spawning/placing #'chunks/chunk-loading} packet-systems))

(defn- early? [phase] (some before-level phase))

(defn open
  "Returns the world and the deltas of each level as a tick opens,
  before any of its events."
  [world]
  (begin world []))

(defn- batch-of [world events]
  (let [by (group-by #(event-dim world %) (arrived events))]
    (into {} (for [dim dims :let [evs (by dim)] :when (seq evs)]
               [dim (assoc deltas/empty-deltas :input (vec evs))]))))

(defn- entered-batch [world bd]
  (reduce-kv
    (fn [[w ds] dim d]
      (let [n (count (get-in w [:levels dim :input :heeded]))
            w (apply/added w dim d)]
        [w (assoc ds dim (apply/heeded-since w dim d n))]))
    [world {}] bd))

(defn- filtered-events [world events]
  (if-let [fs (hooked world :event-filters)]
    (passed fs world events vector?)
    events))

(defn early
  "Returns the world and the deltas of each level of tick acc after a
  batch of its events, and the deltas of the batch alone. The batch
  runs the phases before the level tick, as a server runs the tasks
  between two ticks. Phases are those of the tick by default."
  ([acc events] (early acc events phases))
  ([[world ds] events phases]
   (let [world (apply/batch-opened world)
         evs (filtered-events world events)
         [w bd] (entered-batch world (batch-of world evs))
         bd (zipmap dims (map #(get bd % deltas/empty-deltas) dims))
         [w bd] (reduce run-phase [w bd] (filter early? phases))]
     [[w (merge-with deltas/merge ds bd)] (merged bd)])))

(defn late
  "Returns the world, the deltas of the whole tick acc and those of
  its phases from the level tick on, after those phases."
  ([acc] (late acc phases))
  ([[_ before :as acc] phases]
   (let [[world' ds] (reduce run-phase acc (remove early? phases))
         part (fn [dim]
                (deltas/since (get before dim deltas/empty-deltas)
                              (get ds dim deltas/empty-deltas)))]
     [world' (merged ds)
      (merged (into {} (map (juxt identity part)) dims))])))

(defn tick
  "Returns the world and the deltas after one tick of the events.
  The event filters of the world pass the events before the tick,
  its delta filters the deltas of each phase before they apply."
  ([world events] (tick world events phases))
  ([world events phases]
   (par/in-pool
     #(let [[acc] (early (open world) events phases)]
        (pop (late acc phases))))))
