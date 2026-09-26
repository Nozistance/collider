(ns collider.game.systems.block.updates
  "Scheduled block ticks and their effects."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.out :as out]
            [collider.game.schedule :as schedule]
            [collider.game.state :as state]
            [collider.game.systems.blocks.edit :as edit]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.env.weather :as weather]
            [collider.world.chunk :as chunk]
            [collider.world.rules :as rules]))

(set! *warn-on-reflection* true)

(def ^:private lists
  {:block-wakes {:type-of block/block-of :due rules/reshape-changes}
   :block-ticks {:type-of block/block-of :due rules/cell-changes}
   :fluid-ticks {:type-of liquid/fluid-of :due rules/fluid-changes}})

(defn- cell-changes [chunks ctx k p]
  ((get-in lists [k :due])
   chunks (chunk/chunks-get-block chunks p) p ctx))

(defn- lww-changes
  "Returns the changes the ticking blocks ask for.
  There is at most one per block and the last change wins."
  [chunks ctx k cells]
  (->> cells
       (mapcat #(cell-changes chunks ctx k %))
       (into (sorted-map) (map (fn [[pos :as c]] [pos c])))
       vals
       (into [])))

(defn- player-positions [world]
  (mapv (comp :pos val) (state/player-entries world)))

(defn- tick-ctx [world]
  (merge (select-keys world weather/fields)
         {:rules (:rules world)
          :dim (:dim world)
          :tick (long (:tick world))
          :time-of-day (:time-of-day world 0)
          :players (player-positions world)}))

(defn- again-at [chunks ^long t changed p]
  (when-not (contains? changed p)
    (rules/again-tick chunks (chunk/chunks-get-block chunks p)
                      p t)))

(defn- wake-at [m p at]
  (update m at (fnil conj []) (chunk/block-pos->id p)))

(defn- again-schedule [world now changes]
  (let [chunks (:chunks world)
        t (long (:tick world))
        changed (into #{} (map first) changes)
        at-tick (fn [m p]
                  (if-let [at (again-at chunks t changed p)]
                    (wake-at m p at)
                    m))]
    (reduce at-tick {} now)))

(defn- change-deltas [world records]
  (into [[:set-blocks (edit/block-changes records)]]
        (edit/change-fx world records)))

(defn- still? [world k [id tys]]
  (let [p (chunk/id->block-pos id)
        st (chunk/chunks-get-block (:chunks world) p)]
    (contains? tys ((get-in lists [k :type-of]) st))))

(defn- now-cells [world k active due]
  (into [] (comp (filter #(state/active-id? active (key %)))
                 (filter #(still? world k %))
                 (map (comp chunk/id->block-pos key)))
        due))

(defn- parked-ids
  "Returns the due ids of loaded chunks that do not tick now."
  [world active due]
  (let [chunks (:chunks world)
        loaded? #(contains? chunks (chunk/block-id-chunk %))
        skip? #(or (state/active-id? active %) (not (loaded? %)))]
    (into [] (comp (map key) (remove skip?)) due)))

(defn- due-deltas [world k now parked records]
  (let [t (long (:tick world))
        changes (edit/block-changes records)
        woken (when (= :block-ticks k)
                (again-schedule world now changes))]
    (concat [[:ticks-flushed k t parked]]
            (when (seq woken) [[:schedule-ticks woken]])
            (when (seq changes) (change-deltas world records)))))

(defn- ticks-deltas [world k]
  (let [due (schedule/due (get world k) (:tick world))]
    (when (seq due)
      (let [active (state/ticking-chunks world)
            now (now-cells world k active due)
            parked (parked-ids world active due)
            ctx (tick-ctx world)
            changes (lww-changes (:chunks world) ctx k now)]
        (due-deltas world k now parked changes)))))

(defn neighbor-updates
  "Runs the neighbour updates that are due. They go before the
  block ticks, which then see what the updates did."
  [world _d]
  [#(ticks-deltas world :block-wakes)])

(defn block-updates
  "Runs the block ticks that are due."
  [world _d]
  [#(ticks-deltas world :block-ticks)])

(defn fluid-updates [world _d]
  [#(ticks-deltas world :fluid-ticks)])

(defn- final-records
  "Returns the last state of each block.
  The order is the one in which the blocks first changed."
  [recs]
  (let [end (into {} recs)
        final (fn [pos] [pos (get end pos)])]
    (into [] (comp (map first) (distinct) (map final)) recs)))

(defn- announced
  "Keeps the changes clients hear about.
  A chunk that is only full, never block ticking, is silent."
  [w events]
  (let [heard (state/broadcast-chunks w)]
    (filter (fn [[cp _]] (contains? heard cp)) events)))

(defn- changed-out [[cp recs]]
  (out/all (out/blocks-changed cp (final-records recs))))

(defn- entity-outs [w events]
  (for [[_ recs] events
        [pos _] recs
        :when (be/at w pos)]
    (out/all (out/block-entity pos))))

(defn block-flush
  "Tells the clients about the blocks that changed this tick."
  [w _d]
  (when-let [events (:block-events w)]
    (concat [[:block-events-flushed]]
            (map changed-out (announced w events))
            (entity-outs w events))))
