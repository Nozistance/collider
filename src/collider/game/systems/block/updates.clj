(ns collider.game.systems.block.updates
  "Scheduled block ticks and what they change."
  (:require [clojure.data.int-map :as i]
            [collider.game.block.blockentity :as be]
            [collider.game.state :as state]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.game.block.tnt :as tnt]
            [collider.game.out :as out]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.blocks.water :as water]
            [collider.world.gen :as gen]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.support :as support]
            [collider.world.rules :as rules]))

(set! *warn-on-reflection* true)

(defn- lww-changes
  "Returns the changes the given blocks ask for as they tick, at most one
   change per block."
  [chunks ctx cells]
  (into []
        (vals (into (sorted-map)
                    (map (fn [[pos st]] [pos [pos st]]))
                    (mapcat (fn [p] (rules/cell-changes
                                      chunks
                                      (chunk/chunks-get-block chunks (gen/flat-chunk) p)
                                      p ctx))
                            cells)))))

(defn- loose-scaffold?
  "Returns true when the block is scaffolding that still reaches a
   support."
  [^long old]
  (and (= :scaffolding (block/type-of old)) (not= :7 (:distance (block/props-of old)))))

(defn- falling-entity?
  "Returns true when the block falls once nothing holds it up."
  [^long old]
  (and (block/falls? old) (not (loose-scaffold? old))
       (or (not (dripstone/speleothem? old)) (dripstone/stalactite? old))))

(defn- washed?
  "Returns true when a change puts water where the block was."
  [^long old ^long st]
  (and (= :water (liquid/liquid-class st)) (not (block/waterlogged? old))))

(defn- unsupported?
  "Returns true when a change takes the block away for want of support."
  [^long old ^long st]
  (and (= st (block/emptied old)) (= st (support/gone-state old))
       (not (block/fire? old)) (not (falling-entity? old))))

(defn- destroyed?
  "Returns true when a change destroys the block that was there."
  [^long old ^long st]
  (and (pos? old) (not (liquid/liquid-state? old))
       (or (washed? old st) (unsupported? old st))))

(defn- ticking-fires
  "Returns the positions of the fires among the blocks that tick now."
  [chunks now]
  (into #{} (filter #(fire/fire-state? (chunk/chunks-get-block chunks (gen/flat-chunk) %))) now))

(defn- burnt?
  "Returns true when a fire that ticks now stands next to pos, so the block
   burns away rather than breaks: FireBlock.checkBurnOut drops nothing."
  [fires pos]
  (boolean (some #(contains? fires (mapv + pos %)) dir/around)))

(defn- destroyed
  "Returns the blocks the changes destroy, as they were."
  [world now changes]
  (let [fires (ticking-fires (:chunks world) now)]
    (for [[pos st] changes
          :let [old (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos)]
          :when (and (destroyed? old (long st)) (not (burnt? fires pos)))]
      [pos old])))

(defn- destroyed-effects
  "Returns the breaking effects for the blocks that were destroyed."
  [gone]
  (for [[pos old] gone]
    (out/all (out/break-effect pos old))))

(defn- destroyed-drops
  "Returns the deltas dropping what the destroyed blocks leave."
  [world gone]
  (when (get-in world [:rules :block-drops] true)
    (for [[pos old] gone
          [i stack] (map-indexed vector (block/drops old (fn [salt] (random/of-key (:tick world) pos salt))))]
      [:spawn-entity (items/popped world pos stack i)])))

(defn- fizz-deltas
  "Returns the effects for water and lava meeting."
  [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos)]
        :when (or (and (liquid/liquid-state? old) (pos? (long st)) (nil? (liquid/liquid-class st)))
                  (and (liquid/mix-class? st) (pos? (long old)) (nil? (liquid/liquid-class old))))
        d [(out/all (out/fizz pos))]]
    d))

(defn- fall-deltas
  "Returns the deltas starting the blocks that begin to fall."
  [world changes]
  (for [[[x y z :as pos] st] changes
        :let [old (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos)]
        :when (and (falling-entity? old) (= (long st) (block/emptied old)))]
    [:spawn-entity {:type  :falling-block
                    :pos   [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
                    :vel   [0.0 0.0 0.0] :yaw 0.0 :pitch 0.0 :on-ground false
                    :block (block/without-water old) :start pos :time 0}]))

(def ^:private sponge-plants #{:kelp :kelp-plant :seagrass :tall-seagrass})
(defn- sponge-drops
  "Returns what the plants a sponge dries out leave behind."
  [world sponge changed]
  (let [chunks (:chunks world)]
    (for [[pos st] (water/absorbed chunks sponge)
          :let [old (chunk/chunks-get-block chunks (gen/flat-chunk) pos)]
          :when (and (zero? (long st)) (contains? sponge-plants (block/type-of old))
                     (= 0 (long (get changed pos -1))))
          [i stack] (map-indexed vector (block/drops old (fn [salt] (random/of-key (:tick world) pos salt))))]
      [pos stack i])))

(defn- sponge-deltas
  "Returns the deltas dropping what the plants a sponge dries out leave."
  [world cells changes]
  (when (get-in world [:rules :block-drops] true)
    (let [chunks (:chunks world)
          changed (into {} changes)
          sponges (filter #(= :sponge (block/type-of (chunk/chunks-get-block chunks (gen/flat-chunk) %))) cells)]
      (->> (mapcat #(sponge-drops world % changed) sponges)
           (reduce (fn [[seen acc] [pos stack i]]
                     (if (contains? seen [pos i])
                       [seen acc]
                       [(conj seen [pos i]) (conj acc [:spawn-entity (items/popped world pos stack i)])]))
                   [#{} []])
           second))))

(defn- drip-fill-deltas
  "Returns the effects for a cauldron caught by a drip."
  [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos)]
        :when (contains? block/cauldron-types (block/type-of old))]
    (out/all (out/level-event (if (= :lava-cauldron (block/block-of (long st))) out/sound-drip-lava-into-cauldron out/sound-drip-water-into-cauldron) pos 0))))

(defn- tilt-deltas
  "Returns the sounds for big dripleaves tipping."
  [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos)
              sound (when (and (dripleaf/leaf? (long st)) (dripleaf/leaf? old)
                               (not= (dripleaf/tilt-of (long st)) (dripleaf/tilt-of old)))
                      (dripleaf/tilt-sound (long st)))]
        :when sound]
    (out/all (out/sound sound pos 1.0 (random/pitch (:tick world) pos :tilt)))))

(defn- eyeblossom-deltas
  "Returns the sounds for eyeblossoms opening and closing."
  [changes]
  (for [[pos st] changes :when (eyeblossom/eyeblossom? (long st))]
    (out/all (out/sound (eyeblossom/sound-kind (long st) false) pos 1.0 1.0))))

(defn- eyeblossom-schedules
  "Returns the ticks the eyeblossoms around the changes ask for."
  [world changes]
  (let [chunks (:chunks world) t (long (:tick world))]
    (reduce (fn [m [pos st]]
              (if-not (eyeblossom/eyeblossom? (long st))
                m
                (merge-with into m (eyeblossom/cascade chunks pos (chunk/chunks-get-block chunks (gen/flat-chunk) pos) t))))
            {} changes)))

(defn- burnt-tnt
  "Returns the TNT the fire took away this tick."
  [world changes]
  (let [chunks (:chunks world)
        pending (tnt/primed-origins world)]
    (into (sorted-set)
          (comp (filter (fn [[pos st]]
                          (and (not (tnt/tnt-state? (long st)))
                               (tnt/tnt-state? (chunk/chunks-get-block chunks (gen/flat-chunk) pos)))))
                (map first)
                (remove pending))
          changes)))

(defn- ignite-deltas
  "Returns the deltas for TNT that the fire set off."
  [world changes]
  (mapcat (fn [pos]
            (let [primed (tnt/primed pos [(:tick world) pos])]
              [[:spawn-entity primed]
               (out/all (out/sound :tnt/primed (:pos primed) 1.0 1.0))]))
          (burnt-tnt world changes)))

(defn- due-ticks
  "Returns the blocks whose tick has come."
  [world]
  (let [t (long (:tick world))]
    (into (i/int-set) (comp (take-while (fn [[k _]] (<= (long k) t))) (mapcat val))
          (:block-ticks world))))

(defn- tick-ctx
  "Returns what a ticking block may ask about the world."
  [world]
  {:rules   (:rules world) :tick (long (:tick world)) :time-of-day (:time-of-day world 0)
   :players (mapv (comp :pos val) (state/player-entries world))})

(defn- again-schedule
  "Returns the ticks asked for again by blocks that did nothing."
  [world now changes]
  (let [chunks (:chunks world) t (long (:tick world))
        changed (into #{} (map first) changes)]
    (reduce (fn [m p]
              (if-let [at (and (not (contains? changed p))
                               (rules/again-tick chunks (chunk/chunks-get-block chunks (gen/flat-chunk) p) p t))]
                (update m at (fnil conj []) (chunk/block-pos->id p))
                m))
            {} now)))

(defn- change-deltas
  "Returns the deltas for this tick's block changes and all they set off."
  [world now changes]
  (let [gone (destroyed world now changes)]
    (concat [[:set-blocks changes]]
            (fizz-deltas world changes)
            (destroyed-effects gone)
            (destroyed-drops world gone)
            (sponge-deltas world now changes)
            (fall-deltas world changes)
            (eyeblossom-deltas changes)
            (tilt-deltas world changes)
            (drip-fill-deltas world changes))))

(defn- block-updates-deltas
  "Returns the deltas for the blocks whose tick has come."
  [world _events]
  (let [t (long (:tick world))
        due (due-ticks world)]
    (when (seq due)
      (let [active (state/active-chunks world)
            now (into [] (comp (filter #(state/active-id? active %)) (map chunk/id->block-pos)) due)
            parked (into [] (remove #(state/active-id? active %)) due)
            changes (lww-changes (:chunks world) (tick-ctx world) now)
            woken (merge-with into (again-schedule world now changes)
                              (eyeblossom-schedules world changes))]
        (concat [[:ticks-flushed t parked]]
                (when (seq woken) [[:schedule-ticks woken]])
                (when (seq changes) (change-deltas world now changes))
                (ignite-deltas world changes))))))

(defn block-updates
  "Returns the deltas for the blocks whose tick has come."
  [world d]
  [#(block-updates-deltas world d)])

(defn- final-records
  "Returns the state each block settled at, in the order they first
   changed."
  [recs]
  (let [last (into {} recs)]
    (into [] (comp (map first) (distinct) (map (fn [pos] [pos (get last pos)]))) recs)))

(defn block-flush
  "Returns the effects telling players about the blocks that changed."
  [w _d]
  (when-let [events (:block-events w)]
    (concat [[:block-events-flushed]]
            (map (fn [[cp recs]] (out/all (out/blocks-changed cp (final-records recs)))) events)
            (for [[_ recs] events [pos _] recs :when (be/at w pos)]
              (out/all (out/block-entity pos))))))
