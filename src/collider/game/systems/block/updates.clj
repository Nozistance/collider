(ns collider.game.systems.block.updates
  "Scheduled block ticks and their effects."
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
  "Returns the changes the ticking blocks ask for, at most one per block. The
   last change wins."
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
  "Returns true when the block is scaffolding that still has support."
  [^long old]
  (and (= :scaffolding (block/type-of old)) (not= :7 (:distance (block/props-of old)))))

(defn- falling-entity? [^long old]
  (and (block/falls? old) (not (loose-scaffold? old))
       (or (not (dripstone/speleothem? old)) (dripstone/stalactite? old))))

(defn- washed? [^long old ^long st]
  (and (= :water (liquid/liquid-class st)) (not (block/waterlogged? old))))

(defn- unsupported? [^long old ^long st]
  (and (= st (block/emptied old)) (= st (support/gone-state old))
       (not (block/fire? old)) (not (falling-entity? old))))

(defn- destroyed? [^long old ^long st]
  (and (pos? old) (not (liquid/liquid-state? old))
       (or (washed? old st) (unsupported? old st))))

(defn- ticking-fires [chunks now]
  (into #{} (filter #(fire/fire-state? (chunk/chunks-get-block chunks (gen/flat-chunk) %))) now))

(defn- burnt?
  "Returns true when a fire that ticks now stands next to pos. A burnt block drops
   nothing."
  [fires pos]
  (boolean (some #(contains? fires (mapv + pos %)) dir/around)))

(defn- destroyed [world now changes]
  (let [fires (ticking-fires (:chunks world) now)]
    (for [[pos st] changes
          :let [old (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos)]
          :when (and (destroyed? old (long st)) (not (burnt? fires pos)))]
      [pos old])))

(defn- destroyed-effects [gone]
  (for [[pos old] gone]
    (out/all (out/break-effect pos old))))

(defn- destroyed-drops [world gone]
  (when (get-in world [:rules :block-drops] true)
    (for [[pos old] gone
          [i stack] (map-indexed vector (block/drops old (fn [salt] (random/of-key (:tick world) pos salt))))]
      [:spawn-entity (items/popped world pos stack i)])))

(defn- fizz-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos)]
        :when (or (and (liquid/liquid-state? old) (pos? (long st)) (nil? (liquid/liquid-class st)))
                  (and (liquid/mix-class? st) (pos? (long old)) (nil? (liquid/liquid-class old))))
        d [(out/all (out/fizz pos))]]
    d))

(defn- fall-deltas [world changes]
  (for [[[x y z :as pos] st] changes
        :let [old (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos)]
        :when (and (falling-entity? old) (= (long st) (block/emptied old)))]
    [:spawn-entity {:type  :falling-block
                    :pos   [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
                    :vel   [0.0 0.0 0.0] :yaw 0.0 :pitch 0.0 :on-ground false
                    :block (block/without-water old) :start pos :time 0}]))

(def ^:private sponge-plants #{:kelp :kelp-plant :seagrass :tall-seagrass})
(defn- sponge-drops [world sponge changed]
  (let [chunks (:chunks world)]
    (for [[pos st] (water/absorbed chunks sponge)
          :let [old (chunk/chunks-get-block chunks (gen/flat-chunk) pos)]
          :when (and (zero? (long st)) (contains? sponge-plants (block/type-of old))
                     (= 0 (long (get changed pos -1))))
          [i stack] (map-indexed vector (block/drops old (fn [salt] (random/of-key (:tick world) pos salt))))]
      [pos stack i])))

(defn- sponge-deltas [world cells changes]
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

(defn- drip-fill-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos)]
        :when (contains? block/cauldron-types (block/type-of old))]
    (out/all (out/level-event (if (= :lava-cauldron (block/block-of (long st))) out/sound-drip-lava-into-cauldron out/sound-drip-water-into-cauldron) pos 0))))

(defn- tilt-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos)
              sound (when (and (dripleaf/leaf? (long st)) (dripleaf/leaf? old)
                               (not= (dripleaf/tilt-of (long st)) (dripleaf/tilt-of old)))
                      (dripleaf/tilt-sound (long st)))]
        :when sound]
    (out/all (out/sound sound pos 1.0 (random/pitch (:tick world) pos :tilt)))))

(defn- eyeblossom-deltas [changes]
  (for [[pos st] changes :when (eyeblossom/eyeblossom? (long st))]
    (out/all (out/sound (eyeblossom/sound-kind (long st) false) pos 1.0 1.0))))

(defn- eyeblossom-schedules [world changes]
  (let [chunks (:chunks world) t (long (:tick world))]
    (reduce (fn [m [pos st]]
              (if-not (eyeblossom/eyeblossom? (long st))
                m
                (merge-with into m (eyeblossom/cascade chunks pos (chunk/chunks-get-block chunks (gen/flat-chunk) pos) t))))
            {} changes)))

(defn- burnt-tnt [world changes]
  (let [chunks (:chunks world)
        pending (tnt/primed-origins world)]
    (into (sorted-set)
          (comp (filter (fn [[pos st]]
                          (and (not (tnt/tnt-state? (long st)))
                               (tnt/tnt-state? (chunk/chunks-get-block chunks (gen/flat-chunk) pos)))))
                (map first)
                (remove pending))
          changes)))

(defn- ignite-deltas [world changes]
  (mapcat (fn [pos]
            (let [primed (tnt/primed pos [(:tick world) pos])]
              [[:spawn-entity primed]
               (out/all (out/sound :tnt/primed (:pos primed) 1.0 1.0))]))
          (burnt-tnt world changes)))

(defn- due-ticks [world]
  (let [t (long (:tick world))]
    (into (i/int-set) (comp (take-while (fn [[k _]] (<= (long k) t))) (mapcat val))
          (:block-ticks world))))

(defn- tick-ctx [world]
  {:rules   (:rules world) :tick (long (:tick world)) :time-of-day (:time-of-day world 0)
   :players (mapv (comp :pos val) (state/player-entries world))})

(defn- again-schedule [world now changes]
  (let [chunks (:chunks world) t (long (:tick world))
        changed (into #{} (map first) changes)]
    (reduce (fn [m p]
              (if-let [at (and (not (contains? changed p))
                               (rules/again-tick chunks (chunk/chunks-get-block chunks (gen/flat-chunk) p) p t))]
                (update m at (fnil conj []) (chunk/block-pos->id p))
                m))
            {} now)))

(defn- change-deltas [world now changes]
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

(defn- block-updates-deltas [world _events]
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

(defn block-updates [world d]
  [#(block-updates-deltas world d)])

(defn- final-records
  "Returns the last state of each block, in the order the blocks first changed."
  [recs]
  (let [last (into {} recs)]
    (into [] (comp (map first) (distinct) (map (fn [pos] [pos (get last pos)]))) recs)))

(defn block-flush [w _d]
  (when-let [events (:block-events w)]
    (concat [[:block-events-flushed]]
            (map (fn [[cp recs]] (out/all (out/blocks-changed cp (final-records recs)))) events)
            (for [[_ recs] events [pos _] recs :when (be/at w pos)]
              (out/all (out/block-entity pos))))))
