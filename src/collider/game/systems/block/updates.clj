(ns collider.game.systems.block.updates
  "Scheduled block ticks and their effects."
  (:require [clojure.data.int-map :as i]
            [collider.game.block.blockentity :as be]
            [collider.game.block.tnt :as tnt]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.env.weather :as weather]
            [collider.world.blocks.support :as support]
            [collider.world.blocks.water :as water]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.rules :as rules]))

(set! *warn-on-reflection* true)

(defn- cell-changes [chunks ctx p]
  (rules/cell-changes chunks (chunk/chunks-get-block chunks p)
                      p ctx))

(defn- lww-changes
  "Returns the changes the ticking blocks ask for.
  There is at most one per block and the last change wins."
  [chunks ctx cells]
  (->> (mapcat #(cell-changes chunks ctx %) cells)
       (into (sorted-map) (map (fn [[pos st]] [pos [pos st]])))
       vals
       (into [])))

(defn- loose-scaffold?
  "Returns true when the block is scaffolding with support."
  [^long old]
  (and (= :scaffolding (block/type-of old))
       (not= :7 (:distance (block/props-of old)))))

(defn- falling-entity? [^long old]
  (and (block/falls? old)
       (not (loose-scaffold? old))
       (or (not (dripstone/speleothem? old))
           (dripstone/stalactite? old))))

(defn- washed? [^long old ^long st]
  (and (block/water? st) (not (block/waterlogged? old))))

(defn- unsupported? [^long old ^long st]
  (and (= st (block/emptied old)) (= st (support/gone-state old))
       (not (block/fire? old)) (not (falling-entity? old))))

(defn- destroyed? [^long old ^long st]
  (and (pos? old) (not (block/liquid? old))
       (or (washed? old st) (unsupported? old st))))

(defn- ticking-fires [chunks now]
  (let [fire? #(fire/fire-state? (chunk/chunks-get-block chunks %))]
    (into #{} (filter fire?) now)))

(defn- burnt?
  "Returns true when a fire that ticks now stands next to pos.
  A burnt block drops nothing."
  [fires pos]
  (boolean (some #(contains? fires (mapv + pos %)) dir/around)))

(defn- destroyed [world now changes]
  (let [chunks (:chunks world)
        fires (ticking-fires chunks now)]
    (for [[pos st] changes
          :let [old (chunk/chunks-get-block chunks pos)]
          :when (and (destroyed? old (long st))
                     (not (burnt? fires pos)))]
      [pos old st])))

(defn- break-shown?
  "Returns true when a block that went this way shows its break.
  A washed block only drops; fire never shows one."
  [old st]
  (and (not (washed? old st)) (not (fire/fire-state? old))))

(defn- destroyed-effects [gone]
  (for [[pos old st] gone
        :when (break-shown? old st)]
    (out/all (out/break-effect pos old))))

(defn- drops-of [world pos old]
  (let [salt (fn [salt] (random/of-key (:tick world) pos salt))]
    (map-indexed vector (block/drops old salt))))

(defn- destroyed-drops [world gone]
  (when (get-in world [:rules :block-drops] true)
    (for [[pos old] gone
          [i stack] (drops-of world pos old)]
      [:spawn-entity (items/popped world pos stack i)])))

(defn- fizzed? [^long old ^long st]
  (or (and (block/liquid? old) (pos? st)
           (nil? (block/liquid-class st)))
      (and (liquid/mix-class? st) (pos? old)
           (nil? (block/liquid-class old)))))

(defn- fizz-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) pos)]
        :when (fizzed? old (long st))]
    (out/all (out/fizz pos))))

(defn- falling-block [pos old]
  (let [[x y z] pos]
    {:type :falling-block
     :pos [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
     :vel [0.0 0.0 0.0] :yaw 0.0 :pitch 0.0 :on-ground false
     :block (block/without-water old) :start pos :time 0}))

(defn- fall-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) pos)]
        :when (and (falling-entity? old)
                   (= (long st) (block/emptied old)))]
    [:spawn-entity (falling-block pos old)]))

(def ^:private sponge-plants
  #{:kelp :kelp-plant :seagrass :tall-seagrass})

(defn- sponge-drops [world sponge changed]
  (let [chunks (:chunks world)]
    (for [[pos st] (water/absorbed chunks sponge)
          :let [old (chunk/chunks-get-block chunks pos)]
          :when (and (zero? (long st))
                     (contains? sponge-plants (block/type-of old))
                     (= 0 (long (get changed pos -1))))
          [i stack] (drops-of world pos old)]
      [pos stack i])))

(defn- sponge? [chunks p]
  (= :sponge (block/type-of (chunk/chunks-get-block chunks p))))

(defn- add-drop [world [seen acc] [pos stack i]]
  (if (contains? seen [pos i])
    [seen acc]
    [(conj seen [pos i])
     (conj acc [:spawn-entity (items/popped world pos stack i)])]))

(defn- sponge-deltas [world cells changes]
  (when (get-in world [:rules :block-drops] true)
    (let [chunks (:chunks world)
          changed (into {} changes)
          sponges (filter #(sponge? chunks %) cells)
          drops (mapcat #(sponge-drops world % changed) sponges)]
      (second (reduce #(add-drop world %1 %2) [#{} []] drops)))))

(defn- drip-sound [^long st]
  (if (= :lava-cauldron (block/block-of st))
    out/sound-drip-lava-into-cauldron
    out/sound-drip-water-into-cauldron))

(defn- drip-fill-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) pos)]
        :when (contains? block/cauldron-types (block/type-of old))]
    (out/all (out/level-event (drip-sound (long st)) pos 0))))

(defn- tilted-sound [^long st ^long old]
  (when (and (dripleaf/leaf? st) (dripleaf/leaf? old)
             (not= (dripleaf/tilt-of st) (dripleaf/tilt-of old)))
    (dripleaf/tilt-sound st)))

(defn- tilt-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) pos)
              sound (tilted-sound (long st) old)]
        :when sound]
    (out/all (out/sound sound pos 1.0
                        (random/pitch (:tick world) pos :tilt)))))

(defn- eyeblossom-deltas [changes]
  (for [[pos st] changes
        :when (eyeblossom/eyeblossom? (long st))
        :let [kind (eyeblossom/sound-kind (long st) false)]]
    (out/all (out/sound kind pos 1.0 1.0))))

(defn- cascade-into [world m [pos st]]
  (if-not (eyeblossom/eyeblossom? (long st))
    m
    (let [chunks (:chunks world)
          at (chunk/chunks-get-block chunks pos)
          t (long (:tick world))]
      (merge-with into m (eyeblossom/cascade chunks pos at t)))))

(defn- eyeblossom-schedules [world changes]
  (reduce #(cascade-into world %1 %2) {} changes))

(defn- doused-tnt? [chunks [pos st]]
  (and (not (tnt/tnt-state? (long st)))
       (tnt/tnt-state? (chunk/chunks-get-block chunks pos))))

(defn- burnt-tnt [world changes]
  (let [chunks (:chunks world)
        pending (tnt/primed-origins world)]
    (into (sorted-set)
          (comp (filter #(doused-tnt? chunks %))
                (map first)
                (remove pending))
          changes)))

(defn- ignited [world pos]
  (let [primed (tnt/primed pos [(:tick world) pos])]
    [[:spawn-entity primed]
     (out/all (out/sound :tnt/primed (:pos primed) 1.0 1.0))]))

(defn- ignite-deltas [world changes]
  (mapcat #(ignited world %) (burnt-tnt world changes)))

(defn- due-ticks [world]
  (let [t (long (:tick world))
        due? (fn [[k _]] (<= (long k) t))]
    (into (i/int-set) (comp (take-while due?) (mapcat val))
          (:block-ticks world))))

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

(defn- now-cells [active due]
  (into [] (comp (filter #(state/active-id? active %))
                 (map chunk/id->block-pos))
        due))

(defn- parked-ids
  "Returns the due ids of loaded chunks that do not tick now."
  [world active due]
  (let [chunks (:chunks world)
        loaded? #(contains? chunks (chunk/block-id-chunk %))
        skip? #(or (state/active-id? active %) (not (loaded? %)))]
    (into [] (remove skip?) due)))

(defn- woken-ticks [world now changes]
  (merge-with into (again-schedule world now changes)
              (eyeblossom-schedules world changes)))

(defn- due-deltas [world now parked changes]
  (let [t (long (:tick world))
        woken (woken-ticks world now changes)]
    (concat [[:ticks-flushed t parked]]
            (when (seq woken) [[:schedule-ticks woken]])
            (when (seq changes) (change-deltas world now changes))
            (ignite-deltas world changes))))

(defn- block-updates-deltas [world _events]
  (let [due (due-ticks world)]
    (when (seq due)
      (let [active (state/ticking-chunks world)
            now (now-cells active due)
            parked (parked-ids world active due)
            ctx (tick-ctx world)
            changes (lww-changes (:chunks world) ctx now)]
        (due-deltas world now parked changes)))))

(defn block-updates
  "Runs the block ticks that are due."
  [world d]
  [#(block-updates-deltas world d)])

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
