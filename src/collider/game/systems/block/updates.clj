(ns collider.game.systems.block.updates
  (:require [clojure.data.int-map :as i]
            [collider.game.state :as state]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.game.block.tnt :as tnt]
            [collider.game.out :as out]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.blocks.sponge :as sponge]
            [collider.world.gen :as gen]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.rules :as rules]))

(set! *warn-on-reflection* true)

(defn- tnt-neighbors [chunks [x y z]]
  (filterv (fn [[_ ny _ :as p]]
             (and (chunk/in-range? ny)
                  (tnt/tnt-state? (chunk/chunks-get-block chunks gen/flat-chunk p))))
           (map (fn [d] (mapv + [x y z] d)) dir/around)))

(defn- lww-changes [chunks ctx cells]
  (into []
        (vals (into (sorted-map)
                    (map (fn [[pos st]] [pos [pos st]]))
                    (mapcat (fn [p] (rules/cell-changes
                                      chunks
                                      (chunk/chunks-get-block chunks gen/flat-chunk p)
                                      p ctx))
                                    cells)))))

(defn- wash-deltas [world changes]
  (when (get-in world [:rules :block-drops] true)
    (for [[pos st] changes
          :let [old (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos)]
          :when (and (= :water (liquid/liquid-class st))
                     (pos? (long old))
                     (nil? (liquid/liquid-class old))
                     (not (block/waterlogged? old)))
          [i stack] (map-indexed vector (block/drops old (fn [salt] (random/of-key [(:tick world) pos salt]))))]
      [:spawn-entity (items/popped world pos stack i)])))

(defn- fizz-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos)]
        :when (or (and (liquid/liquid-state? old) (pos? (long st)) (nil? (liquid/liquid-class st)))
                  (and (liquid/mix-class? st) (pos? (long old)) (nil? (liquid/liquid-class old))))
        d [(out/all (out/fizz pos))]]
    d))

(defn- loose-scaffold? [old]
  (and (= :scaffolding (block/type-of old)) (not= :7 (:distance (block/props-of old)))))

(defn- fall-deltas [world changes]
  (for [[[x y z :as pos] st] changes
        :let [old (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos)]
        :when (and (block/falls? old) (= (long st) (block/emptied old)))]
    (if (loose-scaffold? old)
      [:spawn-entity (items/popped world pos {:item :scaffolding :count 1} :loose)]
      [:spawn-entity {:type :falling-block
                      :pos [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
                      :vel [0.0 0.0 0.0] :yaw 0.0 :pitch 0.0 :on-ground false
                      :block (block/without-water old) :start pos :time 0}])))

(def ^:private sponge-plants #{:kelp :kelp-plant :seagrass :tall-seagrass})
(defn- sponge-drops [world sponge changed]
  (let [chunks (:chunks world)]
    (for [[pos st] (sponge/absorbed chunks sponge)
          :let [old (chunk/chunks-get-block chunks gen/flat-chunk pos)]
          :when (and (zero? (long st)) (contains? sponge-plants (block/type-of old))
                     (= 0 (long (get changed pos -1))))
          [i stack] (map-indexed vector (block/drops old (fn [salt] (random/of-key [(:tick world) pos salt]))))]
      [pos stack i])))

(defn- sponge-deltas [world cells changes]
  (when (get-in world [:rules :block-drops] true)
    (let [chunks  (:chunks world)
          changed (into {} changes)
          sponges (filter #(= :sponge (block/type-of (chunk/chunks-get-block chunks gen/flat-chunk %))) cells)]
      (->> (mapcat #(sponge-drops world % changed) sponges)
           (reduce (fn [[seen acc] [pos stack i]]
                     (if (contains? seen [pos i])
                       [seen acc]
                       [(conj seen [pos i]) (conj acc [:spawn-entity (items/popped world pos stack i)])]))
                   [#{} []])
           second))))

(defn- drip-fill-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos)]
        :when (contains? block/cauldron-types (block/type-of old))]
    (out/all (out/level-event (if (= :lava-cauldron (block/block-of (long st))) 1046 1047) pos 0))))

(defn- tilt-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos)
              sound (when (and (dripleaf/leaf? (long st)) (dripleaf/leaf? old)
                               (not= (dripleaf/tilt-of (long st)) (dripleaf/tilt-of old)))
                      (dripleaf/tilt-sound (long st)))]
        :when sound]
    (out/all (out/sound sound pos 1.0 (+ 0.8 (* 0.4 (double (random/of-key [(:tick world) pos :tilt]))))))))

(defn- eyeblossom-deltas [changes]
  (for [[pos st] changes :when (eyeblossom/eyeblossom? (long st))]
    (out/all (out/sound (eyeblossom/sound-kind (long st) false) pos 1.0 1.0))))

(defn- eyeblossom-schedules [world changes]
  (let [chunks (:chunks world) t (long (:tick world))]
    (reduce (fn [m [pos st]]
              (if-not (eyeblossom/eyeblossom? (long st))
                m
                (merge-with into m (eyeblossom/cascade chunks pos (chunk/chunks-get-block chunks gen/flat-chunk pos) t))))
            {} changes)))

(defn- ignite-deltas [world due]
  (let [chunks   (:chunks world)
        pending  (tnt/primed-origins world)
        tnts     (into (sorted-set)
                       (comp (filter #(fire/fire-state?
                                       (chunk/chunks-get-block chunks gen/flat-chunk %)))
                             (mapcat #(tnt-neighbors chunks %))
                             (remove pending))
                       due)]
    (mapcat (fn [pos]
              (cons [:spawn-entity (tnt/primed pos [(:tick world) pos])]
                    [(out/all (out/sound :tnt/primed pos 1.0 1.0))]))
            tnts)))

(defn- block-updates-deltas [world _events]
  (let [t   (long (:tick world))
        due (into (i/int-set) (comp (take-while (fn [[k _]] (<= (long k) t))) (mapcat val))
                  (:block-ticks world))]
    (when (seq due)
      (let [active  (state/active-chunks world)
            now     (into [] (comp (filter #(state/active-id? active %))
                                   (map chunk/id->block-pos)) due)
            parked  (into [] (remove #(state/active-id? active %)) due)
            changes (lww-changes (:chunks world)
                                 {:rules (:rules world) :tick t :time-of-day (:time-of-day world 0)
                                  :players (mapv (comp :pos val) (state/player-entries world))}
                                 now)
            changed (into #{} (map first) changes)
            again   (reduce (fn [m p]
                              (if-let [at (and (not (contains? changed p))
                                               (rules/again-tick (:chunks world)
                                                                 (chunk/chunks-get-block (:chunks world) gen/flat-chunk p)
                                                                 p t))]
                                (update m at (fnil conj []) (chunk/block-pos->id p))
                                m))
                            {} now)
            woken   (merge-with into again (eyeblossom-schedules world changes))]
        (concat
         [[:ticks-flushed t parked]]
         (when (seq woken) [[:schedule-ticks woken]])
         (when (seq changes)
           (concat [[:set-blocks changes]]
                   (fizz-deltas world changes)
                   (wash-deltas world changes)
                   (sponge-deltas world now changes)
                   (fall-deltas world changes)
                   (eyeblossom-deltas changes)
                   (tilt-deltas world changes)
                   (drip-fill-deltas world changes)))
         (ignite-deltas world now))))))

(defn block-updates [world events]
  [#(block-updates-deltas world events)])
