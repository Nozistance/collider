(ns collider.game.systems.random.tick
  "Random block ticks for growth, melting, dripping and weathering."
  (:require [collider.data :as data]
            [collider.game.clock :as clock]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.areas :as areas]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.grow :as grow]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.precipitation :as precipitation]))

(set! *warn-on-reflection* true)

(defn- within? [radius [px py pz] [cx cy cz]]
  (let [dx (- (double px) (double cx))
        dy (- (double py) (double cy))
        dz (- (double pz) (double cz))]
    (< (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
       (double radius))))

(defn- near-player? [world ^long radius [x y z]]
  (or (= radius -1)
      (let [c [(+ (double x) 0.5) (+ (double y) 0.5)
               (+ (double z) 0.5)]]
        (some (fn [eid]
                (when-let [p (get-in world [:entities eid :pos])]
                  (within? radius p c)))
              (vals (:players world))))))

(defn- fire-radius ^long [world]
  (long (get-in world [:rules :fire-spread-radius-around-player]
                128)))

(defn- block-result [world chunks time p st roll]
  (let [drip (dripstone/drip chunks p st roll (:dim world))
        grown (grow/random-tick chunks p st roll time world)
        drops (grow/random-drops st roll)
        changes (concat (:changes drip)
                        (dripstone/random-changes chunks p st roll)
                        grown)]
    (when (or drip (seq drops) (seq changes))
      {:drip drip :drops drops :pos p :with (block/block-of st)
       :changes changes})))

(defn- cell-result [world chunks time p st]
  (let [roll (fn [salt] (random/of-key (:tick world) p salt))]
    (if (block/lava? st)
      (when (near-player? world (fire-radius world) p)
        {:changes (liquid/lava-random-tick chunks p roll)
         :with (block/block-of st)})
      (block-result world chunks time p st roll))))

(defn- local ^long [^long t ^long cid ^long k ^long i]
  (long (Math/floor (* 16.0 (random/of-longs t cid k i)))))

(defn- section-cells [acc t cid si speed c x0 z0]
  (let [t (long t) cid (long cid) si (long si) speed (long speed)
        x0 (long x0) z0 (long z0)
        y0 (* 16 (- si (long chunk/section-offset))) k (* 3 si)]
    (loop [i 0 acc acc]
      (if (= i speed)
        acc
        (let [lx (local t cid k i) ly (+ y0 (local t cid (+ k 1) i))
              lz (local t cid (+ k 2) i)
              st (chunk/get-block c lx ly lz)]
          (recur (inc i)
                 (if (block/randomly-ticking? st)
                   (conj acc [[(+ x0 lx) ly (+ z0 lz)] st])
                   acc)))))))

(def ^:private ^:table ticking
  (delay (let [a (boolean-array (data/block-state-count))]
           (dotimes [st (alength a)]
             (aset a st (block/randomly-ticking? st)))
           a)))

(defn- quiet? [s]
  (or (nil? s) (identical? s chunk/empty-section)
      (not (chunk/holds? s @ticking))))

(defn- chunk-cells [world speed cid c]
  (let [t (long (:tick world)) cid (long cid)
        [cx cz] (chunk/id->pos cid)
        x0 (* 16 (long cx)) z0 (* 16 (long cz))]
    (loop [si 0 acc []]
      (if (= si (long chunk/section-count))
        acc
        (recur (inc si)
               (if (quiet? (chunk/chunk-section c si))
                 acc
                 (section-cells acc t cid si speed c x0 z0)))))))

(def ^:private ^:const chunk-leaf 16)

(def ^:private ^:const chunk-threshold 64)

(defn- per-chunk [cids f]
  (deltas/pmapv f cids chunk-leaf chunk-threshold))

(defn- chunk-results [world chunks speed time cid]
  (when-let [c (get chunks cid)]
    (let [cells (chunk-cells world speed cid c)]
      (if (zero? (count cells))
        cells
        (mapv (fn [[p st]] (cell-result world chunks time p st))
              cells)))))

(defn- roll-of ^double [t cid i salt]
  (random/of-longs t cid i (hash salt)))

(defn- column-of [base t cid i salt]
  (+ (long base) (long (Math/floor (* 16.0 (roll-of t cid i salt))))))

(defn- precipitation-at [world chunks t cid max-height i]
  (when (< (roll-of t cid i :precipitation) (/ 1.0 48.0))
    (let [t (long t) cid (long cid)
          [cx cz] (chunk/id->pos cid)
          x (column-of (* 16 (long cx)) t cid i :precipitation-x)
          z (column-of (* 16 (long cz)) t cid i :precipitation-z)
          fill (roll-of t cid i :precipitation-fill)]
      (precipitation/tick-precipitation
        world chunks [x 0 z] max-height fill))))

(defn- max-snow ^long [world]
  (long (get-in world [:rules :max-snow-accumulation-height] 1)))

(defn- chunk-fallen [world chunks speed h cid]
  (let [t (long (:tick world)) cid (long cid)]
    (loop [i 0 acc []]
      (if (= i (long speed))
        acc
        (recur (inc i)
               (if-some [cs (precipitation-at world chunks t cid h i)]
                 (into acc cs)
                 acc))))))

(defn- drip-schedules [world drips]
  (reduce (fn [m {:keys [cauldron delay]}]
            (if cauldron
              (update m (+ (long (:tick world)) (long delay))
                      (fnil conj []) (chunk/block-pos->id cauldron))
              m))
          {} drips))

(defn- drip-events [drips]
  (for [{:keys [tip]} drips]
    (out/all (out/level-event out/dripstone-drip tip 0))))

(defn- drop-spawns [world results]
  (when (get-in world [:rules :block-drops] true)
    (for [{:keys [pos drops]} results
          [i stack] (map-indexed vector drops)]
      [:spawn-entity (items/popped world pos stack [:decay i])])))

(defn- run-closed [runs with ^long n]
  (if (pos? n) (conj runs [{:with with} n]) runs))

(defn- change-runs
  "Returns runs [author n] of the changes, n in a row of one author.
  The precipitation makes those fallen, the block that ticked those of
  each of results."
  [fallen results]
  (loop [rs (seq results) runs [] by :precipitation n (count fallen)]
    (if-not rs
      (run-closed runs by n)
      (let [r (first rs) k (count (:changes r)) w (:with r)]
        (cond (zero? k) (recur (next rs) runs by n)
              (= w by) (recur (next rs) runs by (+ n k))
              :else (recur (next rs) (run-closed runs by n) w k))))))

(defn- changed [world changes runs]
  (delta/authored (edit/set-deltas world changes runs)
                  (nth (peek runs) 0)))

(defn- result-deltas [world results changes drips runs]
  (let [woken (drip-schedules world drips)]
    (concat (when (seq changes) (changed world changes runs))
            (when (seq woken) [[:schedule-ticks woken]])
            (drip-events drips)
            (drop-spawns world results))))

(defn- ticked [world speed]
  (let [chunks (:chunks world) time (clock/day-ticks world)
        h (max-snow world) cids (areas/active-chunk-ids world)
        both (fn [cid]
               [(chunk-results world chunks speed time cid)
                (chunk-fallen world chunks speed h cid)])
        pairs (per-chunk cids both)
        results (into [] (mapcat #(nth % 0)) pairs)
        fallen (into [] (mapcat #(nth % 1)) pairs)
        changes (into fallen (mapcat :changes) results)
        drips (into [] (keep :drip) results)]
    (when (or (seq changes) (seq drips))
      (result-deltas world results changes drips
                     (change-runs fallen results)))))

(defn- random-tick-deltas [world _events]
  (let [speed (long (get-in world [:rules :random-tick-speed] 3))]
    (when (pos? speed)
      (ticked world speed))))

(defn random-ticks
  "Returns the deltas of the random ticks of the active chunks."
  {:wake {:types #{:player}}}
  [world d]
  (let [events (:input d)]
    (deltas/of-vec (random-tick-deltas world events))))
