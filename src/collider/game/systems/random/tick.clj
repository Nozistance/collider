(ns collider.game.systems.random.tick
  "Random block ticks for growth, melting, dripping and weathering."
  (:require [collider.game.clock :as clock]
            [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.state :as state]
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

(defn- block-result [world chunks p st roll]
  (let [drip (dripstone/drip chunks p st roll (:dim world))
        time (clock/day-ticks world)
        grown (grow/random-tick chunks p st roll time world)]
    {:drip    drip
     :drops   (grow/random-drops st roll)
     :pos     p
     :changes (concat (:changes drip)
                      (dripstone/random-changes chunks p st roll)
                      grown)}))

(defn- cell-result [world chunks p ^long st]
  (let [roll (fn [salt] (random/of-key (:tick world) p salt))]
    (if (block/lava? st)
      (when (near-player? world (fire-radius world) p)
        {:changes (liquid/lava-random-tick chunks p roll)})
      (block-result world chunks p st roll))))

(defn- cell-at [t cid si i c]
  (let [n (fn [^long k]
            (let [r (random/of-longs t cid (+ k (* 3 si)) i)]
              (long (Math/floor (* 16.0 r)))))
        [cx cz] (chunk/id->pos cid)
        y0 (* 16 (- (long si) (long chunk/section-offset)))
        lx (n 0) ly (n 1) lz (n 2)
        st (chunk/get-block c lx (+ y0 ly) lz)]
    (when (block/randomly-ticking? st)
      [[(+ (* 16 (long cx)) lx) (+ y0 ly) (+ (* 16 (long cz)) lz)]
       st])))

(defn- section-cells [world chunks cid si speed]
  (let [t (long (:tick world)) cid (long cid) si (long si)
        c (get chunks cid)]
    (into [] (keep #(cell-at t cid si (long %) c))
          (range (long speed)))))

(defn- blank? [s] (or (nil? s) (identical? s chunk/empty-section)))

(defn- chunk-cells [world chunks speed cid c]
  (let [cells (fn [si]
                (when-not (blank? (chunk/chunk-section c si))
                  (section-cells world chunks cid si speed)))]
    (into [] (mapcat cells) (range chunk/section-count))))

(def ^:private ^:const chunk-leaf 16)

(defn- per-chunk [world speed f]
  (let [chunks (:chunks world)
        one #(f world chunks speed %)]
    (deltas/pmapcat one (vec (state/active-chunks world))
                    chunk-leaf 64)))

(defn- chunk-results [world chunks speed cid]
  (when-let [c (get chunks cid)]
    (mapv (fn [[p st]] (cell-result world chunks p st))
          (chunk-cells world chunks speed cid c))))

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

(defn- chunk-fallen [world chunks speed cid]
  (let [t (long (:tick world)) h (max-snow world) cid (long cid)]
    (into [] (mapcat #(precipitation-at world chunks t cid h %))
          (range (long speed)))))

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

(defn- result-deltas [world results changes drips]
  (let [woken (drip-schedules world drips)]
    (concat (when (seq changes) (edit/set-deltas world changes))
            (when (seq woken) [[:schedule-ticks woken]])
            (drip-events drips)
            (drop-spawns world results))))

(defn- ticked [world speed]
  (let [results (per-chunk world speed chunk-results)
        fallen (per-chunk world speed chunk-fallen)
        changes (into fallen (mapcat :changes) results)
        drips (into [] (keep :drip) results)]
    (when (or (seq changes) (seq drips))
      (result-deltas world results changes drips))))

(defn- random-tick-deltas [world _events]
  (let [speed (long (get-in world [:rules :random-tick-speed] 3))]
    (when (pos? speed)
      (ticked world speed))))

(defn random-ticks [world d]
  (let [events (:input d)]
    [#(random-tick-deltas world events)]))
