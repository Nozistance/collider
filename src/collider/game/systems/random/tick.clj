(ns collider.game.systems.random.tick
  (:require [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.rnd :as rnd]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.dripstone :as dripstone]
            [collider.world.eyeblossom :as eyeblossom]
            [collider.world.gen :as gen]
            [collider.world.grow :as grow]
            [collider.world.liquid :as liquid])
  (:import (collider.world.chunk Section)))

(set! *warn-on-reflection* true)

(defn- near-player? [world ^long radius [x y z]]
  (or (= radius -1)
      (let [cx (+ (double x) 0.5) cy (+ (double y) 0.5) cz (+ (double z) 0.5)]
        (some (fn [eid]
                (when-let [[px py pz] (get-in world [:entities eid :pos])]
                  (let [dx (- (double px) cx) dy (- (double py) cy) dz (- (double pz) cz)]
                    (< (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))) (double radius)))))
              (vals (:players world))))))

(defn- cell-result [world chunks p ^long st]
  (let [rnd (fn [salt] (rnd/rnd [(:tick world) p salt]))]
    (if (= :lava (liquid/liquid-class st))
      (when (near-player? world (long (get-in world [:rules :fire-spread-radius-around-player] 128)) p)
        {:changes (liquid/lava-random-tick chunks gen/flat-chunk p rnd)})
      (let [drip (dripstone/drip chunks p st rnd)]
        {:drip drip
         :changes (concat (:changes drip)
                          (dripstone/random-changes chunks p st rnd)
                          (grow/random-tick chunks p st rnd (:time-of-day world 0)))}))))

(defn- section-cells [world chunks cid si speed]
  (let [t (long (:tick world)) cid (long cid) si (long si) speed (long speed)
        c (get chunks cid)
        [cx cz] (chunk/id->pos cid)
        y0 (* 16 (- si (long chunk/section-offset)))]
    (into []
          (keep (fn [^long i]
                  (let [lx (long (Math/floor (* 16.0 (rnd/rnd4 t cid (* 3 si) i))))
                        ly (long (Math/floor (* 16.0 (rnd/rnd4 t cid (inc (* 3 si)) i))))
                        lz (long (Math/floor (* 16.0 (rnd/rnd4 t cid (+ 2 (* 3 si)) i))))
                        st (chunk/get-block c lx (+ y0 ly) lz)]
                    (when (block/randomly-ticking? st)
                      [[(+ (* 16 (long cx)) lx) (+ y0 ly) (+ (* 16 (long cz)) lz)] st]))))
          (range speed))))

(defn- world-cells [world chunks speed]
  (into []
        (mapcat (fn [cid]
                  (when-let [c (get chunks cid)]
                    (into []
                          (mapcat (fn [[si ^Section s]]
                                    (when (and s (not (identical? s chunk/empty-section)))
                                      (section-cells world chunks cid si speed))))
                          (map-indexed vector (:sections c))))))
        (seq (state/active-chunks world))))

(defn- eyeblossom-changes [changes]
  (filter (fn [[_ st]] (eyeblossom/eyeblossom? (long st))) changes))

(defn- eyeblossom-sounds [changes]
  (for [[p st] (eyeblossom-changes changes)]
    (out/all (out/sound (eyeblossom/sound-kind (long st) true) p 1.0 1.0))))

(defn- eyeblossom-schedules [world changes]
  (let [chunks (:chunks world) t (long (:tick world))]
    (reduce (fn [m [p st]]
              (let [old (chunk/chunks-get-block chunks gen/flat-chunk p)]
                (merge-with into m (eyeblossom/cascade chunks p old t))))
            {} (eyeblossom-changes changes))))

(defn- drip-schedules [world drips]
  (reduce (fn [m {:keys [cauldron delay]}]
            (if cauldron
              (update m (+ (long (:tick world)) (long delay)) (fnil conj []) (chunk/block-pos->id cauldron))
              m))
          {} drips))

(defn- drip-events [drips]
  (for [{:keys [tip]} drips]
    (out/all (out/level-event 1504 tip 0))))

(defn- random-tick-deltas [world _events]
  (let [speed (long (get-in world [:rules :random-tick-speed] 3))
        chunks (:chunks world)]
    (when (pos? speed)
      (let [results (mapv (fn [[p st]] (cell-result world chunks p st)) (world-cells world chunks speed))
            changes (into [] (mapcat :changes) results)
            drips (into [] (keep :drip) results)]
        (when (or (seq changes) (seq drips))
          (let [woken (merge-with into (eyeblossom-schedules world changes) (drip-schedules world drips))]
            (concat (when (seq changes) [[:set-blocks changes]])
                    (when (seq woken) [[:schedule-ticks woken]])
                    (eyeblossom-sounds changes)
                    (drip-events drips))))))))

(defn random-ticks [world events]
  [#(random-tick-deltas world events)])
