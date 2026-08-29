(ns collider.game.tnt

  (:require [collider.rnd :as rnd]))

(set! *warn-on-reflection* true)

(def ^:const block-id 46)
(def ^:const fuse-ticks 80)
(def ^:const power 4.0)
(defn tnt-state? [st] (= block-id (bit-shift-right (long st) 4)))

(defn primed
  ([pos seed] (primed pos seed fuse-ticks))
  ([[x y z :as pos] seed fuse]
   (let [a (* (rnd/rnd [seed pos :ang]) Math/PI 2.0)]
     {:type :tnt
      :pos [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
      :vel [(* -0.02 (Math/sin a)) 0.2 (* -0.02 (Math/cos a))]
      :yaw 0.0 :pitch 0.0 :on-ground false
      :origin pos
      :fuse fuse})))

(defn chain-primed [pos seed]
  (primed pos seed (+ 10 (mod (long (hash [seed pos :fuse])) 20))))

(defn primed-origins [world]
  (into #{}
        (keep (fn [[_ e]] (when (= :tnt (:type e)) (:origin e))))
        (:entities world)))
