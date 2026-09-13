(ns collider.game.block.tnt
  "TNT: lighting a block into the entity that falls and blows up."
  (:require [collider.random :as random]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:const fuse-ticks 80)
(def ^:const power 4.0)
(defn tnt-state?
  "Returns true when the block is TNT."
  [st] (block/tnt? (long st)))
(defn primed
  "Returns the lit TNT that rises out of a block at pos."
  ([pos seed] (primed pos seed fuse-ticks))
  ([[x y z :as pos] seed fuse]
   (let [a (* (random/of-key seed pos :ang) Math/PI 2.0)]
     {:type   :tnt
      :pos    [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
      :vel    [(* -0.02 (Math/sin a)) 0.2 (* -0.02 (Math/cos a))]
      :yaw    0.0 :pitch 0.0 :on-ground false
      :origin pos
      :fuse   fuse})))

(defn chain-primed
  "Returns the lit TNT set off by a nearby blast, with a shorter fuse."
  [pos seed]
  (primed pos seed (+ 10 (mod (long (hash [seed pos :fuse])) 20))))

(defn primed-origins
  "Returns the blocks that already have lit TNT rising out of them."
  [world]
  (into #{}
        (keep (fn [[_ e]] (when (= :tnt (:type e)) (:origin e))))
        (:entities world)))
