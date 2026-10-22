(ns collider.game.block.boat
  "Where a dispenser sets a boat, and the boat it sets."
  (:require [collider.math :as math]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private ^:const reach (+ 0.5625 (* 1.375 0.5)))

(def ^:private ^:const boat-height 0.5625)

(def ^:private yaw
  {:south 0.0 :west 90.0 :north 180.0 :east 270.0
   :up 270.0 :down 270.0})

(defn- water-cell [chunks pos facing]
  (let [there (mapv + pos (dir/offset facing))
        st (long (chunk/at chunks there))
        below (update there 1 dec)]
    (cond
      (block/water? st) there
      (and (block/air-type? st)
           (block/water? (chunk/at chunks below)))
      below)))

(defn- surface ^double [chunks [x y z]]
  (let [above (block/water? (chunk/at chunks [x (inc (long y)) z]))
        h (if above 1.0 (math/f32 (/ 8.0 9.0)))]
    (math/f32 (+ (double y) h))))

(defn- seat ^double [chunks water]
  (+ (math/fsub (surface chunks water) (math/f32 boat-height)) 0.101))

(defn entity
  "Returns the boat of kind facing out of pos, already where it
  sits after the fall onto the water. Nil when no water holds it."
  [chunks pos facing kind]
  (when-let [water (water-cell chunks pos facing)]
    (let [[x _ z] (map double pos)
          [sx _ sz] (map double (dir/offset facing))]
      {:type kind
       :pos [(+ x 0.5 (* sx reach))
             (seat chunks water)
             (+ z 0.5 (* sz reach))]
       :vel [0.0 0.0 0.0]
       :yaw (double (yaw facing))
       :pitch 0.0
       :on-ground false})))
