(ns collider.game.block.boat
  "Where a dispenser sets a boat, and the boat it sets."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private ^:const reach (+ 0.5625 (* 1.375 0.5)))

(def ^:private yaw
  {:south 0.0 :west 90.0 :north 180.0 :east 270.0
   :up 270.0 :down 270.0})

(defn offset
  "Returns how far above the cell in front of pos a boat is set
  when the dispenser faces facing. 1.0 when that cell holds water,
  0.0 when it is air over water, nil when a boat will not sit."
  [chunks pos facing]
  (let [there (mapv + pos (dir/offset facing))
        st (chunk/at chunks there)
        below (update there 1 dec)]
    (cond
      (block/water? st) 1.0
      (and (block/air-type? st)
           (block/water? (chunk/at chunks below)))
      0.0)))

(defn entity
  "Returns the boat of kind set in front of pos, facing facing,
  raised by y-off. y-off is in blocks above the cell in front."
  [pos facing y-off kind]
  (let [[x y z] (map double pos)
        [sx sy sz] (map double (dir/offset facing))]
    {:type kind
     :pos [(+ x 0.5 (* sx reach))
           (+ y 0.5 (* sy 1.125) (double y-off))
           (+ z 0.5 (* sz reach))]
     :vel [0.0 0.0 0.0]
     :yaw (double (yaw facing))
     :pitch 0.0
     :on-ground false}))
