(ns collider.world.direction
  "The six block faces: their offsets, turns, and wire indices.")

(set! *warn-on-reflection* true)

(def six [:down :up :north :south :west :east])

(def horizontal [:north :south :west :east])

(def offset
  {:down [0 -1 0] :up [0 1 0] :north [0 0 -1] :south [0 0 1]
   :west [-1 0 0] :east [1 0 0]})

(def horizontal-offset
  {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})

(def around (mapv offset [:east :west :up :down :south :north]))

(def opposite
  {:down :up :up :down :north :south :south :north
   :west :east :east :west})

(def clockwise {:north :east :east :south :south :west :west :north})

(def counter-clockwise
  {:north :west :west :south :south :east :east :north})

(def axis {:down :y :up :y :north :z :south :z :west :x :east :x})

(def index {:down 0 :up 1 :north 2 :south 3 :west 4 :east 5})

(def from-index (zipmap (range) six))

(def face-offset (zipmap (range) (mapv offset six)))

(def face-facing {2 :north 3 :south 4 :west 5 :east})

(def opposite-index (int-array [1 0 3 2 5 4]))

(def ^:private player-order [:south :west :north :east])

(defn player-index ^long [yaw]
  (let [q (/ (* (double yaw) 4.0) 360.0)]
    (bit-and (long (Math/floor (+ q 0.5))) 3)))

(defn player-direction [yaw] (nth player-order (player-index yaw)))

(defn- nearest-axes
  [^double ps ^double pc ^double ys ^double yc]
  (let [ax (if (pos? ys) :east :west)
        ay (if (neg? ps) :up :down)
        az (if (pos? yc) :south :north)
        xy (Math/abs ys) ym (Math/abs ps) zy (Math/abs yc)
        xm (* xy pc) zm (* zy pc)]
    (cond
      (> xy zy) (cond (> ym xm) [ay ax az]
                      (> zm ym) [ax az ay]
                      :else [ax ay az])
      (> ym zm) [ay az ax]
      (> xm ym) [az ax ay]
      :else [az ay ax])))

(defn look-order
  "Returns the six directions as a player sees them, nearest first.
  The player looks along yaw and pitch."
  [yaw pitch]
  (let [p (Math/toRadians (double pitch))
        y (Math/toRadians (- (double yaw)))
        [a b c] (nearest-axes (Math/sin p) (Math/cos p)
                    (Math/sin y) (Math/cos y))]
    [a b c (opposite c) (opposite b) (opposite a)]))

(defn up [p] (mapv + p [0 1 0]))

(defn down [p] (mapv + p [0 -1 0]))
