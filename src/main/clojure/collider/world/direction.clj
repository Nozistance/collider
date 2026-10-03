(ns collider.world.direction
  "The six block faces, their offsets, turns and ids.")

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

(def horizontal-face {2 :north 3 :south 4 :west 5 :east})

(def ^:private player-order [:south :west :north :east])

(defn player-index
  "Returns the index of the horizontal direction of yaw, south first."
  ^long [yaw]
  (let [q (/ (* (double yaw) 4.0) 360.0)]
    (bit-and (long (Math/floor (+ q 0.5))) 3)))

(defn player-direction
  "Returns the horizontal direction a player with that yaw faces."
  [yaw]
  (nth player-order (player-index yaw)))

(defn- x-ahead [ax ay az x y z]
  (cond (> y x) [ay ax az]
        (> z y) [ax az ay]
        :else [ax ay az]))

(defn- z-ahead [ax ay az x y z]
  (cond (> y z) [ay az ax]
        (> x y) [az ax ay]
        :else [az ay ax]))

(defn- nearest-axes
  [^double pitch-sin ^double pitch-cos
   ^double yaw-sin ^double yaw-cos]
  (let [ax (if (pos? yaw-sin) :east :west)
        ay (if (neg? pitch-sin) :up :down)
        az (if (pos? yaw-cos) :south :north)
        x-turn (Math/abs yaw-sin) z-turn (Math/abs yaw-cos)
        x (* x-turn pitch-cos) y (Math/abs pitch-sin)
        z (* z-turn pitch-cos)]
    (if (> x-turn z-turn)
      (x-ahead ax ay az x y z)
      (z-ahead ax ay az x y z))))

(defn look-order
  "Returns the six directions as a player looking along yaw sees them.
  The nearest comes first, and pitch counts too."
  [yaw pitch]
  (let [p (Math/toRadians (double pitch))
        y (Math/toRadians (- (double yaw)))
        [a b c] (nearest-axes (Math/sin p) (Math/cos p)
                    (Math/sin y) (Math/cos y))]
    [a b c (opposite c) (opposite b) (opposite a)]))

(defn up
  [p]
  [(nth p 0) (inc (nth p 1)) (nth p 2)])

(defn down
  [p]
  [(nth p 0) (dec (nth p 1)) (nth p 2)])

(defn toward
  ([p d] (toward p d 1))
  ([p d ^long n]
   (let [[dx dy dz] (offset d)]
     [(+ (long (nth p 0)) (* (long dx) n))
      (+ (long (nth p 1)) (* (long dy) n))
      (+ (long (nth p 2)) (* (long dz) n))])))

(defn segment
  "Returns which of n equal turns the yaw falls in. The count n is a
  power of two."
  ^long [yaw ^long n]
  (let [q (/ (* (double yaw) n) 360.0)]
    (bit-and (long (Math/floor (+ q 0.5))) (dec n))))
