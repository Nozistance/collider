(ns collider.world.blocks.bed
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.blocks.connect :as connect]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn head-pos [chunks pos]
  (let [st (gen/at chunks pos)]
    (when (= :bed (block/type-of st))
      (let [head (if (= :head (:part (block/props-of st))) pos (mapv + pos (connect/bed-partner-offset st)))]
        (when (= :head (:part (block/props-of (gen/at chunks head))))
          head)))))

(def ^:private steps {:north [0 -1] :south [0 1] :west [-1 0] :east [1 0]})
(defn- facing-angle? [dir ^double yaw]
  (let [[dx dz] (steps dir)
        a (Math/toDegrees (Math/atan2 (- dx) dz))
        d (^[double] Math/abs (rem (+ (- yaw a) 540.0) 360.0))]
    (< (^[double] Math/abs (- d 180.0)) 90.0)))

(defn- stand-up-offsets [forward side]
  (let [[fx fz] (steps forward) [sx sz] (steps side)]
    [[sx sz] [(- sx fx) (- sz fz)] [(- sx (* 2 fx)) (- sz (* 2 fz))] [(* -2 fx) (* -2 fz)]
     [(- (- sx) (* 2 fx)) (- (- sz) (* 2 fz))] [(- (- sx) fx) (- (- sz) fz)] [(- sx) (- sz)]
     [(+ (- sx) fx) (+ (- sz) fz)] [fx fz] [(+ sx fx) (+ sz fz)] [0 0] [(- fx) (- fz)]]))

(defn- box-top ^double [st]
  (reduce (fn [^double m [_ _ _ _ y1 _]] (max m (/ (double y1) 16.0))) Double/NEGATIVE_INFINITY (block/collision-boxes st)))

(defn- floor-height [chunks [x y z :as pos]]
  (let [here (box-top (gen/at chunks pos))]
    (cond
      (and (> here Double/NEGATIVE_INFINITY) (< here 1.0)) here
      (>= here 1.0) nil
      :else (let [below (box-top (gen/at chunks [x (dec (long y)) z]))]
              (when (> below Double/NEGATIVE_INFINITY) (- below 1.0))))))

(defn- player-fits? [chunks [x y z]]
  (let [x (double x) y (double y) z (double z)]
    (not-any? (fn [[bx by bz]]
                (some (fn [[a b c d e f]]
                        (and (> (+ x 0.3) (+ bx (/ (double a) 16.0))) (< (- x 0.3) (+ bx (/ (double d) 16.0)))
                             (> (+ y 1.8) (+ by (/ (double b) 16.0))) (< y (+ by (/ (double e) 16.0)))
                             (> (+ z 0.3) (+ bz (/ (double c) 16.0))) (< (- z 0.3) (+ bz (/ (double f) 16.0)))))
                      (block/collision-boxes (gen/at chunks [bx by bz]))))
              (for [bx [(long (Math/floor (- x 0.3))) (long (Math/floor (+ x 0.3)))]
                    by (range (long (Math/floor y)) (inc (long (Math/floor (+ y 1.8)))))
                    bz [(long (Math/floor (- z 0.3))) (long (Math/floor (+ z 0.3)))]]
                [bx by bz]))))

(def ^:private burning #{:fire :soul-fire :lava :magma-block :lava-cauldron})
(def ^:private campfires #{:campfire :soul-campfire})
(def ^:private prickly #{:wither-rose :sweet-berry-bush :cactus :powder-snow})
(defn- dangerous? [st]
  (let [b (block/block-of st)]
    (or (contains? burning b)
        (and (contains? campfires b) (= :true (:lit (block/props-of st))))
        (contains? prickly b))))

(defn- dismount-position [chunks [x y z :as cell] safe?]
  (when-not (and safe? (dangerous? (gen/at chunks cell)))
    (when-let [h (floor-height chunks cell)]
      (when-not (and safe? (<= (double h) 0.0) (dangerous? (gen/at chunks [x (dec (long y)) z])))
        (let [p [(+ (double x) 0.5) (+ (double y) (double h)) (+ (double z) 0.5)]]
          (when (player-fits? chunks p) p))))))

(defn stand-up-position [chunks [x y z :as pos] ^double yaw]
  (let [st      (gen/at chunks pos)
        forward (block/facing-of st)
        right   (dir/clockwise forward)
        side    (if (facing-angle? right yaw) (dir/opposite right) right)
        cells   (mapv (fn [[dx dz]] [(+ (long x) (long dx)) y (+ (long z) (long dz))])
                      (stand-up-offsets forward side))
        found   (or (some #(dismount-position chunks % true) cells)
                    (some #(dismount-position chunks % false) cells))]
    (or found [(+ (double x) 0.5) (+ (double y) 1.1) (+ (double z) 0.5)])))

(defn look-yaw ^double [[x _ z] [fx _ fz]]
  (let [dx (- (+ (double x) 0.5) (double fx))
        dz (- (+ (double z) 0.5) (double fz))
        a  (- (Math/toDegrees (Math/atan2 dz dx)) 90.0)]
    (- (rem (+ (rem a 360.0) 540.0) 360.0) 180.0)))
