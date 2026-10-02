(ns collider.world.blocks.bed
  "Beds, their heads and where a sleeper stands up."
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.blocks.halves :as halves]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn head-pos
  "Returns the cell of the head of the bed at pos, or nil when pos
  holds no bed."
  [chunks pos]
  (let [st (chunk/at chunks pos)]
    (when (= :bed (block/type-of st))
      (if (= :head (:part (block/props-of st)))
        pos
        (first (halves/partner chunks pos st))))))

(defn- step [d]
  (let [[dx _ dz] (dir/horizontal-offset d)] [dx dz]))

(defn- looks-toward? [^double yaw face]
  (let [[dx dz] (step face)
        a (Math/toDegrees (Math/atan2 (- dx) dz))
        d (^[double] Math/abs (rem (+ (- yaw a) 540.0) 360.0))]
    (< (^[double] Math/abs (- d 180.0)) 90.0)))

(defn- stand-up-offsets [forward side]
  (let [[fx fz] (step forward) [sx sz] (step side)]
    [[sx sz]
     [(- sx fx) (- sz fz)]
     [(- sx (* 2 fx)) (- sz (* 2 fz))]
     [(* -2 fx) (* -2 fz)]
     [(- (- sx) (* 2 fx)) (- (- sz) (* 2 fz))]
     [(- (- sx) fx) (- (- sz) fz)]
     [(- sx) (- sz)]
     [(+ (- sx) fx) (+ (- sz) fz)]
     [fx fz]
     [(+ sx fx) (+ sz fz)]
     [0 0]
     [(- fx) (- fz)]]))

(defn- box-top ^double [st]
  (reduce (fn [^double m [_ _ _ _ y1 _]]
            (max m (/ (double y1) 16.0)))
          Double/NEGATIVE_INFINITY (block/collision-boxes st)))

(defn- floor-below [chunks [x y z]]
  (let [below (box-top (chunk/at chunks [x (dec (long y)) z]))]
    (when (> below Double/NEGATIVE_INFINITY) (- below 1.0))))

(defn- floor-height [chunks pos]
  (let [here (box-top (chunk/at chunks pos))]
    (cond
      (>= here 1.0) nil
      (> here Double/NEGATIVE_INFINITY) here
      :else (floor-below chunks pos))))

(defn- edge ^double [c v]
  (+ (double c) (/ (double v) 16.0)))

(defn- box-hits? [x y z [bx by bz] [a b c d e f]]
  (let [x (double x) y (double y) z (double z)]
    (and (> (+ x 0.3) (edge bx a)) (< (- x 0.3) (edge bx d))
         (> (+ y 1.8) (edge by b)) (< y (edge by e))
         (> (+ z 0.3) (edge bz c)) (< (- z 0.3) (edge bz f)))))

(defn- floor-span [^double lo ^double hi]
  (range (long (Math/floor lo)) (inc (long (Math/floor hi)))))

(defn- player-fits? [chunks [x y z]]
  (let [x (double x) y (double y) z (double z)]
    (not-any? (fn [cell]
                (some #(box-hits? x y z cell %)
                      (block/collision-boxes (chunk/at chunks cell))))
              (for [bx (floor-span (- x 0.3) (+ x 0.3))
                    by (floor-span y (+ y 1.8))
                    bz (floor-span (- z 0.3) (+ z 0.3))]
                [bx by bz]))))

(def ^:private burning
  #{:fire :soul-fire :lava :magma-block :lava-cauldron})

(def ^:private campfires #{:campfire :soul-campfire})

(def ^:private prickly
  #{:wither-rose :sweet-berry-bush :cactus :powder-snow})

(defn- lit? [st]
  (= :true (:lit (block/props-of st))))

(defn- dangerous? [st]
  (let [b (block/block-of st)]
    (or (contains? burning b)
        (and (contains? campfires b) (lit? st))
        (contains? prickly b))))

(defn- dangerous-below? [chunks [x y z]]
  (dangerous? (chunk/at chunks [x (dec (long y)) z])))

(defn- dismount-position [chunks [x y z :as cell] safe?]
  (when-not (and safe? (dangerous? (chunk/at chunks cell)))
    (when-let [h (floor-height chunks cell)]
      (when-not (and safe? (<= (double h) 0.0)
                     (dangerous-below? chunks cell))
        (let [p [(+ (double x) 0.5) (+ (double y) (double h))
                 (+ (double z) 0.5)]]
          (when (player-fits? chunks p) p))))))

(defn- shifted [[x y z] [dx dz]]
  [(+ (long x) (long dx)) y (+ (long z) (long dz))])

(defn- above-bed [[x y z]]
  [(+ (double x) 0.5) (+ (double y) 1.1) (+ (double z) 0.5)])

(defn stand-up-position
  "Returns where a player facing yaw stands up from the bed at pos."
  [chunks pos ^double yaw]
  (let [st (chunk/at chunks pos)
        forward (block/facing-of st)
        right (dir/clockwise forward)
        side (if (looks-toward? yaw right) (dir/opposite right) right)
        cells (mapv #(shifted pos %) (stand-up-offsets forward side))
        found (or (some #(dismount-position chunks % true) cells)
                  (some #(dismount-position chunks % false) cells))]
    (or found (above-bed pos))))

(defn look-yaw
  "Returns the yaw in degrees from -180 to 180 that points from the
  second position to the middle of the first."
  ^double [[x _ z] [fx _ fz]]
  (let [dx (- (+ (double x) 0.5) (double fx))
        dz (- (+ (double z) 0.5) (double fz))
        a (- (Math/toDegrees (Math/atan2 dz dx)) 90.0)]
    (- (rem (+ (rem a 360.0) 540.0) 360.0) 180.0)))
