(ns collider.vec
  "Points and motions of three doubles."
  (:import (collider V3)))

(set! *warn-on-reflection* true)

(defn v3? [v] (instance? V3 v))

(defn v3
  (^V3 [v]
   (if (v3? v)
     v
     (let [[a b c] v] (V3. (double a) (double b) (double c)))))
  (^V3 [^double x ^double y ^double z] (V3. x y z)))

(defn x ^double [v] (if (v3? v) (.x ^V3 v) (double (nth v 0))))

(defn y ^double [v] (if (v3? v) (.y ^V3 v) (double (nth v 1))))

(defn z ^double [v] (if (v3? v) (.z ^V3 v) (double (nth v 2))))

(defn add ^V3 [a b]
  (V3. (+ (x a) (x b)) (+ (y a) (y b)) (+ (z a) (z b))))

(defn dot
  "Returns the dot product of a and b."
  ^double [a b]
  (+ (* (x a) (x b)) (* (y a) (y b)) (* (z a) (z b))))

(defn length ^double [v] (Math/sqrt (dot v v)))

(defn normalized
  "Returns v scaled to length one. Returns zero when v is shorter
  than eps."
  ^V3 [v ^double eps]
  (let [l (length v)]
    (if (< l eps)
      (V3. 0.0 0.0 0.0)
      (V3. (/ (x v) l) (/ (y v) l) (/ (z v) l)))))

(defn dist-xz-sq
  "Returns the square of the distance between two points, ignoring y."
  (^double [a b]
   (let [dx (- (x b) (x a))
         dz (- (z b) (z a))]
     (+ (* dx dx) (* dz dz))))
  (^double [a ^double tx ^double tz]
   (let [dx (- tx (x a))
         dz (- tz (z a))]
     (+ (* dx dx) (* dz dz)))))

(defn dist-sq ^double [a b]
  (let [dx (- (x b) (x a)) dy (- (y b) (y a)) dz (- (z b) (z a))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn cell
  "Returns the block cell that holds point p."
  [p]
  [(long (Math/floor (x p)))
   (long (Math/floor (y p)))
   (long (Math/floor (z p)))])

(defn centre
  "Returns the centre point of the block cell p."
  [p]
  [(+ (x p) 0.5) (+ (y p) 0.5) (+ (z p) 0.5)])

(defn bottom-centre
  "Returns the centre point of the floor of the block cell p."
  [p]
  [(+ (x p) 0.5) (y p) (+ (z p) 0.5)])

(defn- axis-meets? [a b ^long i]
  (and (< (double (nth a i)) (double (nth b (+ i 3))))
       (> (double (nth a (+ i 3))) (double (nth b i)))))

(defn boxes-meet?
  "Returns true when boxes a and b overlap with some volume. A box
  is its low corner followed by its high corner."
  [a b]
  (and (axis-meets? a b 0) (axis-meets? a b 1) (axis-meets? a b 2)))

(defn yaw-toward
  "Returns the yaw in degrees that looks from p at tgt."
  ^double [p tgt]
  (let [dx (- (x p) (x tgt)) dz (- (z tgt) (z p))]
    (Math/toDegrees (Math/atan2 dx dz))))

(defn wrap-deg ^double [^double a]
  (let [a (rem a 360.0)]
    (cond (< a -180.0) (+ a 360.0)
          (>= a 180.0) (- a 360.0)
          :else a)))

(defn limit-angle
  "Returns cur turned toward target by at most step degrees."
  ^double [^double cur ^double target ^double step]
  (let [d (wrap-deg (- target cur))]
    (+ cur (Math/max (- step) (Math/min step d)))))

(def ^:private ^:const sin-scale 10430.378350470453)

(def ^:private sin-table
  (let [a (float-array 65536)]
    (dotimes [i 65536]
      (aset a i (float (Math/sin (/ (double i) sin-scale)))))
    a))

(defn- sin-at ^double [^double i]
  (aget ^floats sin-table
        (unchecked-int (bit-and (unchecked-long i) 65535))))

(defn sin
  "Returns the sine of a in radians from the table of Mth.sin."
  ^double [^double a] (sin-at (* a sin-scale)))

(defn cos
  "Returns the cosine of a in radians from the table of Mth.cos."
  ^double [^double a]
  (sin-at (+ (* a sin-scale) 16384.0)))
