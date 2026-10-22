(ns collider.vec
  "Points and motions of three doubles."
  (:import (collider V3)))

(set! *warn-on-reflection* true)

(defn v3? [v] (instance? V3 v))

(defn same?
  "Returns true when a and b are identical, two numbers of one class
  with the same bits, or two points with the same bits. Unlike =, it
  tells 0.0 from -0.0 and finds NaN the same as NaN. Other values are
  the same only when identical."
  [a b]
  (V3/same a b))

(defn v3
  "Returns v as a point, v itself when it is one. With x, y and z,
  the point of them."
  (^V3 [v]
   (if (v3? v)
     v
     (let [[a b c] v] (V3. (double a) (double b) (double c)))))
  (^V3 [^double x ^double y ^double z] (V3. x y z)))

(defn x
  "Returns the x of v, a point or a plain [x y z] vector."
  ^double [v] (if (v3? v) (.x ^V3 v) (double (nth v 0))))

(defn y ^double [v] (if (v3? v) (.y ^V3 v) (double (nth v 1))))

(defn z ^double [v] (if (v3? v) (.z ^V3 v) (double (nth v 2))))

(defn add ^V3 [a b]
  (V3. (+ (x a) (x b)) (+ (y a) (y b)) (+ (z a) (z b))))

(defn dot
  ^double [a b]
  (+ (* (x a) (x b)) (* (y a) (y b)) (* (z a) (z b))))

(defn length ^double [v] (Math/sqrt (dot v v)))

(defn normalized
  "Returns v scaled to length one. Returns zero when v is
  shorter than eps."
  ^V3 [v ^double eps]
  (let [l (length v)]
    (if (< l eps)
      (V3. 0.0 0.0 0.0)
      (V3. (/ (x v) l) (/ (y v) l) (/ (z v) l)))))

(defn dist-xz-sq
  "Returns the squared distance from a to b in the xz plane. With tx
  and tz the second point is given by its x and z."
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
