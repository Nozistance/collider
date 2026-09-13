(ns collider.vec
  "Points and motions of three doubles, as V3 or as any three numbers."
  (:refer-clojure :exclude [+])
  (:import (collider.java V3)))

(set! *warn-on-reflection* true)

(defn v3?
  "Returns true if v is a V3."
  [v] (instance? V3 v))
(defn v3
  "Returns a V3 of three doubles, or of anything holding three numbers."
  (^V3 [v] (if (v3? v)
             v
             (let [[a b c] v] (V3. (double a) (double b) (double c)))))
  (^V3 [^double x ^double y ^double z] (V3. x y z)))

(defn x
  "Returns the x of a point."
  ^double [v] (if (v3? v) (.x ^V3 v) (double (nth v 0))))
(defn y
  "Returns the y of a point."
  ^double [v] (if (v3? v) (.y ^V3 v) (double (nth v 1))))
(defn z
  "Returns the z of a point."
  ^double [v] (if (v3? v) (.z ^V3 v) (double (nth v 2))))
(defn +
  "Returns the sum of two points."
  ^V3 [a b]
  (V3. (clojure.core/+ (x a) (x b))
       (clojure.core/+ (y a) (y b))
       (clojure.core/+ (z a) (z b))))

(defn dist-sq
  "Returns the square of the distance between two points, ignoring y."
  (^double [a b]
   (let [dx (- (x b) (x a))
         dz (- (z b) (z a))]
     (clojure.core/+ (* dx dx) (* dz dz))))
  (^double [a ^double tx ^double tz]
   (let [dx (- tx (x a))
         dz (- tz (z a))]
     (clojure.core/+ (* dx dx) (* dz dz)))))

(defn yaw-toward
  "Returns the yaw in degrees that looks from p at tgt."
  ^double [p tgt]
  (Math/toDegrees (Math/atan2 (- (x p) (x tgt))
                              (- (z tgt) (z p)))))

(defn wrap-deg
  "Returns the angle in degrees wrapped into -180 to 180."
  ^double [^double a]
  (let [a (rem a 360.0)]
    (cond (< a -180.0) (clojure.core/+ a 360.0)
          (>= a 180.0) (- a 360.0)
          :else a)))

(defn limit-angle
  "Returns cur turned toward target by at most step degrees."
  ^double [^double cur ^double target ^double step]
  (clojure.core/+ cur (Math/max (- step) (Math/min step (wrap-deg (- target cur))))))
