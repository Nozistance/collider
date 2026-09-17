(ns collider.vec
  "Points and motions of three doubles, as V3 or as any three numbers."
  (:refer-clojure :exclude [+])
  (:import (collider.java V3)))

(set! *warn-on-reflection* true)

(defn v3? [v] (instance? V3 v))
(defn v3
  (^V3 [v] (if (v3? v)
             v
             (let [[a b c] v] (V3. (double a) (double b) (double c)))))
  (^V3 [^double x ^double y ^double z] (V3. x y z)))

(defn x ^double [v] (if (v3? v) (.x ^V3 v) (double (nth v 0))))
(defn y ^double [v] (if (v3? v) (.y ^V3 v) (double (nth v 1))))
(defn z ^double [v] (if (v3? v) (.z ^V3 v) (double (nth v 2))))
(defn + ^V3 [a b]
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

(defn dist3-sq ^double [a b]
  (let [dx (- (x b) (x a)) dy (- (y b) (y a)) dz (- (z b) (z a))]
    (clojure.core/+ (* dx dx) (* dy dy) (* dz dz))))

(defn yaw-toward
  "Returns the yaw in degrees that looks from p at tgt."
  ^double [p tgt]
  (Math/toDegrees (Math/atan2 (- (x p) (x tgt))
                              (- (z tgt) (z p)))))

(defn wrap-deg ^double [^double a]
  (let [a (rem a 360.0)]
    (cond (< a -180.0) (clojure.core/+ a 360.0)
          (>= a 180.0) (- a 360.0)
          :else a)))

(defn limit-angle
  "Returns cur turned toward target by at most step degrees."
  ^double [^double cur ^double target ^double step]
  (clojure.core/+ cur (Math/max (- step) (Math/min step (wrap-deg (- target cur))))))
