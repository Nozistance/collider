(ns collider.vec

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
(defn +

  ^V3 [a b]
  (V3. (clojure.core/+ (x a) (x b))
       (clojure.core/+ (y a) (y b))
       (clojure.core/+ (z a) (z b))))

(defn dist-sq
  (^double [a b]
   (let [dx (- (x b) (x a))
         dz (- (z b) (z a))]
     (clojure.core/+ (* dx dx) (* dz dz))))
  (^double [a ^double tx ^double tz]
   (let [dx (- tx (x a))
         dz (- tz (z a))]
     (clojure.core/+ (* dx dx) (* dz dz)))))

(defn yaw-toward ^double [p tgt]
  (Math/toDegrees (Math/atan2 (- (x p) (x tgt))
                              (- (z tgt) (z p)))))

(defn wrap-deg ^double [^double a]
  (let [a (rem a 360.0)]
    (cond (< a -180.0) (clojure.core/+ a 360.0)
          (>= a 180.0) (- a 360.0)
          :else a)))

(defn limit-angle ^double [^double cur ^double target ^double step]
  (clojure.core/+ cur (Math/max (- step) (Math/min step (wrap-deg (- target cur))))))
