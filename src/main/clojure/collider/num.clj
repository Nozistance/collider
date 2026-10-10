(ns collider.num
  "Scalar arithmetic at the precision the game computes in."
  (:import (collider Mth)))

(set! *warn-on-reflection* true)

(defn f32
  "Returns x rounded to the nearest float, kept as a double."
  ^double [^double x]
  (double (unchecked-float x)))

(defn i32
  "Returns x wrapped to an int."
  ^long [^long x]
  (long (unchecked-int x)))

(defn floor
  ^long [^double x]
  (long (Math/floor x)))

(defn fmul
  "Returns the product of a and b rounded to a float."
  ^double [^double a ^double b]
  (double (unchecked-float (* a b))))

(defn fsub
  "Returns b subtracted from a, rounded to a float."
  ^double [^double a ^double b]
  (double (unchecked-float (- a b))))

(defn wrap-degrees
  "Returns angle a in degrees as a float from -180 up to 180. An
  infinite angle gives NaN."
  {:inline (fn [a] `(Mth/wrapDegrees (unchecked-float ~a)))}
  [a]
  (Mth/wrapDegrees (unchecked-float a)))

(defn fdiv
  "Returns a divided by b rounded to a float."
  ^double [^double a ^double b]
  (double (unchecked-float (/ a b))))

(def ^:private ^:const deg-per-rad
  (double (unchecked-float (/ 180.0 (double (unchecked-float Math/PI))))))

(defn degrees
  "Returns radians r as degrees, multiplied by a float constant."
  ^double [^double r]
  (* r deg-per-rad))

(defn atan2
  "Returns the angle of y, x in radians as the game finds it, close to
  the exact arctangent but not equal."
  {:inline (fn [y x] `(Mth/atan2 (double ~y) (double ~x)))}
  ^double [^double y ^double x]
  (Mth/atan2 y x))
