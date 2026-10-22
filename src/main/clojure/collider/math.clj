(ns collider.math
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
  ^double [^double a]
  (Mth/wrapDegrees (unchecked-float a)))

(defn frem
  "Returns the remainder of a divided by b as floats, with the sign of
  a."
  ^double [^double a ^double b]
  (Mth/rem (unchecked-float a) (unchecked-float b)))

(defn fdiv
  "Returns a divided by b rounded to a float."
  ^double [^double a ^double b]
  (double (unchecked-float (/ a b))))

(defn pack-degrees
  "Returns angle a in degrees as a signed byte, 256 steps a turn,
  computed in floats and floored to an int that saturates."
  ^long [^double a]
  (let [steps (fdiv (fmul (f32 a) 256.0) 360.0)]
    (long (unchecked-byte (unchecked-int (Math/floor steps))))))

(def ^:private ^:const deg-per-rad
  (let [pi (double (unchecked-float Math/PI))]
    (double (unchecked-float (/ 180.0 pi)))))

(def ^:const deg->rad
  "The factor from degrees to radians, rounded to a float."
  (double (unchecked-float (/ Math/PI 180.0))))

(defn degrees
  "Returns radians r as degrees, multiplied by a float constant."
  ^double [^double r]
  (* r deg-per-rad))

(defn atan2
  "Returns the angle of y, x in radians as the game finds it, close to
  the exact arctangent but not equal."
  ^double [^double y ^double x]
  (Mth/atan2 y x))

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
  "Returns the sine of a in radians at the precision of the game,
  65536 steps a turn rounded to a float."
  ^double [^double a] (sin-at (* a sin-scale)))

(defn cos
  "Returns the cosine of a in radians at the precision of the game,
  65536 steps a turn rounded to a float."
  ^double [^double a]
  (sin-at (+ (* a sin-scale) 16384.0)))
