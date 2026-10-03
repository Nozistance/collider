(ns collider.num
  "Scalar casts to the float and int widths the game computes in.")

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

(defn fdiv
  "Returns a divided by b rounded to a float."
  ^double [^double a ^double b]
  (double (unchecked-float (/ a b))))
