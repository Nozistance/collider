(ns collider.cell
  "Block cells and section positions packed into longs.")

(set! *warn-on-reflection* true)

(defn pack
  {:inline (fn [x y z]
             `(bit-or (bit-shift-left (bit-and (long ~x) 0x3FFFFFF) 38)
                      (bit-shift-left (bit-and (long ~z) 0x3FFFFFF) 12)
                      (bit-and (long ~y) 0xFFF)))
   :inline-arities #{3}}
  (^long [[x y z]] (pack (long x) (long y) (long z)))
  (^long [^long x ^long y ^long z]
   (bit-or (bit-shift-left (bit-and x 0x3FFFFFF) 38)
           (bit-shift-left (bit-and z 0x3FFFFFF) 12)
           (bit-and y 0xFFF))))

(defn x
  {:inline (fn [c] `(bit-shift-right (long ~c) 38))}
  ^long [^long c]
  (bit-shift-right c 38))

(defn y
  {:inline (fn [c] `(bit-shift-right (bit-shift-left (long ~c) 52) 52))}
  ^long [^long c]
  (bit-shift-right (bit-shift-left c 52) 52))

(defn z
  {:inline (fn [c] `(bit-shift-right (bit-shift-left (long ~c) 26) 38))}
  ^long [^long c]
  (bit-shift-right (bit-shift-left c 26) 38))

(defn offset
  "Returns the cell dx dy dz away from cell c. Each coordinate wraps
  inside its own field."
  ^long [^long c ^long dx ^long dy ^long dz]
  (pack (unchecked-add (x c) dx) (unchecked-add (y c) dy)
        (unchecked-add (z c) dz)))

(defn unpack
  "Returns the coordinates of cell c as [x y z]."
  [^long c]
  [(x c) (y c) (z c)])

(defn pack-section
  {:inline (fn [x y z]
             `(bit-or (bit-shift-left (bit-and (long ~x) 0x3FFFFF) 42)
                      (bit-shift-left (bit-and (long ~z) 0x3FFFFF) 20)
                      (bit-and (long ~y) 0xFFFFF)))}
  ^long [^long x ^long y ^long z]
  (bit-or (bit-shift-left (bit-and x 0x3FFFFF) 42)
          (bit-shift-left (bit-and z 0x3FFFFF) 20)
          (bit-and y 0xFFFFF)))

(defn section-x
  {:inline (fn [s] `(bit-shift-right (long ~s) 42))}
  ^long [^long s]
  (bit-shift-right s 42))

(defn section-y
  {:inline (fn [s] `(bit-shift-right (bit-shift-left (long ~s) 44) 44))}
  ^long [^long s]
  (bit-shift-right (bit-shift-left s 44) 44))

(defn section-z
  {:inline (fn [s] `(bit-shift-right (bit-shift-left (long ~s) 22) 42))}
  ^long [^long s]
  (bit-shift-right (bit-shift-left s 22) 42))
