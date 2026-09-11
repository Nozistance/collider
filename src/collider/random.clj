(ns collider.random)

(set! *warn-on-reflection* true)

(defn of-key ^double [ks]
  (/ (double (bit-and (long (hash ks)) 0xFFFFFF)) 16777216.0))

(defn mix64 ^long [^long z]
  (let [z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 30)) -4658895280553007687)
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 27)) -7723592293110705685)]
    (bit-xor z (unsigned-bit-shift-right z 31))))

(defn of-longs
  (^double [^long a ^long b ^long c]
   (/ (double (bit-and (mix64 (unchecked-add (mix64 (unchecked-add (mix64 a) b)) c)) 0xFFFFFF))
      1.6777216E7))
  (^double [^long a ^long b ^long c ^long d]
   (of-longs a b (unchecked-add (unchecked-multiply 31 c) d))))
