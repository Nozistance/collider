(ns collider.rnd

  (:import (clojure.lang Murmur3)))

(set! *warn-on-reflection* true)

(defn rnd
  ^double [ks]
  (/ (double (bit-and (long (hash ks)) 0xFFFFFF)) 16777216.0))

(defn mix64
  ^long [^long z]
  (let [z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 30)) -4658895280553007687)
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 27)) -7723592293110705685)]
    (bit-xor z (unsigned-bit-shift-right z 31))))

(defn rnd3 ^double [^long a ^long b ^long c]
  (/ (double (bit-and (mix64 (unchecked-add (mix64 (unchecked-add (mix64 a) b)) c)) 0xFFFFFF))
     1.6777216E7))

(defn rnd4 ^double [^long a ^long b ^long c ^long d]
  (rnd3 a b (unchecked-add (unchecked-multiply 31 c) d)))

(defn rnd-v4 ^double [^long seed-h ^long j ^long k ^long l]
  (let [h (+ (* 31 (+ (* 31 (+ (* 31 (+ 31 seed-h)) (Murmur3/hashLong j))) (Murmur3/hashLong k)))
             (Murmur3/hashLong l))
        h (Murmur3/mixCollHash (unchecked-int h) 4)]
    (/ (double (bit-and (long h) 0xFFFFFF)) 16777216.0)))
