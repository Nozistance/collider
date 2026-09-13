(ns collider.random
  (:import (clojure.lang Murmur3 Util)))

(set! *warn-on-reflection* true)

(defn- frac ^double [^long h]
  (/ (double (bit-and h 0xFFFFFF)) 16777216.0))

(defn- step ^long [^long h o]
  (unchecked-add-int (unchecked-multiply-int (unchecked-int h) 31) (Util/hasheq o)))

(defn- coll-hash ^long [^long h ^long n]
  (Murmur3/mixCollHash (unchecked-int h) (unchecked-int n)))

(defn of-key
  (^double [ks] (frac (hash ks)))
  (^double [a b] (frac (coll-hash (step (step 1 a) b) 2)))
  (^double [a b c] (frac (coll-hash (step (step (step 1 a) b) c) 3)))
  (^double [a b c d] (frac (coll-hash (step (step (step (step 1 a) b) c) d) 4))))

(defn pitch
  (^double [ks] (+ 0.8 (* 0.4 (of-key ks))))
  (^double [a b c] (+ 0.8 (* 0.4 (of-key a b c)))))

(defn hinge-pitch
  (^double [ks] (+ 0.9 (* 0.1 (of-key ks))))
  (^double [a b c] (+ 0.9 (* 0.1 (of-key a b c)))))

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
