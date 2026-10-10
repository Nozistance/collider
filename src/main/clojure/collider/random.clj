(ns collider.random
  "Random numbers drawn from keys."
  (:import (clojure.lang Murmur3 Util)))

(set! *warn-on-reflection* true)

(defn- frac ^double [^long h]
  (/ (double (bit-and h 0xFFFFFF)) 16777216.0))

(defn- step ^long [^long h o]
  (unchecked-add-int (unchecked-multiply-int (unchecked-int h) 31)
                     (Util/hasheq o)))

(defn- coll-hash ^long [^long h ^long n]
  (Murmur3/mixCollHash (unchecked-int h) (unchecked-int n)))

(defn of-key
  "Returns a number at least 0 and below 1 for the given key or keys.
  The same keys always give the same number."
  (^double [ks] (frac (hash ks)))
  (^double [a b] (frac (coll-hash (step (step 1 a) b) 2)))
  (^double [a b c]
   (frac (coll-hash (step (step (step 1 a) b) c) 3)))
  (^double [a b c d]
   (frac (coll-hash (step (step (step (step 1 a) b) c) d) 4))))

(defn pitch
  "Returns a sound pitch from 0.8 below 1.2 that the keys decide."
  (^double [ks] (+ 0.8 (* 0.4 (of-key ks))))
  (^double [a b c] (+ 0.8 (* 0.4 (of-key a b c)))))

(defn hinge-pitch
  "Returns the pitch of a door or other hinge, from 0.9 below 1.0,
  that the keys decide."
  (^double [ks] (+ 0.9 (* 0.1 (of-key ks))))
  (^double [a b c] (+ 0.9 (* 0.1 (of-key a b c)))))

(defn mix64
  "Returns z mixed so that near longs give far apart ones."
  ^long [^long z]
  (let [z (unchecked-multiply
           (bit-xor z (unsigned-bit-shift-right z 30))
           -4658895280553007687)
        z (unchecked-multiply
           (bit-xor z (unsigned-bit-shift-right z 27))
           -7723592293110705685)]
    (bit-xor z (unsigned-bit-shift-right z 31))))

(defn of-longs
  "Returns a number at least 0 and below 1 for the given longs."
  (^double [^long a ^long b ^long c]
   (let [h (mix64 (unchecked-add (mix64 a) b))]
     (frac (mix64 (unchecked-add h c)))))
  (^double [^long a ^long b ^long c ^long d]
   (of-longs a b (unchecked-add (unchecked-multiply 31 c) d))))

(defn below
  "Returns a whole number from 0 below n picked by roll r."
  ^long [^double r ^long n]
  (long (Math/floor (* r n))))

(defn between
  "Returns a whole number from lo to hi inclusive picked by roll r."
  ^long [^double r ^long lo ^long hi]
  (+ lo (below r (inc (- hi lo)))))

(defn triangle
  "Returns a value near centre that two rolls a and b move by at most
  spread. Values near centre come most often."
  ^double [^double centre ^double spread ^double a ^double b]
  (+ centre (* spread (- a b))))

(defn rnd
  "Returns a number at least 0 and below 1 that tick t, eid and key k
  decide."
  (^double [t eid k] (of-longs (long t) (long eid) (hash k)))
  (^double [t eid k i]
   (of-longs (long t) (long eid) (hash k) (long i))))

(defn shuffled
  "Returns the vector xs shuffled. The function pick takes a count i
  and returns a whole number from 0 below i."
  [pick xs]
  (reduce (fn [v i]
            (let [j (long (pick i)) a (v (dec i))]
              (assoc v (dec i) (v j) j a)))
          (vec xs) (range (count xs) 1 -1)))
