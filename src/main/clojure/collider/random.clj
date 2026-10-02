(ns collider.random
  "Random numbers drawn from keys."
  (:import (clojure.lang Murmur3 Util)
           (collider RandomSupport)))

(set! *warn-on-reflection* true)

(defn- frac ^double [^long h]
  (/ (double (bit-and h 0xFFFFFF)) 16777216.0))

(defn- step ^long [^long h o]
  (unchecked-add-int (unchecked-multiply-int (unchecked-int h) 31)
                     (Util/hasheq o)))

(defn- coll-hash ^long [^long h ^long n]
  (Murmur3/mixCollHash (unchecked-int h) (unchecked-int n)))

(defn of-key
  "Returns a number from 0 to 1 for the given key or keys."
  (^double [ks] (frac (hash ks)))
  (^double [a b] (frac (coll-hash (step (step 1 a) b) 2)))
  (^double [a b c]
   (frac (coll-hash (step (step (step 1 a) b) c) 3)))
  (^double [a b c d]
   (frac (coll-hash (step (step (step (step 1 a) b) c) d) 4))))

(defn pitch
  "Returns a sound pitch around 1.0 for the given key or keys."
  (^double [ks] (+ 0.8 (* 0.4 (of-key ks))))
  (^double [a b c] (+ 0.8 (* 0.4 (of-key a b c)))))

(defn hinge-pitch
  "Returns the narrower sound pitch of doors and other hinges for the
  given key or keys."
  (^double [ks] (+ 0.9 (* 0.1 (of-key ks))))
  (^double [a b c] (+ 0.9 (* 0.1 (of-key a b c)))))

(defn mix64
  "Returns z mixed so that near longs give far apart ones."
  {:inline (fn [z] `(RandomSupport/mixStafford13 ~z))}
  ^long [^long z]
  (RandomSupport/mixStafford13 z))

(defn of-longs
  "Returns a number from 0 to 1 for the given longs."
  {:inline (fn [& args] `(RandomSupport/unit ~@args))
   :inline-arities #{3 4}}
  (^double [^long a ^long b ^long c] (RandomSupport/unit a b c))
  (^double [^long a ^long b ^long c ^long d]
   (RandomSupport/unit a b c d)))

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
  "Returns a number from 0 to 1 decided by tick t, eid and key k."
  (^double [t eid k] (of-longs (long t) (long eid) (hash k)))
  (^double [t eid k i]
   (of-longs (long t) (long eid) (hash k) (long i))))

(defn one-in?
  "Returns true with a chance of one in n."
  [t eid k ^long n]
  (zero? (long (* n (rnd t eid k)))))

(defn exp-delay
  "Returns a wait of at least one tick, drawn from an exponential law
  with the given mean."
  ^long [mean ^long t ^long eid kind]
  (let [r (max 1.0E-9 (of-longs t eid (hash kind)))]
    (max 1 (long (* (double mean) (- (Math/log r)))))))

(defn shuffled
  "Returns the vector xs shuffled. The function pick takes a count i
  and returns a whole number from 0 below i."
  [pick xs]
  (reduce (fn [v i]
            (let [j (long (pick i)) a (v (dec i))]
              (assoc v (dec i) (v j) j a)))
          (vec xs) (range (count xs) 1 -1)))
