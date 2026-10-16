(ns collider.random.xoroshiro
  "The xoroshiro random of the game as lazy seqs of its longs."
  (:require [collider.random :as random]))

(set! *warn-on-reflection* true)

(def ^:private ^:const golden -7046029254386353131)

(def ^:private ^:const silver 7640891576956012809)

(defn- step [[^long lo ^long hi]]
  (let [hi (bit-xor hi lo)]
    [(bit-xor (Long/rotateLeft lo 49) hi (bit-shift-left hi 21))
     (Long/rotateLeft hi 28)]))

(defn- drawn ^long [[^long lo ^long hi]]
  (unchecked-add (Long/rotateLeft (unchecked-add lo hi) 17) lo))

(defn- of-pair [^long lo ^long hi]
  (map drawn (iterate step (if (zero? (bit-or lo hi))
                             [golden silver]
                             [lo hi]))))

(defn seeded
  "Returns the longs the random of seed gives, in order."
  [^long seed]
  (let [lo (bit-xor seed silver)]
    (of-pair (random/mix64 lo)
             (random/mix64 (unchecked-add lo golden)))))

(defn- pos-seed ^long [[x y z]]
  (let [s (bit-xor (long (unchecked-multiply-int (int x) 3129871))
                   (unchecked-multiply (long z) 116129781)
                   (long y))
        s (unchecked-add (unchecked-multiply (unchecked-multiply s s)
                                             42317861)
                         (unchecked-multiply s 11))]
    (bit-shift-right s 16)))

(defn positional
  "Returns the longs the random of seed forked for block pos gives."
  [^long seed pos]
  (let [[lo hi] (seeded seed)]
    (of-pair (bit-xor (pos-seed pos) (long lo)) hi)))

(defn int-below
  "Returns a whole number from 0 below bound drawn from the longs
  xs, paired with the longs left."
  [xs ^long bound]
  (let [bits #(bit-and (long (first %)) 0xFFFFFFFF)
        least (Integer/remainderUnsigned (int (- bound)) (int bound))]
    (loop [xs xs]
      (let [m (* (long (bits xs)) bound)]
        (if (< (bit-and m 0xFFFFFFFF) least)
          (recur (rest xs))
          [(bit-shift-right m 32) (rest xs)])))))

(defn int-between
  "Returns a whole number from lo to hi inclusive drawn from the
  longs xs, paired with the longs left."
  [xs ^long lo ^long hi]
  (let [[i more] (int-below xs (inc (- hi lo)))]
    [(+ lo (long i)) more]))
