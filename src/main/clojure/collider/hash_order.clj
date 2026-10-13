(ns collider.hash-order
  "The order in which a hash map gives back keys of given hash codes."
  (:import (java.util HashMap Map)))

(set! *warn-on-reflection* true)

(defn- key-of [h]
  (let [h (int h)]
    (reify Object
      (hashCode [_] h))))

(defn- filled ^HashMap [hashes]
  (let [m (HashMap.)]
    (reduce (fn [^long i h] (.put m (key-of h) i) (inc i)) 0 hashes)
    m))

(defn- order [^HashMap m]
  (vec (.values m)))

(defn of
  "Returns the indices of keys with hash codes hashes, put into a
  hash map in that order, in the order the map iterates them."
  [hashes]
  (order (filled hashes)))

(defn copied
  "Returns the indices of keys with hash codes hashes, put into a
  hash map in that order, in the order a copy of the map iterates
  them. The copy is a new map given all keys at once."
  [hashes]
  (order (HashMap. ^Map (filled hashes))))

(defn- slot ^long [h ^long cap]
  (let [h (long h)
        u (bit-and h 0xffffffff)]
    (bit-and (bit-xor h (unsigned-bit-shift-right u 16)) (dec cap))))

(defn at-capacity
  "Returns the indices of keys with hash codes hashes, put into a
  hash map in that order, in the order it iterates them once its
  table is cap wide. No bucket of it may hold a tree."
  [hashes ^long cap]
  (let [hs (vec hashes)]
    (vec (sort-by #(slot (nth hs %) cap) (range (count hs))))))

(defn grown
  "Returns how wide the table of a hash map cap wide that holds keys
  of hash codes hashes is once it takes a key of hash code h."
  ^long [^long cap hashes h]
  (let [k (slot h cap)
        bin (inc (count (filter #(= k (slot % cap)) hashes)))
        cap (if (and (>= bin 9) (< cap 64)) (* 2 cap) cap)]
    (if (> (inc (count hashes)) (quot (* 3 cap) 4)) (* 2 cap) cap)))

(defn by-name
  "Returns xs in the order a hash map given them in order iterates
  them, each keyed by the string (f x)."
  [f xs]
  (let [xs (vec xs)]
    (mapv xs (of (map #(.hashCode ^String (f %)) xs)))))

(defn compound
  "Returns the entries [k v] that have a value as a map that iterates
  as a hash map given them in order iterates their names."
  [entries]
  (->> (filter (comp some? second) entries)
       (by-name (comp name first))
       (apply concat)
       (apply array-map)))

(defn computed
  "Returns xs in the order a hash map iterates them after
  computeIfAbsent took them in turn, each keyed by (f x). A new key
  goes to the head of its bucket, so a later x of a shared bucket
  comes first, and of equal keys the first x stays."
  [f xs]
  (let [m (HashMap.)]
    (doseq [x xs] (.computeIfAbsent m (f x) (fn [_] x)))
    (vec (.values m))))
