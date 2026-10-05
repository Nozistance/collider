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
