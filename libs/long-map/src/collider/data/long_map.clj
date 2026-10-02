(ns collider.data.long-map
  "Persistent maps and sets with long keys, in signed key order.

  Both types are Clojure collections, so `clojure.core` works on them.
  The functions here that share a name with `clojure.core` take
  primitive keys and skip boxing."
  (:refer-clojure :exclude [get contains? assoc dissoc conj disj first last
                            range merge merge-with])
  (:require [clojure.core.reducers :as r])
  (:import (clojure.lang IFn)
           (collider.data LongMap LongSet)))

(defn long-map
  "Returns a map of the given keys and values."
  (^LongMap [] LongMap/EMPTY)
  (^LongMap [k v] (.put LongMap/EMPTY (long k) v))
  (^LongMap [k v & kvs] (apply clojure.core/assoc (long-map k v) kvs)))

(defn long-set
  "Returns a set of the longs in `coll`."
  (^LongSet [] LongSet/EMPTY)
  (^LongSet [coll] (into LongSet/EMPTY coll)))

(defn get
  "Returns the value at `k` in map `m`, or `nf` when absent."
  {:inline (fn
             ([m k] `(.get ~(with-meta m {:tag `LongMap}) (long ~k)))
             ([m k nf]
              `(.get ~(with-meta m {:tag `LongMap}) (long ~k) ~nf)))
   :inline-arities #{2 3}}
  ([^LongMap m ^long k] (.get m k))
  ([^LongMap m ^long k nf] (.get m k nf)))

(defn contains?
  "Returns true when map or set `x` has key `k`."
  [x ^long k]
  (if (instance? LongSet x)
    (.has ^LongSet x k)
    (.has ^LongMap x k)))

(defn assoc
  "Returns map `m` with `v` at `k`."
  {:inline (fn [m k v]
             `(.put ~(with-meta m {:tag `LongMap}) (long ~k) ~v))}
  ^LongMap [^LongMap m ^long k v]
  (.put m k v))

(defn dissoc
  "Returns map `m` without key `k`."
  {:inline (fn [m k]
             `(.remove ~(with-meta m {:tag `LongMap}) (long ~k)))}
  ^LongMap [^LongMap m ^long k]
  (.remove m k))

(defn conj
  "Returns set `s` with `k`."
  {:inline (fn [s k]
             `(.add ~(with-meta s {:tag `LongSet}) (long ~k)))}
  ^LongSet [^LongSet s ^long k]
  (.add s k))

(defn disj
  "Returns set `s` without `k`."
  {:inline (fn [s k]
             `(.remove ~(with-meta s {:tag `LongSet}) (long ~k)))}
  ^LongSet [^LongSet s ^long k]
  (.remove s k))

(defn first
  "Returns the least key of map or set `x`. Throws when it is empty."
  ^long [x]
  (if (instance? LongSet x)
    (.first ^LongSet x)
    (.first ^LongMap x)))

(defn last
  "Returns the greatest key of map or set `x`. Throws when it is
  empty."
  ^long [x]
  (if (instance? LongSet x)
    (.last ^LongSet x)
    (.last ^LongMap x)))

(defn range
  "Returns the part of map or set `x` with keys from `lo` to `hi`,
  both included."
  [x ^long lo ^long hi]
  (if (instance? LongMap x)
    (.range ^LongMap x lo hi)
    (.range ^LongSet x lo hi)))

(defn union
  "Returns the keys in either set, `a` itself when `b` adds none."
  ^LongSet [^LongSet a ^LongSet b]
  (.union a b))

(defn intersection
  "Returns the keys in both sets."
  ^LongSet [^LongSet a ^LongSet b]
  (.intersection a b))

(defn difference
  "Returns the keys of `a` that are not in `b`."
  ^LongSet [^LongSet a ^LongSet b]
  (.difference a b))

(defn merge
  "Returns the entries of all maps, the rightmost value on a shared
  key."
  (^LongMap [] LongMap/EMPTY)
  (^LongMap [^LongMap a ^LongMap b] (.merge a b))
  (^LongMap [a b & more] (reduce merge a (cons b more))))

(defn merge-with
  "Returns the entries of all maps. A key in two maps gets `(f left
  right)`, and `f` must not return nil."
  (^LongMap [_] LongMap/EMPTY)
  (^LongMap [f ^LongMap a ^LongMap b] (.merge a b ^IFn f))
  (^LongMap [f a b & more]
   (reduce #(merge-with f %1 %2) a (cons b more))))

(defn diff
  "Reduces over the keys where `a` and `b` differ, in key order. For
  maps the step is `(f acc k old new)` with nil for an absent value.
  For sets it is `(f acc k added?)`, true for keys only in `b`. Parts
  that `a` and `b` share cost nothing."
  [a b f init]
  (if (instance? LongMap a)
    (.diff ^LongMap a ^LongMap b f init)
    (.diff ^LongSet a ^LongSet b f init)))

(defn key-array
  "Returns the keys of map or set `x` in order as a long array."
  ^longs [x]
  (if (instance? LongMap x)
    (.keys ^LongMap x)
    (.keys ^LongSet x)))

(defn from-sorted
  "Returns the set of keys `ks`, or the map of `ks` to `vs`."
  ([^longs ks] (LongSet/fromSorted ks))
  ([^longs ks ^objects vs] (LongMap/fromSorted ks vs)))

(extend-protocol r/CollFold
  LongMap
  (coll-fold [m n combinef reducef] (.fold m n combinef reducef))
  LongSet
  (coll-fold [s n combinef reducef] (.fold s n combinef reducef)))
