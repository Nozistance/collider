(ns collider.par
  "Parallel folds over vectors and long maps."
  (:require [clojure.core.reducers :as r]
            [collider.data.long-map :as lm])
  (:import (clojure.lang MapEntry)
           (collider.data LongMap)
           (java.util.concurrent ForkJoinPool ForkJoinTask)))

(set! *warn-on-reflection* true)

(def ^:const fold-leaf
  "The size of the smallest part a parallel fold splits a vector
  into."
  64)

(def ^:private ^:const fold-threshold 64)

(def ^:private ^:const select-leaf 4096)

(defn fork
  "Returns the task that runs thunk f in the fork pool."
  [f]
  (@#'r/fjfork (r/fjtask f)))

(defn join
  "Returns the result of task, once it is done."
  [task]
  (@#'r/fjjoin task))

(defn in-pool
  "Returns (f) run so that the folds inside it run in parallel."
  [f]
  (@#'r/fjinvoke f))

(defn threads
  "Returns the number of threads that the folds started here share."
  ^long []
  (let [p (or (ForkJoinTask/getPool) @r/pool)]
    (.getParallelism ^ForkJoinPool p)))

(defn pmapcat
  "Returns the mapcat of f over vector v.
  The work runs in parallel when v is longer than threshold."
  ([f v] (pmapcat f v fold-leaf fold-threshold))
  ([f v leaf threshold]
   (if (<= (count v) (long threshold))
     (into [] (mapcat f) v)
     (r/fold (long leaf) (r/monoid into vector)
             (fn [acc x] (into acc (f x)))
             v))))

(defn pmapv
  "Returns (mapv f v), computed in parallel when v is longer than
  threshold."
  ([f v] (pmapv f v fold-leaf fold-threshold))
  ([f v leaf threshold]
   (if (<= (count v) (long threshold))
     (mapv f v)
     (r/fold (long leaf) (r/monoid into vector)
             (fn [acc x] (conj acc (f x)))
             v))))

(defn joined
  "Returns vector a followed by vector b. When one is empty the other
  comes back as it is."
  [a b]
  (cond (zero? (count b)) a
        (zero? (count a)) b
        :else (into a b)))

(defn keyed
  "Returns map m as a long map, which folds by key in parallel."
  [m]
  (if (instance? LongMap m) m (into (lm/long-map) m)))

(defn select
  "Returns (into [] xf m) for a transducer xf that keeps no state.
  The runs of more than leaf entries of a long map go in parallel."
  ([xf m] (select xf m select-leaf))
  ([xf m leaf]
   (if (instance? LongMap m)
     (let [rf (xf conj)]
       (r/fold leaf
               (fn ([] []) ([a b] (joined a b)))
               (fn [acc k v] (rf acc (MapEntry/create k v)))
               m))
     (into [] xf m))))
