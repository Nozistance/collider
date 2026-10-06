(ns collider.parallel
  "Parallel folds over vectors and long maps."
  (:require [clojure.core.reducers :as r]
            [collider.data.long-map :as lm])
  (:import (clojure.lang MapEntry)
           (collider.data LongMap)
           (java.util.concurrent ForkJoinPool ForkJoinTask)))

(set! *warn-on-reflection* true)

(def ^:const fold-leaf
  "The most items of a vector that a parallel fold runs as one part."
  64)

(def ^:private ^:const select-leaf 4096)

(defn fork
  "Starts thunk f in the fork pool and returns its task."
  ^ForkJoinTask [f]
  (.fork (ForkJoinTask/adapt ^Callable f)))

(defn join
  "Returns the result of task, waiting until it is done."
  [^ForkJoinTask task]
  (.join task))

(defn in-pool
  "Returns (f), run so that the folds inside it go in parallel."
  [f]
  (if (ForkJoinTask/inForkJoinPool)
    (f)
    (.invoke ^ForkJoinPool @r/pool (ForkJoinTask/adapt ^Callable f))))

(defn threads
  "Returns the number of threads that the folds started here share."
  ^long []
  (let [p (or (ForkJoinTask/getPool) @r/pool)]
    (.getParallelism ^ForkJoinPool p)))

(defn joined
  "Returns (into a b) for vectors a and b. When one is empty the other
  comes back as it is."
  [a b]
  (cond (zero? (count b)) a
        (zero? (count a)) b
        :else (into a b)))

(def ^:private joinedv (r/monoid joined vector))

(defn pmapv
  "Returns (mapv f v). When v is longer than threshold, the work goes
  in parallel in parts of at most leaf items."
  ([f v] (pmapv f v fold-leaf fold-leaf))
  ([f v leaf threshold]
   (if (<= (count v) (long threshold))
     (mapv f v)
     (r/fold leaf joinedv #(conj %1 (f %2)) v))))

(defn keyed
  "Returns map m as a long map, which runs over its keys in ascending
  order and folds in parallel."
  [m]
  (if (instance? LongMap m) m (into (lm/long-map) m)))

(defn select
  "Returns (into [] xf m) for a transducer xf that keeps no state.
  Over a long map the work goes in parallel in parts of about leaf
  entries."
  ([xf m] (select xf m select-leaf))
  ([xf m leaf]
   (if (instance? LongMap m)
     (let [rf (xf conj)]
       (r/fold leaf joinedv #(rf %1 (MapEntry/create %2 %3)) m))
     (into [] xf m))))
