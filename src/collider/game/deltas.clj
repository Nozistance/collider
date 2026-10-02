(ns collider.game.deltas
  "Deltas of one tick and the folds that make them."
  (:refer-clojure :exclude [merge])
  (:require [clojure.core.reducers :as r]
            [collider.data.long-map :as lm]
            [collider.game.delta :as delta]
            [collider.game.deltas.record :refer [->Deltas]])
  (:import (clojure.lang MapEntry)
           (collider.data LongMap)
           (collider.game.deltas.record Deltas)))

(set! *warn-on-reflection* true)

(def ^:private ^:const fold-leaf 64)

(defn- fork [f] (@#'r/fjfork (r/fjtask f)))

(defn- join [task] (@#'r/fjjoin task))

(defn- invoke [f] (@#'r/fjinvoke f))

(def ^:private ^:const fold-threshold 64)

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
  "Returns the map of f over vector v as a vector.
  The work runs in parallel when v is longer than threshold."
  ([f v] (pmapv f v fold-leaf fold-threshold))
  ([f v leaf threshold]
   (if (<= (count v) (long threshold))
     (mapv f v)
     (r/fold (long leaf) (r/monoid into vector)
             (fn [acc x] (conj acc (f x)))
             v))))

(defn world-of
  "Returns the deltas of d that change the level as a whole."
  [^Deltas d]
  (.world d))

(defn entities-of
  "Returns the deltas of d that change one entity, by eid."
  [^Deltas d]
  (.entities d))

(defn out-of
  "Returns the effects of d."
  [^Deltas d]
  (.out d))

(defn input-of
  "Returns the input deltas of d, the events of the tick."
  [^Deltas d]
  (.input d))

(defn as-vec
  "Returns the deltas of d as one vector: those of the level, those
  of each entity by eid, then the effects as [:fx msg]."
  [^Deltas d]
  (-> (world-of d)
      (into cat (vals (entities-of d)))
      (into (map #(vector :fx %)) (out-of d))))

(def empty-deltas (->Deltas [] (lm/long-map) [] []))

(defn input
  "Returns the deltas that start a tick with events as input."
  ^Deltas [events]
  (->Deltas [[:advance-tick]] (lm/long-map) [] (vec events)))

(defn- built ^Deltas [w e o acc]
  (->Deltas (persistent! w) (persistent! e) (persistent! o)
            (input-of acc)))

(defn- added ^Deltas [^Deltas acc ds]
  (loop [ds ds
         w (transient (world-of acc))
         e (transient (entities-of acc))
         o (transient (out-of acc))]
    (if ds
      (let [d (first ds) ds (next ds)]
        (let [tag (nth d 0)]
          (cond
            (identical? :fx tag) (recur ds w e (conj! o (nth d 1)))
            (contains? delta/entity-apply tag)
            (let [eid (nth d 1)]
              (recur ds w (assoc! e eid (conj (get e eid []) d)) o))
            :else (recur ds (conj! w d) e o))))
      (built w e o acc))))

(defn- add ^Deltas [^Deltas acc v]
  (if-let [ds (seq (if delta/validate? (delta/check! v) v))]
    (added acc ds)
    acc))

(defn of-vec
  "Returns the deltas of vector v, in its order."
  ^Deltas [v]
  (add empty-deltas v))

(defn- joined [a b]
  (cond (zero? (count b)) a
        (zero? (count a)) b
        :else (into a b)))

(def ^:private ^:const select-leaf 4096)

(defn keyed
  "Returns map m as a long map, which runs by key."
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

(defn vacant?
  "Returns true when map m holds no entry."
  [m]
  (reduce-kv (fn [_ _ _] (reduced false)) true m))

(defn- joined-by-eid [a b]
  (cond (vacant? b) a
        (vacant? a) b
        :else (lm/merge-with into a b)))

(defn- blank? [^Deltas d]
  (and (zero? (count (world-of d))) (vacant? (entities-of d))
       (zero? (count (out-of d))) (zero? (count (input-of d)))))

(defn- joined-deltas ^Deltas [^Deltas a ^Deltas b]
  (->Deltas (joined (world-of a) (world-of b))
            (joined-by-eid (entities-of a) (entities-of b))
            (joined (out-of a) (out-of b))
            (joined (input-of a) (input-of b))))

(defn merge
  "Returns the deltas of a followed by those of b."
  (^Deltas [^Deltas a ^Deltas b]
   (cond (blank? b) a
         (blank? a) b
         :else (joined-deltas a b)))
  (^Deltas [a b & more] (reduce merge (merge a b) more)))

(defn inert?
  "Returns true when d changes nothing in the world it applies to."
  [^Deltas d]
  (and (empty? (world-of d)) (vacant? (entities-of d))
       (empty? (input-of d))))

(defn- marked [dim m]
  (if (contains? m :dim) m (assoc m :dim dim)))

(defn with-dim
  "Returns d with its unmarked effects marked as those of level dim.
  An effect marked with no dimension is the server's."
  ^Deltas [^Deltas d dim]
  (if (empty? (out-of d))
    d
    (assoc d :out (mapv #(marked dim %) (out-of d)))))

(defn in-pool
  "Returns (f) run so that the folds inside it run in parallel."
  [f]
  (invoke f))

(defn- folded-into [f]
  (fn [acc x] (add acc (f x))))

(defn fold
  "Returns the deltas f gives for each batch of vector v, in order.
  The batches run in parallel."
  ^Deltas [f v]
  (if (< (count v) 2)
    (reduce (folded-into f) empty-deltas v)
    (r/fold 1 (r/monoid merge (constantly empty-deltas))
            (folded-into f) v)))

(defn merge-all
  "Returns the deltas of vector v merged in order, pairwise."
  ^Deltas [v]
  (let [n (count v)]
    (case n
      0 empty-deltas
      1 (nth v 0)
      (let [h (quot n 2)]
        (merge (merge-all (subvec v 0 h))
               (merge-all (subvec v h)))))))

(defn- forked [heavy? timed heaviest k f]
  (when (and (heavy? k) (not (identical? k heaviest)))
    (fork (timed k f))))

(defn- ran-here [timed tasks ks fs]
  (mapv (fn [t k f] (when-not t ((timed k f)))) tasks ks fs))

(defn- joined-all [tasks rs]
  (mapv (fn [t r] (if t (join t) r)) tasks rs))

(defn- run-all [timed ks fs]
  (merge-all (mapv (fn [k f] ((timed k f))) ks fs)))

(defn- run-forked [heavy? timed heaviest ks fs]
  (let [tasks (mapv #(forked heavy? timed heaviest %1 %2) ks fs)]
    (merge-all (joined-all tasks (ran-here timed tasks ks fs)))))

(defn run-weighed
  "Returns the deltas of thunks fs merged in their order.
  The thunks whose keys ks are heavy? run in parallel, the rest in
  order; timed wraps a thunk so its key keeps its weight."
  ^Deltas [heavy? timed ks fs]
  (if (= 1 (count fs))
    ((nth fs 0))
    (let [heaviest (reduce #(if (heavy? %2) %2 %1) nil ks)]
      (if (nil? heaviest)
        (run-all timed ks fs)
        (run-forked heavy? timed heaviest ks fs)))))
