(ns collider.game.deltas
  "Deltas of one tick and the folds that make them."
  (:refer-clojure :exclude [merge])
  (:require [clojure.core.reducers :as r]
            [collider.data.long-map :as lm]
            [collider.game.delta :as delta]
            [collider.game.deltas.record :refer [->Deltas]]
            [collider.par :as par])
  (:import (collider.game.deltas.record Deltas)))

(set! *warn-on-reflection* true)

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
  "Returns the deltas of d in one sequence. The deltas of the level
  come first, followed by those of each entity by eid, and the
  effects come last."
  [^Deltas d]
  (-> (world-of d)
      (into cat (vals (entities-of d)))
      (into (map #(vector :fx %)) (out-of d))))

(def empty-deltas
  "The deltas that change nothing."
  (->Deltas [] (lm/long-map) [] []))

(defn input
  "Returns the deltas that start a tick with events as input."
  ^Deltas [events]
  (->Deltas [[:advance-tick]] (lm/long-map) [] (vec events)))

(defn- open ^objects [^Deltas acc]
  (object-array [(transient (world-of acc))
                 (transient (entities-of acc))
                 (transient (out-of acc))]))

(defn- put [^objects c d]
  (let [tag (nth d 0)]
    (cond
      (identical? :fx tag) (aset c 2 (conj! (aget c 2) (nth d 1)))
      (contains? delta/entity-apply tag)
      (let [e (aget c 1) eid (nth d 1)]
        (aset c 1 (assoc! e eid (conj (get e eid []) d))))
      :else (aset c 0 (conj! (aget c 0) d)))
    c))

(defn- closed ^Deltas [^objects c input]
  (->Deltas (persistent! (aget c 0)) (persistent! (aget c 1))
            (persistent! (aget c 2)) input))

(defn- add ^Deltas [^Deltas acc v]
  (if-let [ds (seq (if delta/validate? (delta/check! v) v))]
    (closed (reduce put (open acc) ds) (input-of acc))
    acc))

(defn collecting
  "Returns an empty collector of deltas, which keeps them in the
  order they come. It takes no two adds at once."
  ^objects []
  (open empty-deltas))

(defn collect!
  "Adds delta d to collector c after the deltas it keeps."
  [c d]
  (put c (if delta/validate? (nth (delta/check! [d]) 0) d)))

(defn collect-all!
  "Adds the deltas of vector v to collector c in order."
  [c v]
  (reduce put c (if delta/validate? (delta/check! v) v)))

(defn collected
  "Returns the deltas collector c keeps. It takes no more after."
  ^Deltas [c]
  (closed c []))

(defn of-vec
  "Returns the deltas of vector v, in its order."
  ^Deltas [v]
  (add empty-deltas v))

(defn- joined-by-eid [a b]
  (cond (lm/empty? b) a
        (lm/empty? a) b
        :else (lm/merge-with into a b)))

(defn- blank? [^Deltas d]
  (and (zero? (count (world-of d))) (lm/empty? (entities-of d))
       (zero? (count (out-of d))) (zero? (count (input-of d)))))

(defn- joined-deltas ^Deltas [^Deltas a ^Deltas b]
  (->Deltas (par/joined (world-of a) (world-of b))
            (joined-by-eid (entities-of a) (entities-of b))
            (par/joined (out-of a) (out-of b))
            (par/joined (input-of a) (input-of b))))

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
  (and (empty? (world-of d)) (lm/empty? (entities-of d))
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

(defn fold-merged
  "Returns the deltas f gives for each item of vector v, merged in
  order. The items run in parallel."
  ^Deltas [f v]
  (let [rf (fn [acc x] (merge acc (f x)))]
    (if (< (count v) 2)
      (reduce rf empty-deltas v)
      (r/fold 1 (r/monoid merge (constantly empty-deltas)) rf v))))

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
    (par/fork (timed k f))))

(defn- ran-here [timed tasks ks fs]
  (mapv (fn [t k f] (when-not t ((timed k f)))) tasks ks fs))

(defn- joined-all [tasks rs]
  (mapv (fn [t r] (if t (par/join t) r)) tasks rs))

(defn- run-all [timed ks fs]
  (merge-all (mapv (fn [k f] ((timed k f))) ks fs)))

(defn- run-forked [heavy? timed heaviest ks fs]
  (let [tasks (mapv #(forked heavy? timed heaviest %1 %2) ks fs)]
    (merge-all (joined-all tasks (ran-here timed tasks ks fs)))))

(defn run-weighed
  "Returns the deltas of thunks fs merged in their order.
  The thunks whose keys ks are heavy? run in parallel and the rest
  in order. A thunk that timed wraps keeps the weight of its key."
  ^Deltas [heavy? timed ks fs]
  (if (= 1 (count fs))
    ((nth fs 0))
    (let [heaviest (reduce #(if (heavy? %2) %2 %1) nil ks)]
      (if (nil? heaviest)
        (run-all timed ks fs)
        (run-forked heavy? timed heaviest ks fs)))))
