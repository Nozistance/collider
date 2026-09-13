(ns collider.game.deltas
  "Deltas of one tick, a record of world, entity, out and input buckets, and
   the parallel fold that collects them from systems."
  (:refer-clojure :exclude [merge])
  (:require [clojure.core.reducers :as r]
            [clojure.data.int-map :as i]
            [collider.game.delta :as delta]))

(set! *warn-on-reflection* true)

(def ^:private ^:const fold-leaf 64)
(def ^:private ^:const fold-threshold 64)
(defn pmapcat
  "Like mapcat over a vector, in parallel when v is longer than threshold."
  ([f v] (pmapcat f v fold-leaf fold-threshold))
  ([f v leaf threshold]
   (if (<= (count v) (long threshold))
     (into [] (mapcat f) v)
     (r/fold (long leaf) (r/monoid into vector)
             (fn [acc x] (into acc (f x)))
             v))))

(defrecord Deltas [world entities out input])
(def empty-deltas (->Deltas [] (i/int-map) [] []))
(defn input
  "Returns the Deltas of a tick's network input: the events in arrival order,
   with the tick advance as its one world delta."
  ^Deltas [events]
  (->Deltas [[:advance-tick]] (i/int-map) [] (vec events)))

(defn add
  "Returns acc with deltas sorted into its buckets by tag; checks them against
   the schemas when validation is on."
  ^Deltas [^Deltas acc deltas]
  (loop [ds (seq (if delta/validate? (delta/check! deltas) deltas))
         w (transient (.world acc))
         e (transient (.entities acc))
         o (transient (.out acc))]
    (if ds
      (let [d (first ds) ds (next ds)]
        (case (nth d 0)
          :fx (recur ds w e (conj! o (nth d 1)))
          (:merge-entity :track :tracking :set-slot :chunks-sent :push :damage :teleport :client-slots)
          (let [eid (long (nth d 1))]
            (recur ds w (assoc! e eid (conj (get e eid []) d)) o))
          (recur ds (conj! w d) e o)))
      (->Deltas (persistent! w) (persistent! e) (persistent! o) (.input acc)))))

(defn merge
  "Returns the concatenation of Deltas, bucket by bucket, in argument order."
  (^Deltas [^Deltas a ^Deltas b]
   (->Deltas (into (.world a) (.world b))
             (i/merge-with into (.entities a) (.entities b))
             (into (.out a) (.out b))
             (into (.input a) (.input b))))
  (^Deltas [a b & more] (reduce merge (merge a b) more)))

(def merge-deltas merge)

(defn fold
  "Folds v with reducef in parallel, merging Deltas as the monoid; the result
   does not depend on the number of threads."
  [reducef v]
  (r/fold 1 (r/monoid merge (constantly empty-deltas)) reducef v))

(defn run
  "Returns the Deltas of the jobs fs run in parallel; a job returns deltas or
   more jobs."
  ^Deltas [fs]
  (fold (fn [^Deltas acc f]
          (let [r (f)]
            (if (fn? (first r))
              (merge acc (run (vec r)))
              (add acc r))))
        fs))

(defn of ^Deltas [systems world deltas]
  (run (mapv (fn [s] (fn [] (s world deltas))) systems)))

(defn run-seq [fs]
  (into [] (mapcat (fn [f]
                     (let [r (f)]
                       (if (fn? (first r)) (run-seq (vec r)) r))))
        fs))
