(ns collider.game.deltas
  "Deltas of one tick and their jobs.
  A job is a source of deltas or of more jobs."
  (:refer-clojure :exclude [merge])
  (:require [clojure.core.reducers :as r]
            [clojure.data.int-map :as i]
            [collider.game.delta :as delta]))

(set! *warn-on-reflection* true)

(def ^:private ^:const fold-leaf 64)

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

(defrecord Deltas [world entities out input])

(defn world-of [^Deltas d] (.world d))

(defn entities-of [^Deltas d] (.entities d))

(defn out-of [^Deltas d] (.out d))

(defn input-of [^Deltas d] (.input d))

(def empty-deltas (->Deltas [] (i/int-map) [] []))

(defn input ^Deltas [events]
  (->Deltas [[:advance-tick]] (i/int-map) [] (vec events)))

(defn add ^Deltas [^Deltas acc deltas]
  (loop [ds (seq (if delta/validate? (delta/check! deltas) deltas))
         w (transient (world-of acc))
         e (transient (entities-of acc))
         o (transient (out-of acc))]
    (if ds
      (let [d (first ds) ds (next ds)]
        (case (nth d 0)
          :fx (recur ds w e (conj! o (nth d 1)))
          (:merge-entity :track :tracking :set-slot :chunks-sent :push
           :damage :teleport :client-slots :award)
          (let [eid (long (nth d 1))]
            (recur ds w (assoc! e eid (conj (get e eid []) d)) o))
          (recur ds (conj! w d) e o)))
      (->Deltas (persistent! w) (persistent! e) (persistent! o)
                (input-of acc)))))

(defn merge
  (^Deltas [^Deltas a ^Deltas b]
   (->Deltas (into (world-of a) (world-of b))
             (i/merge-with into (entities-of a) (entities-of b))
             (into (out-of a) (out-of b))
             (into (input-of a) (input-of b))))
  (^Deltas [a b & more] (reduce merge (merge a b) more)))

(def merge-deltas merge)

(defn inert?
  "Whether d changes nothing in the world it is applied to."
  [^Deltas d]
  (and (empty? (world-of d)) (zero? (count (entities-of d)))
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

(defn dim-of
  "Returns the dimension of the level effect m came from, or nil."
  [m]
  (:dim m))

(defn fold [reducef v]
  (r/fold 1 (r/monoid merge (constantly empty-deltas)) reducef v))

(defn run
  "Returns the Deltas of the jobs run in parallel.
  The order is the same every time."
  ^Deltas [fs]
  (fold (fn [^Deltas acc f]
          (let [r (f)]
            (cond (instance? Deltas r) (merge acc r)
                  (fn? (first r)) (merge acc (run (vec r)))
                  :else (add acc r))))
        fs))

(defn of ^Deltas [systems world deltas]
  (run (mapv (fn [s] (fn [] (s world deltas))) systems)))

(defn run-seq [fs]
  (into [] (mapcat (fn [f]
                     (let [r (f)]
                       (if (fn? (first r)) (run-seq (vec r)) r))))
        fs))
