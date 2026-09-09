(ns collider.game.deltas
  (:require [clojure.core.reducers :as r]
            [clojure.data.int-map :as i]
            [collider.game.delta :as delta]))

(set! *warn-on-reflection* true)

(def ^:private ^:const fold-leaf 64)
(def ^:private ^:const fold-threshold 64)
(defn pmapcat
  ([f v] (pmapcat f v fold-leaf fold-threshold))
  ([f v leaf threshold]
   (if (<= (count v) (long threshold))
     (into [] (mapcat f) v)
     (r/fold (long leaf) (r/monoid into vector)
             (fn [acc x] (into acc (f x)))
             v))))

(defrecord Deltas [world entities out])
(def empty-deltas (->Deltas [] (i/int-map) []))
(def entity-tags
  #{:merge-entity :track :tracking :set-slot :chunks-sent :push :damage :teleport :client-slots})

(defn add ^Deltas [^Deltas acc deltas]
  (loop [ds (seq (if delta/validate? (delta/check! deltas) deltas))
         w  (transient (.world acc))
         e  (transient (.entities acc))
         o  (transient (.out acc))]
    (if ds
      (let [d (first ds) ds (next ds)]
        (case (nth d 0)
          :fx    (recur ds w e (conj! o (nth d 1)))
          (:merge-entity :track :tracking :set-slot :chunks-sent :push :damage :teleport :client-slots)
          (let [eid (long (nth d 1))]
            (recur ds w (assoc! e eid (conj (get e eid []) d)) o))
          (recur ds (conj! w d) e o)))
      (->Deltas (persistent! w) (persistent! e) (persistent! o)))))

(defn merge-deltas ^Deltas [^Deltas a ^Deltas b]
  (->Deltas (into (.world a) (.world b))
            (i/merge-with into (.entities a) (.entities b))
            (into (.out a) (.out b))))

(defn fold [reducef v]
  (r/fold 1 (r/monoid merge-deltas (constantly empty-deltas)) reducef v))

(defn run ^Deltas [fs]
  (fold (fn [^Deltas acc f]
                  (let [r (f)]
                    (if (fn? (first r))
                      (merge-deltas acc (run (vec r)))
                      (add acc r))))
                fs))

(defn of ^Deltas [systems world events]
  (run (mapv (fn [s] (fn [] (s world events))) systems)))

(defn run-seq [fs]
  (into [] (mapcat (fn [f]
                     (let [r (f)]
                       (if (fn? (first r)) (run-seq (vec r)) r))))
        fs))
