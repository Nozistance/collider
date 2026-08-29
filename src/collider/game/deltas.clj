(ns collider.game.deltas

  (:require [clojure.core.reducers :as r]
            [clojure.data.int-map :as i]))

(set! *warn-on-reflection* true)

(def ^:private ^:const fold-leaf 64)
(def ^:private ^:const fold-threshold 64)
(defn fold-each

  ([f v] (fold-each f v fold-leaf fold-threshold))
  ([f v leaf threshold]
   (if (<= (count v) (long threshold))
     (into [] (mapcat f) v)
     (r/fold (long leaf) (r/monoid into vector)
             (fn [acc x] (into acc (f x)))
             v))))

(defrecord Deltas [world ents out])
(def empty-deltas (->Deltas [] (i/int-map) []))

(def entity-tags
  #{:merge-entity :track :tracking :spawned :set-slot :chunks-sent :push :damage})

(defn bucket-deltas
  ^Deltas [^Deltas acc deltas]
  (loop [ds (seq deltas)
         w  (transient (.world acc))
         e  (transient (.ents acc))
         o  (transient (.out acc))]
    (if ds
      (let [d (first ds) ds (next ds)]
        (case (nth d 0)
          :send  (recur ds w e (conj! o (subvec d 1)))
          :close (recur ds w e (conj! o [(nth d 1) :close]))
          (:merge-entity :track :tracking :spawned :set-slot :chunks-sent :push :damage)
          (let [eid (long (nth d 1))]
            (recur ds w (assoc! e eid (conj (get e eid []) d)) o))
          (recur ds (conj! w d) e o)))
      (->Deltas (persistent! w) (persistent! e) (persistent! o)))))

(defn merge-deltas
  ^Deltas [^Deltas a ^Deltas b]
  (->Deltas (into (.world a) (.world b))
            (i/merge-with into (.ents a) (.ents b))
            (into (.out a) (.out b))))

(defn fold-buckets [reducef v]
  (r/fold 1 (r/monoid merge-deltas (constantly empty-deltas)) reducef v))

(defn fold
  ^Deltas [fs]
  (fold-buckets (fn [^Deltas acc f]
                  (let [r (f)]
                    (if (fn? (first r))
                      (merge-deltas acc (fold (vec r)))
                      (bucket-deltas acc r))))
                fs))

(defn of ^Deltas [systems world events]
  (fold (mapv (fn [s] (fn [] (s world events))) systems)))

(defn fold-seq [fs]
  (into [] (mapcat (fn [f]
                     (let [r (f)]
                       (if (fn? (first r)) (fold-seq (vec r)) r))))
        fs))
