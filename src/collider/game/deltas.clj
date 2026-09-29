(ns collider.game.deltas
  "Deltas of one tick and the thunks that make them."
  (:refer-clojure :exclude [merge])
  (:require [clojure.core.reducers :as r]
            [clojure.data.int-map :as i]
            [collider.game.delta :as delta]
            [collider.game.deltas.record :refer [->Deltas]])
  (:import (clojure.data.int_map PersistentIntMap)
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

(def empty-deltas (->Deltas [] (i/int-map) [] []))

(defn input
  "Returns the deltas that start a tick with events as input."
  ^Deltas [events]
  (->Deltas [[:advance-tick]] (i/int-map) [] (vec events)))

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
        (case (nth d 0)
          :fx (recur ds w e (conj! o (nth d 1)))
          (:merge-entity :track :tracking :set-slot :chunks-sent :push
           :damage :teleport :client-slots :award)
          (let [eid (nth d 1)]
            (recur ds w (assoc! e eid (conj (get e eid []) d)) o))
          (recur ds (conj! w d) e o)))
      (built w e o acc))))

(defn add
  "Returns acc with the deltas added after its own."
  ^Deltas [^Deltas acc deltas]
  (if-let [ds (seq (if delta/validate? (delta/check! deltas) deltas))]
    (added acc ds)
    acc))

(defn- joined [a b]
  (cond (zero? (count b)) a
        (zero? (count a)) b
        :else (into a b)))

(def ^:private ^:const select-leaf 4096)

(defn keyed
  "Returns map m as an int-map, which runs by key."
  [m]
  (if (instance? PersistentIntMap m) m (into (i/int-map) m)))

(defn- halves [m]
  (let [lo (long (key (first m)))
        hi (long (key (first (rseq m))))
        mid (+ lo (quot (- hi lo) 2))]
    [(i/range m lo mid) (i/range m (inc mid) hi)]))

(defn- folded [rf m ^long leaf]
  (if (<= (count m) leaf)
    (reduce rf [] m)
    (let [[a b] (halves m)
          t (fork #(folded rf b leaf))
          l (folded rf a leaf)]
      (joined l (join t)))))

(defn select
  "Returns (into [] xf m) for a transducer xf that keeps no state.
  The runs of more than leaf entries of an int-map go in parallel."
  ([xf m] (select xf m select-leaf))
  ([xf m leaf]
   (if (instance? PersistentIntMap m)
     (invoke #(folded (xf conj) m leaf))
     (into [] xf m))))

(defn vacant?
  "Returns true when map m holds no entry."
  [m]
  (reduce-kv (fn [_ _ _] (reduced false)) true m))

(defn- joined-by-eid [a b]
  (cond (vacant? b) a
        (vacant? a) b
        :else (i/merge-with into a b)))

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

(def merge-deltas merge)

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

(defn dim-of
  "Returns the dimension of the level effect m came from, or nil."
  [m]
  (:dim m))

(defn in-pool
  "Returns (f) run so that the folds inside it run in parallel."
  [f]
  (invoke f))

(defn fold
  "Returns the deltas of reducef over vector v, folded in parallel."
  [reducef v]
  (r/fold 1 (r/monoid merge (constantly empty-deltas)) reducef v))

(declare run)

(defn- ran ^Deltas [^Deltas acc f]
  (let [r (f)]
    (cond (instance? Deltas r) (merge acc r)
          (fn? (nth r 0 nil)) (merge acc (run (vec r)))
          :else (add acc r))))

(defn run
  "Returns the deltas of the thunks run in parallel.
  The order is the same every time."
  ^Deltas [fs]
  (if (< (count fs) 2)
    (reduce ran empty-deltas fs)
    (fold ran fs)))

(defn run-each
  "Returns the deltas of the thunks run one after another.
  The thunks a thunk hands back run in parallel."
  ^Deltas [fs]
  (reduce ran empty-deltas fs))

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

(defn of
  "Returns the deltas of the systems over world and deltas."
  ^Deltas [systems world deltas]
  (run (mapv (fn [s] (fn [] (s world deltas))) systems)))

(defn run-seq
  "Returns the deltas of thunks fs in order, as one vector."
  [fs]
  (into [] (mapcat (fn [f]
                     (let [r (f)]
                       (if (fn? (nth r 0 nil)) (run-seq (vec r)) r))))
        fs))
