(ns collider.data.state
  "Block state ids and the tables kept for every state."
  (:require [collider.data :as data])
  (:import (java.util Arrays)))

(set! *warn-on-reflection* true)

(defn- prop-order [b] (vec (keys (:props b))))

(defn- place-values
  "Returns the weight of each property of block facts b in a state
  offset, in property order."
  [b]
  (let [sizes (mapv #(count (get (:props b) %)) (prop-order b))]
    (mapv #(long (reduce * 1 (subvec sizes (inc (long %)))))
          (range (count sizes)))))

(defn- decode-props [b ^long offset]
  (let [props (:props b)
        step (fn [[acc ^long left] [k ^long w]]
               [(assoc acc k (nth (get props k) (quot left w)))
                (rem left w)])]
    (first (reduce step [{} offset]
                   (map vector (prop-order b) (place-values b))))))

(def ^:private ^:table state-blocks
  (delay
    (let [a (object-array (data/block-state-count))]
      (doseq [[block b] (data/blocks)
              :let [from (long (:first b))]
              i (range (data/state-count b))]
        (aset a (+ from (long i)) block))
      a)))

(defn each-run!
  "Calls f with each state id and its value in table t.
  States past the last known one are skipped."
  [{:keys [palette runs]} f]
  (let [n (data/block-state-count)]
    (doseq [[from to i] runs
            :let [v (nth palette i)]
            id (range from (inc (min (long to) (dec n))))]
      (f id v))))

(defn- object-table [t]
  (let [a (object-array (data/block-state-count))]
    (each-run! t (fn [id v] (aset a (int id) v)))
    a))

(defn- byte-table [t ^long default]
  (let [a (byte-array (data/block-state-count))]
    (Arrays/fill a (byte default))
    (each-run! t (fn [id v] (aset a (int id) (unchecked-byte (long v)))))
    a))

(def ^:private ^:table shape-table
  (delay (object-table (data/read-edn "shapes.edn"))))

(def ^:private ^:table y-coords-table
  (delay (update (data/read-edn "collision-ys.edn")
                 :states object-table)))

(defn collision-ys
  "Returns the y coordinates of the collision shapes of every state."
  [] @y-coords-table)

(def ^:private ^:table outline-table
  (delay (object-table (data/read-edn "outlines.edn"))))

(def ^:private ^:table sturdy-tables
  (delay (update-vals (data/read-edn "sturdy.edn")
                      #(byte-table % 63))))

(def ^:private ^:table flag-table
  (delay (byte-table (data/read-edn "flags.edn") 0)))

(defn shapes
  "Returns the collision boxes of every state, by id.
  A state that is a full cube has nil instead."
  ^objects []
  @shape-table)

(defn outlines
  "Returns the outline boxes of every state, by id.
  A state that is a full cube has nil instead."
  ^objects []
  @outline-table)

(defn sturdy
  "Returns which faces of every state hold things, by id."
  ^bytes []
  (:full @sturdy-tables))

(defn sturdy-center
  "Returns which faces of every state hold a centered thing."
  ^bytes []
  (:center @sturdy-tables))

(defn sturdy-rigid
  "Returns which faces of every state hold things rigidly, by id."
  ^bytes []
  (:rigid @sturdy-tables))

(defn flags
  ^bytes []
  @flag-table)

(defn- default-of [b]
  (decode-props b (- (long (:default b)) (long (:first b)))))

(def ^:private ^:table default-table
  (delay
    (into {}
          (map (fn [[block b]] [block (default-of b)]))
          (data/blocks))))

(defn defaults
  "Returns the default properties of every block."
  []
  @default-table)

(defn- prop-index ^long [block-name prop vs v]
  (or (first (keep-indexed (fn [i x] (when (= x v) i)) vs))
      (throw (ex-info "unknown property value"
                      {:block block-name :prop prop :value v}))))

(defn- state-offset ^long [block-name b wanted defaults]
  (let [props (:props b)
        want #(get wanted % (get defaults %))
        index #(prop-index block-name % (get props %) (want %))]
    (long (reduce + (long (:first b))
                  (map (fn [k ^long w] (* (index k) w))
                       (prop-order b) (place-values b))))))

(defn id
  "Returns the global state id of a block. Properties missing from
  wanted take their default values."
  (^long [block-name] (long (:default (data/info block-name))))
  (^long [block-name wanted]
   (let [b (data/info block-name)]
     (if (empty? wanted)
       (id block-name)
       (state-offset block-name b wanted
                     (get (defaults) block-name))))))

(defn block
  "Returns the block of block state id, or nil for no such state."
  [^long id]
  (when (< -1 id (data/block-state-count))
    (aget ^objects @state-blocks id)))

(defn props
  "Returns the block and the properties of state id, or nil."
  [^long id]
  (when-let [block-name (block id)]
    (let [b (get (data/blocks) block-name)]
      [block-name (decode-props b (- id (long (:first b))))])))
