(ns collider.world.blocks.mushroom
  "Huge mushroom blocks: which of their six faces show."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private sides
  {:down [0 -1 0] :up [0 1 0] :north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})

(defn- at [chunks template [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks template p) 0))

(defn placed
  "Returns the state a huge mushroom block takes at pos, showing every face
   that does not meet its own kind."
  [chunks template pos st]
  (let [self (block/block-of (long st))
        same? (fn [off] (= self (block/block-of (at chunks template (mapv + pos off)))))]
    (block/state self (reduce-kv (fn [m k off] (assoc m k (if (same? off) :false :true)))
                                 (block/props-of (long st)) sides))))

(defn updated
  "Returns st with the faces that meet its own kind hidden, leaving the rest
   as they are."
  [self ^long st at]
  (block/state self (reduce-kv (fn [m k off] (if (= self (block/block-of (at off))) (assoc m k :false) m))
                               (block/props-of st) sides)))
