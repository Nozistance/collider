(ns collider.world.blocks.mushroom
  "Huge mushroom blocks and the faces they show."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private sides
  {:down [0 -1 0] :up [0 1 0] :north [0 0 -1]
   :south [0 0 1] :west [-1 0 0] :east [1 0 0]})

(defn- at [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks p) 0))

(defn placed [chunks pos st]
  (let [self (block/block-of (long st))
        same? #(= self (block/block-of (at chunks (mapv + pos %))))
        face (fn [m k off]
               (assoc m k (if (same? off) :false :true)))]
    (block/state self
                 (reduce-kv face (block/props-of (long st)) sides))))

(defn updated [self ^long st at]
  (let [face (fn [m k off]
               (if (= self (block/block-of (at off)))
                 (assoc m k :false)
                 m))]
    (block/state self (reduce-kv face (block/props-of st) sides))))
