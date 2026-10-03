(ns collider.world.blocks.hugemushroom
  "Huge mushroom blocks and the faces they show."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- at [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/at chunks p) 0))

(defn- faced ^long [self ^long st face]
  (block/state self (reduce-kv face (block/props-of st) dir/offset)))

(defn placed
  "Returns the huge mushroom block st placed at pos. It hides each
  face that touches a block of its own kind."
  [chunks pos st]
  (let [self (block/block-of (long st))
        same? #(= self (block/block-of (at chunks (mapv + pos %))))]
    (faced self st (fn [m side off]
                     (assoc m side (block/flag (not (same? off))))))))

(defn updated
  "Returns the huge mushroom block st of kind self with the faces
  hidden that now touch its own kind. The function at reads the
  block at an offset."
  [self ^long st at]
  (faced self st (fn [m side off]
                   (if (= self (block/block-of (at off)))
                     (assoc m side :false)
                     m))))
