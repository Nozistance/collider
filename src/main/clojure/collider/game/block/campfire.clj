(ns collider.game.block.campfire
  "The food that campfires take and cook."
  (:require [collider.game.block.furnace :as furnace]))

(set! *warn-on-reflection* true)

(defn recipe
  "Returns the campfire recipe that cooks the item, if any."
  [item]
  (furnace/recipe :campfire {:item item :count 1}))

(defn- free-slot [items]
  (first (keep-indexed (fn [i s] (when (nil? s) i)) items)))

(defn place-food
  "Returns the campfire with one piece of the item on its first free
  slot, or nil when it takes nothing."
  [e item]
  (when-let [i (free-slot (:items e))]
    (when-let [r (recipe item)]
      (-> e
          (assoc-in [:items i] {:item item :count 1})
          (assoc-in [:cook i] 0)
          (assoc-in [:cook-total i] (:time r))))))
