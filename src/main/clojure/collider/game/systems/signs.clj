(ns collider.game.systems.signs
  "Signs that editors hold open."
  (:require [collider.game.reach :as reach]))

(set! *warn-on-reflection* true)

(defn- away? [world e pos]
  (let [p (get-in world [:entities (:editor e)])]
    (or (nil? p) (not (reach/in-edit-range? p pos)))))

(defn tick-deltas
  "Returns the deltas of one tick of the sign e at pos: it goes back
  from an editor who left. Nobody is told, as the text did not
  change."
  [world [pos e]]
  (when (and (:editor e) (away? world e pos))
    [[:set-block-entity pos (assoc e :editor nil)]]))
