(ns collider.game.systems.signs
  "Signs that editors hold open."
  (:require [collider.game.state :as state]
            [collider.game.systems.blocks.reach :as reach]))

(set! *warn-on-reflection* true)

(defn- edited [world]
  (let [active (state/active-chunks world)]
    (for [[cid entries] (:block-entities world)
          :when (contains? active cid)
          [pos e] entries
          :when (:editor e)]
      [pos e])))

(defn- away? [world e pos]
  (let [p (get-in world [:entities (:editor e)])]
    (or (nil? p) (not (reach/in-edit-range? p pos)))))

(defn- release-deltas [world [pos e]]
  (when (away? world e pos)
    [[:set-block-entity pos (assoc e :editor nil)]]))

(defn sign-editors
  "Returns the deltas that take a sign back from an editor who left.
  Nobody is told, as the text did not change."
  [world _d]
  (into [] (mapcat (fn [entry] (release-deltas world entry)))
        (edited world)))
