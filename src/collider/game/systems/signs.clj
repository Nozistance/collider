(ns collider.game.systems.signs
  "The sign an editor holds open, and when he loses it."
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
  "Returns the deltas taking a sign back from an editor who left.
  SignBlockEntity.tick clears him the same way, without telling
  anyone: the text on the sign did not change."
  [world _d]
  (into [] (mapcat (fn [entry] (release-deltas world entry)))
        (edited world)))
