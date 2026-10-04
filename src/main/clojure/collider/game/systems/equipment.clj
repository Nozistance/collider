(ns collider.game.systems.equipment
  "Attribute modifiers of the stacks players wear and hold, taken up
  in their turn."
  (:require [collider.game.attribute :as attribute]
            [collider.game.effect.account :as account]
            [collider.game.player :as player]))

(set! *warn-on-reflection* true)

(defn player-deltas
  "Returns the deltas that move the modifiers of the equipment of the
  player entry p that changed since its last turn."
  [_ p]
  (let [eid (key p) e (val p)
        [e' attrs] (attribute/equipped e (player/equipment e))]
    (when-not (identical? e e')
      (cons [:merge-entity eid
             (select-keys e' [:equipment-modifiers :last-equipment])]
            (account/changed-deltas eid e' attrs)))))
