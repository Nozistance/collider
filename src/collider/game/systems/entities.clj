(ns collider.game.systems.entities
  "The entities of a level, each in its turn."
  (:require [collider.game.deltas :as deltas]
            [collider.game.mob.mobs :as mobs]
            [collider.game.systems.items :as items]
            [collider.game.systems.orbs :as orbs]
            [collider.game.turn.mob :as mob]))

(set! *warn-on-reflection* true)

(def ^:private kinds
  (into #{:item :experience-orb} (keys mobs/types)))

(defn entities
  "Returns the deltas of the entities of the level in one tick, as
  ServerLevel.tick walks its entity tick list."
  {:wake {:types kinds :events #{:interact}}}
  [world d]
  (deltas/merge
    (deltas/merge (deltas/of-vec (items/turns world))
                  (orbs/orbs world d))
    (mob/turns world d)))
