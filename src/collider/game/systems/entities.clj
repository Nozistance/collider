(ns collider.game.systems.entities
  "The entities of a level, each in its turn."
  (:require [collider.game.deltas :as deltas]
            [collider.game.mob.mobs :as mobs]
            [collider.game.systems.items :as items]
            [collider.game.systems.orbs :as orbs]
            [collider.game.turn.falling :as falling]
            [collider.game.turn.mob :as mob]))

(set! *warn-on-reflection* true)

(def ^:private kinds
  (into #{:item :experience-orb :falling-block} (keys mobs/types)))

(defn entities
  "Returns the deltas of the entities of the level in one tick, as
  ServerLevel.tick walks its entity tick list."
  {:wake {:types kinds :events #{:interact}}}
  [world d]
  (let [[world fallen] (falling/turns world)]
    (deltas/merge
      (deltas/merge (deltas/of-vec fallen)
                    (deltas/of-vec (items/turns world)))
      (deltas/merge (orbs/orbs world d) (mob/turns world d)))))
