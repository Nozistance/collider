(ns collider.game.systems.entities
  "The entities of a level, each in its turn."
  (:require [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.mob.mobs :as mobs]
            [collider.game.systems.items :as items]
            [collider.game.systems.orbs :as orbs]
            [collider.game.turn.falling :as falling]
            [collider.game.turn.mob :as mob]
            [collider.game.turn.thrown :as thrown]))

(set! *warn-on-reflection* true)

(def ^:private kinds
  (into #{:item :experience-orb :falling-block :area-effect-cloud}
        cat [entity/thrown-types (keys mobs/types)]))

(defn entities
  "Returns the deltas of the entities of the level in one tick, as
  ServerLevel.tick walks its entity tick list."
  {:wake {:types kinds :events #{:interact}}}
  [world d]
  (let [[world fallen] (falling/turns world)
        ds (deltas/merge
             (deltas/merge (deltas/of-vec fallen)
                           (deltas/of-vec (items/turns world)))
             (deltas/merge (orbs/orbs world d) (mob/turns world d)))]
    (deltas/merge ds (deltas/of-vec (thrown/turns world ds)))))
