(ns collider.game.systems.entities
  "The entities of a level, each in its turn."
  (:require [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.level :as level]
            [collider.game.mob.mobs :as mobs]
            [collider.game.systems.items :as items]
            [collider.game.systems.orbs :as orbs]
            [collider.game.turn.falling :as falling]
            [collider.game.turn.mob :as mob]
            [collider.game.turn.player :as player]
            [collider.game.turn.thrown :as thrown]
            [collider.game.turn.tnt :as tnt]))

(set! *warn-on-reflection* true)

(def ^:private kinds
  (into #{:item :experience-orb :falling-block :area-effect-cloud
          :tnt}
        cat [entity/thrown-types (keys mobs/types)]))

(defn- others
  "Returns the deltas of the entities but the players, kind by kind,
  after the deltas ds of the players."
  [world d ds]
  (let [[world fallen] (falling/turns world)
        ds (reduce deltas/merge ds
                   [(deltas/of-vec fallen)
                    (deltas/of-vec (items/turns world))
                    (orbs/orbs world d) (mob/turns world d)])
        [world thrown] (thrown/turns world ds)
        ds (deltas/merge ds (deltas/of-vec thrown))]
    (deltas/merge ds (deltas/of-vec (tnt/turns world ds)))))

(defn entities
  "Returns the deltas of the entities of the level in one tick, in
  the order of the entity tick list. The players go first, as most
  enter the list before the entities around them."
  {:wake {:types (conj kinds :player)}}
  [world d]
  (let [ds (deltas/of-vec (player/turns world))]
    (if (level/holds-types? world kinds)
      (others world d ds)
      ds)))
