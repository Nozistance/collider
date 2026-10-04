(ns collider.game.systems.effects
  "Mob effects of players, ticked in their turn."
  (:require [collider.game.effect.account :as account]
            [collider.game.entity :as entity]))

(set! *warn-on-reflection* true)

(defn- due? [e]
  (and (or (seq (:effects e)) (:dirty-attributes e))
       (entity/player? e)))

(defn player-deltas
  "Returns the deltas that tick the effects of the player entry p."
  [world p]
  (when (due? (val p))
    (account/tick-deltas world (key p) (val p))))

(defn player-sync
  "Returns the deltas that send the attributes of the player entry p
  that changed in its turn, once."
  [_ p]
  (account/sync-deltas (key p) (val p)))
