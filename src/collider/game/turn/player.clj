(ns collider.game.turn.player
  "The turn of a player in the entity tick list, as ServerPlayer.tick
  (ServerPlayer.java:610): its hurt resistance counts down, its menu
  shows what moved or closes, and a spectator keeps to its camera.
  The rest of a player's tick comes after the level, in join order."
  (:require [collider.game.apply :as apply]
            [collider.game.camera :as camera]
            [collider.game.level :as level]
            [collider.game.systems.containers :as containers]))

(set! *warn-on-reflection* true)

(defn- rest-deltas [eid e]
  (when (pos? (long (or (:hurt-resist e) 0)))
    [[:rest eid]]))

(defn- turn-deltas [world eid e]
  (-> (vec (rest-deltas eid e))
      (into (containers/menu-deltas world eid e))
      (into (camera/follow-deltas world eid e))))

(defn- turn [[w acc] [eid]]
  (if-let [e (get (:entities w) eid)]
    (let [ds (turn-deltas w eid e)]
      [(apply/entities w ds) (into acc ds)])
    [w acc]))

(defn turns
  "Returns the deltas of the players of the level, each in its turn.
  A turn sees what the turns of the players before it did to them."
  [world]
  (nth (reduce turn [world []] (level/player-entries world)) 1))
