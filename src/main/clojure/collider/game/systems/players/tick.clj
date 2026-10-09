(ns collider.game.systems.players.tick
  "The tick of each player after the level. Players go in join order,
  and each step sees the writes of the steps and players before it."
  (:require [collider.game.apply :as apply]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.level :as level]
            [collider.game.mob.push :as push]
            [collider.game.out :as out]
            [collider.game.systems.attacks :as attacks]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.blocks.dig :as dig]
            [collider.game.systems.compasses :as compasses]
            [collider.game.systems.consume :as consume]
            [collider.game.systems.damage :as damage]
            [collider.game.systems.dripleaf :as dripleaf]
            [collider.game.systems.effects :as effects]
            [collider.game.systems.equipment :as equipment]
            [collider.game.systems.experience :as experience]
            [collider.game.systems.food :as food]
            [collider.game.systems.items :as items]
            [collider.game.systems.keepalive :as keepalive]
            [collider.game.systems.orbs :as orbs]
            [collider.game.systems.pose :as pose])
  (:import (clojure.lang MapEntry)))

(set! *warn-on-reflection* true)

(defn- ack [sequences]
  (fn [_ [eid _]]
    (when-let [sq (get sequences eid)]
      [(out/to eid (out/block-ack sq))])))

(defn- steps [d]
  [(ack (blocks/sequences (:input d)))
   dig/player-deltas
   damage/player-deltas
   effects/player-deltas
   consume/player-deltas
   equipment/player-deltas
   food/player-regeneration
   effects/player-sync
   compasses/player-deltas
   dripleaf/player-deltas
   push/player-shoves
   items/player-pickups
   orbs/player-pickup
   attacks/wielded
   pose/player-deltas
   food/player-deltas
   experience/player-deltas
   keepalive/player-deltas])

(defn- stepped [[w :as acc] eid f]
  (if-let [e (get (:entities w) eid)]
    (let [ds (f w (MapEntry/create eid e))]
      (apply/then acc (delta/authored ds eid :player)))
    acc))

(defn player-tick
  {:wake {:types #{:player}}}
  [world d]
  (let [fs (steps d)
        turn (fn [acc [eid _]] (reduce #(stepped %1 eid %2) acc fs))
        init [world deltas/empty-deltas]]
    (second (reduce turn init (level/player-entries world)))))
