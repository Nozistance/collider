(ns collider.game.systems.players.tick
  "The tick of each player after the level, as the connection tick
  of ServerGamePacketListenerImpl. Players go in join order, each
  step sees the writes of the steps and players before it."
  (:require [collider.game.apply :as apply]
            [collider.game.deltas :as deltas]
            [collider.game.level :as level]
            [collider.game.out :as out]
            [collider.game.systems.attacks :as attacks]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.compasses :as compasses]
            [collider.game.systems.consume :as consume]
            [collider.game.systems.dripleaf :as dripleaf]
            [collider.game.systems.effects :as effects]
            [collider.game.systems.experience :as experience]
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
   effects/player-deltas
   consume/player-deltas
   compasses/player-deltas
   dripleaf/player-deltas
   items/player-pickups
   orbs/player-pickup
   attacks/wielded
   pose/player-deltas
   experience/player-deltas
   keepalive/player-deltas])

(defn- stepped [[w :as acc] eid f]
  (if-let [e (get (:entities w) eid)]
    (apply/then acc (f w (MapEntry/create eid e)))
    acc))

(defn player-tick
  "Returns the deltas of the tick of every player of the level."
  {:wake {:types #{:player}}}
  [world d]
  (let [fs (steps d)
        turn (fn [acc [eid _]] (reduce #(stepped %1 eid %2) acc fs))
        init [world deltas/empty-deltas]]
    (second (reduce turn init (level/player-entries world)))))
