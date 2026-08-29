(ns collider.game.systems.inventory
  (:require [collider.proto.packets.play :as play]))

(set! *warn-on-reflection* true)

(defn- restore-deltas [world events]
  (for [[tag eid] events
        :when (= :player-join tag)
        :let [inv (get-in world [:entities eid :inventory])]
        :when (seq inv)]
    [:send eid (play/window-items (mapv inv (range 45)))]))

(defn- echo-deltas [world events]
  (for [[tag eid slot] events
        :when (and (= :creative-slot tag)
                   (<= 0 (long slot) 44)
                   (get-in world [:entities eid]))]
    [:send eid (play/set-slot slot (get-in world [:entities eid :inventory slot]))]))

(defn- inventory-deltas [world events]
  (concat (restore-deltas world events)
          (echo-deltas world events)))

(defn inventory [world events]
  [#(inventory-deltas world events)])
