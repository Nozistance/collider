(ns collider.game.systems.keepalive
  (:require [collider.game.state :as state]
            [collider.proto.packets.play :as play]))

(set! *warn-on-reflection* true)

(def interval-ticks 20)
(def timeout-ticks 600)
(defn- keepalive-deltas [world _events]
  (let [t (long (:tick world))]
    (when (zero? (rem t interval-ticks))
      (into []
            (mapcat
             (fn [[eid e]]
               (if (> (- t (long (:last-echo-tick e))) timeout-ticks)
                 [[:remove-entity eid] [:close eid]]
                 [[:send eid {:packet/key ::play/keep-alive
                              :id (bit-and t 0xFFFFF)}]])))
            (state/player-entries world)))))

(defn keepalive [world events]
  [#(keepalive-deltas world events)])
