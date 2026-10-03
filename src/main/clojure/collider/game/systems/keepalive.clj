(ns collider.game.systems.keepalive
  "Keepalive pings and timeouts."
  (:require [collider.game.out :as out]
            [collider.game.systems.players :as players]))

(set! *warn-on-reflection* true)

(def interval-ticks
  300)

(defn player-deltas
  "Returns the deltas that ping player eid, or drop it when it left
  the last ping unanswered."
  [world [eid e]]
  (let [t (long (:tick world))]
    (when (>= (- t (long (:keepalive-at e t))) interval-ticks)
      (if (:keepalive-pending? e)
        (players/disconnect-deltas
          world eid e {:translate "disconnect.timeout"})
        (let [m {:keepalive-at t :keepalive-pending? true}]
          [[:merge-entity eid m] (out/to eid (out/keepalive t))])))))
