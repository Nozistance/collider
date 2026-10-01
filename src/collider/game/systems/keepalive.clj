(ns collider.game.systems.keepalive
  "Keepalive pings and timeouts."
  (:require [collider.game.out :as out]
            [collider.game.systems.sleep :as sleep]))

(set! *warn-on-reflection* true)

(def interval-ticks 300)

(defn player-deltas
  "Returns the deltas that ping player p, an entry, or drop it when
  it left the last ping unanswered."
  [world [eid e]]
  (let [t (long (:tick world))]
    (when (>= (- t (long (:keepalive-at e t))) interval-ticks)
      (if (:keepalive-pending? e)
        (concat
          (sleep/vacated-deltas world eid e)
          [(out/to eid
                   (out/disconnect {:translate "disconnect.timeout"}))
           [:remove-entity eid]
           (out/to eid (out/close))])
        (let [m {:keepalive-at t :keepalive-pending? true}]
          [[:merge-entity eid m] (out/to eid (out/keepalive t))])))))
