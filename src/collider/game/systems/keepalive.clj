(ns collider.game.systems.keepalive
  "Keepalive pings and timeouts."
  (:require [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.sleep :as sleep]))

(set! *warn-on-reflection* true)

(def interval-ticks 300)

(defn- player-deltas [world ^long t [eid e]]
  (when (>= (- t (long (:keepalive-at e t))) interval-ticks)
    (if (:keepalive-pending? e)
      (concat
        (sleep/vacated-deltas world e)
        [(out/to eid
                 (out/disconnect {:translate "disconnect.timeout"}))
         [:remove-entity eid]
         (out/to eid (out/close))])
      [[:merge-entity eid {:keepalive-at t :keepalive-pending? true}]
       (out/to eid (out/keepalive t))])))

(defn- keepalive-deltas [world _events]
  (let [t (long (:tick world))]
    (into [] (mapcat #(player-deltas world t %))
          (state/player-entries world))))

(defn keepalive
  {:wake {:types #{:player}}}
  [world d]
  (let [events (:input d)]
    (deltas/of-vec (keepalive-deltas world events))))
