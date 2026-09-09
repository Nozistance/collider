(ns collider.game.systems.keepalive
  (:require [collider.game.out :as out]
            [collider.game.state :as state]))

(set! *warn-on-reflection* true)

(def interval-ticks 300)
(defn- player-deltas [^long t [eid e]]
  (when (>= (- t (long (:keepalive-at e t))) interval-ticks)
    (if (:keepalive-pending? e)
      [(out/to eid (out/disconnect {:translate "disconnect.timeout"}))
       [:remove-entity eid]
       (out/to eid (out/close))]
      [[:merge-entity eid {:keepalive-at t :keepalive-pending? true}]
       (out/to eid (out/keepalive t))])))

(defn- keepalive-deltas [world _events]
  (let [t (long (:tick world))]
    (into [] (mapcat #(player-deltas t %)) (state/player-entries world))))

(defn keepalive [world events]
  [#(keepalive-deltas world events)])
