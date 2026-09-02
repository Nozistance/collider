(ns collider.game.systems.keepalive
  (:require [collider.game.out :as out]
            [collider.game.state :as state]))

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
                 [(out/to eid (out/keepalive (bit-and t 0xFFFFF)))])))
            (state/player-entries world)))))

(defn keepalive [world events]
  [#(keepalive-deltas world events)])
