(ns collider.game.systems.daynight
  (:require [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(def send-interval 20)
(defn- time-msg [world]
  (out/time (long (:tick world)) (long (:time-of-day world 0))))

(defn- daynight-deltas [world events]
  (let [msg (time-msg world)]
    (concat
     (when (zero? (rem (long (:tick world)) send-interval))
       [(out/all msg)])
     (for [[tag eid] events :when (= :player-join tag)]
       (out/to eid msg)))))

(defn daynight [world events]
  [#(daynight-deltas world events)])
