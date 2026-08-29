(ns collider.game.systems.daynight
  (:require [collider.game.state :as state]
            [collider.proto.packets.play :as play]))

(set! *warn-on-reflection* true)

(def send-interval 20)
(defn- time-packet [world]
  (let [t (long (:time-of-day world 0))]
    {:packet/key ::play/time-update
     :world-age (long (:tick world))
     :time t}))

(defn- daynight-deltas [world events]
  (let [pkt (time-packet world)]
    (concat
     (when (zero? (rem (long (:tick world)) send-interval))
       (state/broadcast world pkt))
     (for [[tag eid] events :when (= :player-join tag)]
       [:send eid pkt]))))

(defn daynight [world events]
  [#(daynight-deltas world events)])
