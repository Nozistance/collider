(ns collider.game.systems.daynight
  (:require [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(def send-interval 20)
(def ^:private sky-keyframes
  [[133 1.0] [11867 1.0] [13670 0.26666668] [22330 0.26666668] [24133 1.0]])

(defn dark? [time-of-day]
  (let [t (let [r (rem (long time-of-day) 24000)] (if (< r 133) (+ r 24000) r))
        [[t0 v0] [t1 v1]] (first (filter (fn [[[a _] [b _]]] (and (<= a t) (< t b))) (partition 2 1 sky-keyframes)))
        m (+ v0 (* (- v1 v0) (/ (double (- t t0)) (- t1 t0))))]
    (>= (long (- 15.0 (* 15.0 m))) 4)))

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
