(ns collider.game.systems.daynight
  "The passing of the day."
  (:require [collider.world.env.dimension :as dimension]
            [collider.game.clock :as clock]
            [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(def send-interval 20)

(def ^:private sky-keyframes
  [[133 1.0] [11867 1.0] [13670 0.26666668] [22330 0.26666668]
   [24133 1.0]])

(defn- day-time ^long [time-of-day]
  (let [r (rem (long time-of-day) 24000)]
    (if (< r 133) (+ r 24000) r)))

(defn- around? [t [[a _] [b _]]]
  (and (<= (long a) (long t)) (< (long t) (long b))))

(defn- span-of [t]
  (first (filter #(around? t %) (partition 2 1 sky-keyframes))))

(defn dark? [time-of-day]
  (let [t (day-time time-of-day)
        [[t0 v0] [t1 v1]] (span-of t)
        m (+ v0 (* (- v1 v0) (/ (double (- t t0)) (- t1 t0))))]
    (>= (long (- 15.0 (* 15.0 m))) 4)))

(defn dark-outside? [world]
  (and (not (:has-fixed-time (dimension/type-of (:dim world))))
       (dark? (clock/day-ticks world))))

(defn- daynight-deltas [world]
  (when (zero? (rem (long (:tick world)) send-interval))
    [(out/all (out/time (long (:tick world)) {}))]))

(defn daynight
  {:wake {:every send-interval}}
  [world _d]
  [#(daynight-deltas world)])
