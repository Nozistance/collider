(ns collider.game.systems.weather
  "Rain and thunder."
  (:require [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.world.env.weather :as weather]))

(set! *warn-on-reflection* true)

(defn- was-raining? [world]
  (> (double (:o-rain-level world 0.0)) 0.2))

(defn- rain-level [w] (out/rain-level (weather/rain-level w)))

(defn- thunder-level [w]
  (out/thunder-level (weather/raw-thunder-level w)))

(defn- level-messages [w]
  (concat
    (when (not= (double (:o-rain-level w 0.0)) (weather/rain-level w))
      [(out/all (rain-level w))])
    (when (not= (double (:o-thunder-level w 0.0))
                (weather/raw-thunder-level w))
      [(out/all (thunder-level w))])))

(defn- switch-messages [w]
  (when (not= (was-raining? w) (weather/raining? w))
    [(out/all (if (was-raining? w)
                (out/rain-stopped)
                (out/rain-started)))
     (out/everyone (rain-level w))
     (out/everyone (thunder-level w))]))

(defn- join-messages [world events]
  (when (weather/raining? world)
    (for [[tag eid] events
          :when (= :player-join tag)
          msg [(out/rain-started)
               (out/rain-level (weather/rain-level world))
               (out/thunder-level (weather/thunder-level world))]]
      (out/to eid msg))))

(defn weather
  "Returns the rain and thunder of a level that can have weather:
  the step of the cycle, the messages of the change and the state
  a player placed in the level sees."
  [world d]
  (when (weather/can-have-weather? (:dim world))
    (let [w (merge world (weather/advance world))]
      (concat [[:advance-weather]] (level-messages w)
              (switch-messages w)
              (join-messages w (state/joins d))))))
