(ns collider.game.systems.weather
  "Rain and thunder."
  (:require [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.world.env.weather :as weather]))

(set! *warn-on-reflection* true)

(defn- was-raining? [world]
  (> (double (:o-rain-level world 0.0)) 0.2))

(defn- rain-msg [w] (out/rain-level (weather/rain-level w)))

(defn- thunder-msg [w]
  (out/thunder-level (weather/raw-thunder-level w)))

(defn- level-messages [w]
  (concat
    (when (not= (double (:o-rain-level w 0.0)) (weather/rain-level w))
      [(out/all (rain-msg w))])
    (when (not= (double (:o-thunder-level w 0.0))
                (weather/raw-thunder-level w))
      [(out/all (thunder-msg w))])))

(defn- switch-messages [w]
  (let [was? (was-raining? w)]
    (when (not= was? (weather/raining? w))
      [(out/all (if was? (out/rain-stopped) (out/rain-started)))
       (out/everyone (rain-msg w))
       (out/everyone (thunder-msg w))])))

(defn- join-messages [world events]
  (when (weather/raining? world)
    (for [[tag eid] events
          :when (= :player-join tag)
          msg [(out/rain-started)
               (out/rain-level (weather/rain-level world))
               (out/thunder-level (weather/thunder-level world))]]
      (out/to eid msg))))

(defn weather
  "Returns the rain and thunder of a level that can have weather.
  The messages of the change come with them."
  {:wake :always}
  [world d]
  (deltas/of-vec
    (when (weather/can-have-weather? (:dim world))
      (let [w (weather/advance world)]
        (concat [[:advance-weather w]] (level-messages w)
                (switch-messages w)
                (join-messages w (player/joins d)))))))
