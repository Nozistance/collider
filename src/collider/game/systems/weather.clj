(ns collider.game.systems.weather
  (:require [collider.game.out :as out]
            [collider.world.env.weather :as weather]))

(set! *warn-on-reflection* true)

(defn- was-raining? [world]
  (> (double (:o-rain-level world 0.0)) 0.2))

(defn- level-messages [world]
  (concat
   (when (not= (double (:o-rain-level world 0.0)) (weather/rain-level world))
     [(out/all (out/rain-level (weather/rain-level world)))])
   (when (not= (double (:o-thunder-level world 0.0)) (weather/raw-thunder-level world))
     [(out/all (out/thunder-level (weather/raw-thunder-level world)))])))

(defn- switch-messages [world]
  (when (not= (was-raining? world) (weather/raining? world))
    [(out/all (if (was-raining? world) (out/rain-stopped) (out/rain-started)))
     (out/all (out/rain-level (weather/rain-level world)))
     (out/all (out/thunder-level (weather/raw-thunder-level world)))]))

(defn- join-messages [world events]
  (when (weather/raining? world)
    (for [[tag eid] events
          :when (= :player-join tag)
          msg [(out/rain-started)
               (out/rain-level (weather/rain-level world))
               (out/thunder-level (weather/thunder-level world))]]
      (out/to eid msg))))

(defn advanced [world]
  (merge world (weather/advance world)))

(defn messages [world events]
  (concat (level-messages world) (switch-messages world) (join-messages world events)))
