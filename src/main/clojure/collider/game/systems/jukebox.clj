(ns collider.game.systems.jukebox
  "Jukeboxes and their songs as they play."
  (:require [collider.game.block.jukebox :as jukebox]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn- note [[x y z :as pos] t]
  (let [n (long (* 4.0 (double (random/of-key t pos :note))))
        color (double (float (/ n 24.0)))
        at [(+ x 0.5) (+ y (double (float 1.2))) (+ z 0.5)]]
    (out/all
      (out/particles :note nil at 0 1.0 [color 0.0 0.0]))))

(defn tick-deltas
  "Returns the deltas of one tick of the jukebox e at pos. A playing
  song shows a note every twenty ticks and ends when it is over."
  [world [pos e]]
  (when-let [song (:song e)]
    (let [t (long (:tick world))
          age (- t (long (:started e)))]
      (cond
        (jukebox/finished? song age)
        (into [[:set-block-entity pos
                (assoc e :song nil :started nil)]]
              (jukebox/stopped pos e))
        (zero? (rem age 20))
        (conj (signal/game-event :jukebox-play pos nil)
              (note pos t))))))
