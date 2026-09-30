(ns collider.game.systems.jukebox
  "Jukeboxes and the end of their songs."
  (:require [collider.game.block.jukebox :as jukebox]
            [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(defn tick-deltas
  "Returns the deltas of one tick of the jukebox e at pos: the end
  of its song."
  [world [pos e]]
  (when (:song e)
    (let [age (- (long (:tick world)) (long (:started e)))]
      (when (jukebox/finished? (:song e) age)
        [[:set-block-entity pos (assoc e :song nil :started nil)]
         (out/all
           (out/level-event out/sound-stop-jukebox-song pos 0))]))))
