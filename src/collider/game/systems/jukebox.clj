(ns collider.game.systems.jukebox
  (:require [collider.game.block.jukebox :as jukebox]
            [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(defn- playing [world]
  (for [[_ entries] (:block-entities world)
        [pos e] entries
        :when (and (= :jukebox (:kind e)) (:song e))]
    [pos e]))

(defn- stop-deltas [world [pos e]]
  (when (jukebox/finished? (:song e) (- (long (:tick world)) (long (:started e))))
    [[:set-block-entity pos (assoc e :song nil :started nil)]
     (out/all (out/level-event 1011 pos 0))]))

(defn jukebox-songs [world _events]
  [#(mapcat (partial stop-deltas world) (playing world))])
