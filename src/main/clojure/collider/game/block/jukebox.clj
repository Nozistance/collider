(ns collider.game.block.jukebox
  "Jukeboxes and the songs of music discs."
  (:require [collider.data :as data]
            [collider.num :as num]
            [collider.game.out :as out]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(def ^:private ^:table seconds
  (delay (into {} (map (fn [[id v]]
                         [(data/kebab id) (get v "length_in_seconds")]))
               (data/pack "jukebox_song"))))

(defn song-of [item]
  (data/jukebox-song item))

(defn length ^long [song]
  (let [s (float (get @seconds song 0.0))]
    (long (Math/ceil (num/f32 (* s 20.0))))))

(defn finished? [song ^long ticks]
  (>= ticks (+ (length song) 20)))

(defn song-id ^long [song]
  (data/datapack-id "jukebox_song" song))

(defn stopped
  "Returns the deltas of the song of jukebox e at pos stopping, none
  when it plays none."
  [pos e]
  (when (:song e)
    (let [stop (out/level-event out/sound-stop-jukebox-song pos 0)]
      (conj (signal/game-event :jukebox-stop-play pos nil)
            (out/all stop)))))
