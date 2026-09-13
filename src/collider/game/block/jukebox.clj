(ns collider.game.block.jukebox
  "Jukeboxes: the songs music discs play."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(def ^:private seconds
  {:13        178 :cat 185 :blocks 345 :chirp 185 :far 174 :mall 197 :mellohi 96 :stal 150
   :strad     188 :ward 251 :11 71 :wait 238 :pigstep 149 :otherside 195 :5 178 :relic 218
   :precipice 299 :creator 176 :creator-music-box 73 :tears 175 :lava-chicken 134 :bounce 234})

(defn song-of
  "Returns the song a music disc plays, or nil."
  [item]
  (data/jukebox-song item))

(defn length
  "Returns how long a song plays, in ticks."
  ^long [song]
  (long (Math/ceil (* 20.0 (double (get seconds song 0))))))

(defn finished?
  "Returns true when a song has played to its end."
  [song ^long ticks]
  (>= ticks (length song)))

(defn song-id
  "Returns the id of a song."
  ^long [song]
  (data/datapack-id "jukebox_song" song))
