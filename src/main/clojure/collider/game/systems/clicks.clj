(ns collider.game.systems.clicks
  "The clicks of players on mobs, before the level tick."
  (:require [collider.game.deltas :as deltas]
            [collider.game.mob.interact :as interact]))

(set! *warn-on-reflection* true)

(defn clicks
  "Returns the deltas of the answers of mobs to the clicks of players
  this tick, one click after another. What a click spawns ticks in
  the same tick."
  {:wake {:events #{:interact}}}
  [world d]
  (let [evs (:input d)]
    (deltas/of-vec
      (interact/clicks world evs (long (:tick world))))))
