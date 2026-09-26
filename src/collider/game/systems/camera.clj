(ns collider.game.systems.camera
  "Spectators looking through the eyes of other entities."
  (:require [collider.game.camera :as camera]
            [collider.game.state :as state]))

(set! *warn-on-reflection* true)

(defn- event-deltas [world [tag eid tid]]
  (when (= :spectate tag) (camera/spectate-deltas world eid tid)))

(defn- follow-deltas [world]
  (into [] (mapcat (fn [[eid e]] (camera/follow-deltas world eid e)))
        (state/player-entries world)))

(defn- camera-deltas [world events]
  (let [ds (state/fold-events world events event-deltas)
        w (state/apply-entities world ds)]
    (into ds (follow-deltas w))))

(defn camera
  "Returns a step for the spectator actions of this tick and for
  the spectators that follow their cameras."
  [world d]
  (let [events (:input d)]
    [#(camera-deltas world events)]))
