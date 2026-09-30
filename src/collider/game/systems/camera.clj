(ns collider.game.systems.camera
  "Spectators looking through the eyes of other entities."
  (:require [collider.game.camera :as camera]
            [collider.game.deltas :as deltas]
            [collider.game.apply :as apply]
            [collider.game.level :as level]))

(set! *warn-on-reflection* true)

(defn- event-deltas [world [tag eid tid]]
  (when (= :spectate tag) (camera/spectate-deltas world eid tid)))

(defn- follow-deltas [world]
  (into [] (mapcat (fn [[eid e]] (camera/follow-deltas world eid e)))
        (level/player-entries world)))

(defn- camera-deltas [world events]
  (let [ds (apply/fold-events world events event-deltas)
        w (apply/entities world ds)]
    (into ds (follow-deltas w))))

(defn camera
  "Returns the deltas of the spectator actions of this tick and of the
  spectators that follow their cameras."
  {:wake {:types #{:player} :events #{:spectate}}}
  [world d]
  (let [events (:input d)]
    (deltas/of-vec (camera-deltas world events))))
