(ns collider.game.systems.camera
  "Spectators picking the entities they look through."
  (:require [collider.game.camera :as camera]
            [collider.game.deltas :as deltas]
            [collider.game.apply :as apply]))

(set! *warn-on-reflection* true)

(defn- event-deltas [world [tag eid tid]]
  (when (= :spectate tag) (camera/spectate-deltas world eid tid)))

(defn camera
  {:wake {:events #{:spectate}}}
  [world d]
  (deltas/of-vec (apply/fold-events world (:input d) event-deltas)))
