(ns collider.game.systems.landing
  "Players who land, hurt by their fall as their move comes in,
  before the level ticks."
  (:require [collider.game.deltas :as deltas]
            [collider.game.turn.landing :as landing]))

(set! *warn-on-reflection* true)

(defn- landed-deltas [world {:keys [eid to landed]}]
  (when-let [e (get-in world [:entities eid])]
    (let [[_ ds] (landing/landed world eid e to nil landed landed)]
      (conj (vec ds) [:merge-entity eid {:landed nil}]))))

(defn landings
  "Returns the deltas of the players who landed in the moves of the
  tick."
  {:wake {:events #{:move}}}
  [world _d]
  (deltas/of-vec
    (into [] (comp (filter :landed) (mapcat #(landed-deltas world %)))
          (get-in world [:input :moves]))))
