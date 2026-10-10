(ns collider.game.systems.landing
  "Players who land, hurt by their fall as their move comes in,
  before the level ticks."
  (:require [collider.game.deltas :as deltas]
            [collider.game.turn.landing :as landing]))

(set! *warn-on-reflection* true)

(defn- landed-deltas [world e {:keys [eid to landed]}]
  (let [[_ ds] (landing/landed world eid e to nil landed landed)]
    (conj (vec ds) [:merge-entity eid {:landed nil}])))

(defn- move-deltas [world {:keys [eid landed] :as m}]
  (when-let [e (get-in world [:entities eid])]
    (concat (when landed (landed-deltas world e m))
            (landing/settled-deltas world eid e))))

(defn landings
  "Returns the deltas of the players who landed in the moves of the
  tick, and of those whose move ends what a gust started."
  {:wake {:events #{:move}}}
  [world _d]
  (deltas/of-vec
    (into [] (mapcat #(move-deltas world %))
          (get-in world [:input :moves]))))
