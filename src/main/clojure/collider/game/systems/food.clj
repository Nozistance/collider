(ns collider.game.systems.food
  "Natural regeneration and the food of players, ticked in their turn,
  and the health packet at its end."
  (:require [collider.game.entity :as entity]
            [collider.game.food :as food]
            [collider.game.out :as out]
            [collider.world.env.difficulty :as difficulty]))

(set! *warn-on-reflection* true)

(def ^:private food-keys
  [:health :food :saturation :exhaustion :food-timer])

(defn- regen? [world]
  (get-in world [:rules :natural-health-regeneration] true))

(defn- change-deltas [eid e e']
  (let [m (into {} (remove (fn [[k v]] (= v (get e k))))
                (select-keys e' food-keys))]
    (when (seq m)
      [[:merge-entity eid
        (cond-> m (:health m) (assoc :health-sent (:health m)))]])))

(defn player-regeneration
  "Returns the deltas of the natural regeneration of the player entry
  p in a peaceful world."
  [world [eid e]]
  (let [lived (entity/tick-count world e)
        peaceful? (zero? (difficulty/id world))]
    (change-deltas eid e (food/regenerated e lived peaceful?
                                           (regen? world)))))

(defn- hud-deltas [eid e]
  (let [hud (food/hud e)]
    (when (not= hud (:hud-sent e))
      [(out/to eid (out/health (first hud) (second hud)
                               (food/saturation e)))
       [:merge-entity eid {:hud-sent hud}]])))

(defn player-deltas
  "Returns the deltas of one tick of the food of the player entry p,
  and of the health packet when its health or food changed."
  [world [eid e]]
  (let [e' (food/ticked e (regen? world))]
    (concat (change-deltas eid e e') (hud-deltas eid e'))))
