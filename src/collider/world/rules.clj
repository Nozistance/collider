(ns collider.world.rules

  (:require [collider.world.fire :as fire]
            [collider.world.grass :as grass]
            [collider.world.kelp :as kelp]
            [collider.world.liquid :as liquid]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(def rules [kelp/rule
   liquid/rule
   fire/rule
   support/rule
   grass/rule
   grass/smother-rule])

(defn- rule-for [chunks st pos]
  (reduce (fn [_ r] (when ((:match? r) chunks st pos) (reduced r))) nil rules))

(defn wake-tick
  "Tick the first matching rule of the state wants, or nil."
  [chunks st tick pos old self?]
  (when-let [r (rule-for chunks st pos)]
    ((:wake r) chunks tick pos old self?)))

(defn cell-changes
  "[[pos state] ...] from the first matching rule of the state; rules are the
   game rules of the world."
  [chunks st pos rules]
  (when-let [r (rule-for chunks st pos)]
    ((:due r) chunks pos rules)))
