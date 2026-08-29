(ns collider.world.rules

  (:require [collider.world.fire :as fire]
            [collider.world.grass :as grass]
            [collider.world.liquid :as liquid]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(def rules [liquid/rule
   fire/rule
   support/rule
   grass/rule
   grass/smother-rule])

(defn- rule-for [chunks st pos]
  (reduce (fn [_ r] (when ((:match? r) chunks st pos) (reduced r))) nil rules))

(defn wake-tick [chunks st tick pos old self?]
  (when-let [r (rule-for chunks st pos)]
    ((:wake r) chunks tick pos old self?)))

(defn cell-changes [chunks st pos]
  (when-let [r (rule-for chunks st pos)]
    ((:due r) chunks pos)))
