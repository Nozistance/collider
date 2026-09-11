(ns collider.world.rules
  (:require [collider.world.composter :as composter]
            [collider.world.coral :as coral]
            [collider.world.dripleaf :as dripleaf]
            [collider.world.dripstone :as dripstone]
            [collider.world.eyeblossom :as eyeblossom]
            [collider.world.falling :as falling]
            [collider.world.fire :as fire]
            [collider.world.grass :as grass]
            [collider.world.kelp :as kelp]
            [collider.world.liquid :as liquid]
            [collider.world.scaffold :as scaffold]
            [collider.world.sponge :as sponge]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(def rules [kelp/rule
   eyeblossom/rule
   liquid/rule
   fire/rule
   dripleaf/rule
   support/rule
   dripstone/rule
   dripstone/cauldron-rule
   falling/rule
   sponge/rule
   coral/rule
   scaffold/rule
   composter/rule
   grass/rule])

(defn- rule-for [chunks st pos]
  (reduce (fn [_ r] (when ((:match? r) chunks st pos) (reduced r))) nil rules))

(defn wake-tick [chunks st tick pos old self?]
  (when-let [r (rule-for chunks st pos)]
    ((:wake r) chunks tick pos old self?)))

(defn again-tick [chunks st pos tick]
  (when-let [r (rule-for chunks st pos)]
    (when-let [f (:again r)]
      (f chunks tick pos))))

(defn cell-changes [chunks st pos ctx]
  (when-let [r (rule-for chunks st pos)]
    ((:due r) chunks pos ctx)))
