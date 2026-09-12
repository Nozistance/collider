(ns collider.world.rules
  (:require [collider.world.blocks.composter :as composter]
            [collider.world.blocks.coral :as coral]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.blocks.falling :as falling]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.grass :as grass]
            [collider.world.blocks.kelp :as kelp]
            [collider.world.blocks.lectern :as lectern]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.scaffold :as scaffold]
            [collider.world.blocks.sponge :as sponge]
            [collider.world.blocks.support :as support]))

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
   grass/rule
   lectern/rule])

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
