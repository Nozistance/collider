(ns collider.world.rules

  (:require [collider.world.composter :as composter]
            [collider.world.coral :as coral]
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
   support/rule
   falling/rule
   sponge/rule
   coral/rule
   scaffold/rule
   composter/rule
   grass/rule
   grass/smother-rule])

(defn- rule-for [chunks st pos]
  (reduce (fn [_ r] (when ((:match? r) chunks st pos) (reduced r))) nil rules))

(defn wake-tick
  "Tick the first matching rule of the state wants, or nil."
  [chunks st tick pos old self?]
  (when-let [r (rule-for chunks st pos)]
    ((:wake r) chunks tick pos old self?)))

(defn again-tick
  "The tick at which the state rule wants to tick again without a block
   change (vanilla calls scheduleTick from tick), or nil."
  [chunks st pos tick]
  (when-let [r (rule-for chunks st pos)]
    (when-let [f (:again r)]
      (f chunks tick pos))))

(defn cell-changes
  "[[pos state] ...] from the first matching rule of the state; ctx carries
   what a rule may need beside the chunks: {:rules game-rules :tick t}."
  [chunks st pos ctx]
  (when-let [r (rule-for chunks st pos)]
    ((:due r) chunks pos ctx)))
