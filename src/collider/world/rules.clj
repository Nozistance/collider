(ns collider.world.rules
  "Registry of the block rules and their tick hooks."
  (:require [collider.data :as data]
            [collider.world.blocks.composter :as composter]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.grass :as grass]
            [collider.world.blocks.grow.crop :as crop]
            [collider.world.blocks.lectern :as lectern]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.support :as support]
            [collider.world.blocks.water :as water]))

(set! *warn-on-reflection* true)

(def rules [water/kelp-rule
            eyeblossom/rule
            liquid/rule
            fire/rule
            dripleaf/rule
            crop/attached-stem-rule
            support/rule
            dripstone/rule
            dripstone/cauldron-rule
            support/falling-rule
            water/sponge-rule
            water/coral-rule
            support/scaffold-rule
            composter/rule
            grass/rule
            lectern/rule])

(defn- find-rule [st]
  (reduce (fn [_ r] (when ((:match? r) nil st nil) (reduced r))) nil rules))

(def ^:private by-state
  (delay (object-array (data/block-state-count))))

(defn- rule-for [st]
  (let [^objects arr @by-state st (long st)]
    (when (< -1 st (alength arr))
      (let [r (aget arr st)]
        (if (nil? r)
          (let [r (find-rule st)] (aset arr st (or r false)) r)
          (if (false? r) nil r))))))

(defn wake-tick
  "Returns the deltas the rule owning pos makes when a block changes
  there or beside it, or nil when no rule owns pos. old is the state
  before the change. self? is true when the change was at pos
  itself."
  [chunks st tick pos old self?]
  (when-let [r (rule-for st)]
    ((:wake r) chunks tick pos old self?)))

(defn again-tick
  "Returns the tick at which the rule owning pos wants to run again
  after a tick that changed nothing, or nil when it is done."
  [chunks st pos tick]
  (when-let [r (rule-for st)]
    (when-let [f (:again r)]
      (f chunks tick pos))))

(defn cell-changes [chunks st pos ctx]
  (when-let [r (rule-for st)]
    ((:due r) chunks pos ctx)))
