(ns collider.world.rules
  "Registry of the block rules and their tick hooks."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.blocks.composter :as composter]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.blocks.fall :as fall]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.leaves :as leaves]
            [collider.world.blocks.lectern :as lectern]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.rail :as rail]
            [collider.world.blocks.scaffold :as scaffold]
            [collider.world.blocks.support :as support]
            [collider.world.blocks.water :as water]))

(set! *warn-on-reflection* true)

(def rules
  "The block rules in order. The first that matches a state owns it."
  [water/kelp-rule
   eyeblossom/rule
   liquid/rule
   liquid/column-rule
   fire/rule
   dripleaf/rule
   support/attached-stem-rule
   water/coral-rule
   rail/rule
   support/rule
   dripstone/rule
   dripstone/cauldron-rule
   fall/falling-rule
   water/sponge-rule
   scaffold/rule
   composter/rule
   lectern/rule
   leaves/rule])

(defn- find-rule [st]
  (some (fn [r] (when ((:match? r) nil st nil) r)) rules))

(def ^:private ^:table by-state
  (delay (let [n (data/block-state-count)]
           (object-array (map find-rule (range n))))))

(defn- rule-for [st]
  (let [^objects arr @by-state st (long st)]
    (when (< -1 st (alength arr))
      (aget arr st))))

(defn wake-tick
  "Returns the tick that the rule of st asks for after a change,
  :neighbor for a reply at once, or nil. The side is the side of the
  change seen from pos, nil at pos."
  [chunks dim st tick pos old side]
  (when-let [r (rule-for st)]
    ((:wake r) chunks dim tick pos old side)))

(defn update-pass
  "Returns :neighbor or :shape for the pass in which the rule owning
  st hears of a change beside it. Returns nil when no rule owns st."
  [st]
  (when-let [r (rule-for st)]
    (let [p (:pass r :shape)]
      (if (keyword? p) p (p st)))))

(defn fluid-of
  "Returns the fluid of st whose ticks it takes, or nil."
  [st]
  (liquid/fluid-of st))

(defn- still? [chunks pos st side]
  (and (some? side)
       (case (block/type-of st)
         :tall-seagrass true
         (:kelp :kelp-plant) (water/kelp-still? chunks pos st side)
         false)))

(defn fluid-wake-tick
  "Returns the tick the fluid of st asks for after a change, or nil."
  [chunks dim st tick pos old side]
  (when (and (block/liquid-class st)
             (not (still? chunks pos st side)))
    (liquid/fluid-wake chunks dim tick pos old side)))

(defn again-tick
  "Returns the next tick the rule owning pos wants.
  It is asked after a tick that changed nothing. Returns nil
  when the rule is done."
  [chunks st pos tick]
  (when-let [r (rule-for st)]
    (when-let [f (:again r)]
      (f chunks tick pos))))

(defn cell-changes
  "Returns the changes the rule owning pos makes on its tick."
  [chunks st pos ctx]
  (when-let [r (rule-for st)]
    (when-let [f (:due r)] (f chunks pos ctx))))

(defn reshape-changes
  "Returns the changes that the rule owning pos makes on a neighbour
  update. The :side of ctx is the side of the change seen from pos, or
  nil for a change at pos itself."
  [chunks st pos ctx]
  (when-let [r (rule-for st)]
    (when-let [f (:reshape r)] (f chunks pos ctx))))

(def ^:const light-reach
  "How far a change moves light across.
  Block light fades in 15 steps and so does sky light that leaves
  a column."
  15)

(defn lit?
  "Returns true when the tick of the rule owning st reads light."
  [st ctx]
  (let [r (rule-for st)]
    (boolean (when-let [f (:lit? r)] (f st ctx)))))

(defn reach
  "Returns how many columns away the tick of the rule owning st reads,
  at any height. Reading light adds the reach of light. A rule reads
  its own column and the next ones unless it says otherwise."
  ^long [st ctx]
  (if-let [r (rule-for st)]
    (+ (long (:reach r 1)) (if (lit? st ctx) light-reach 0))
    0))

(defn fluid-reach
  "Returns how many columns away the tick of the fluid of st reads."
  ^long [st ctx]
  (if (block/liquid-class st) (liquid/reach (:dim ctx)) 0))

(defn fluid-changes
  "Returns the changes the fluid of st makes on its tick at pos."
  [chunks st pos ctx]
  (when (block/liquid-class st)
    (liquid/update-cell chunks pos ctx)))
