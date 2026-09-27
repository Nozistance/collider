(ns collider.world.rules
  "Registry of the block rules and their tick hooks."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.blocks.composter :as composter]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.grow.crop :as crop]
            [collider.world.blocks.leaves :as leaves]
            [collider.world.blocks.lectern :as lectern]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.support :as support]
            [collider.world.blocks.water :as water]))

(set! *warn-on-reflection* true)

(def rules [water/kelp-rule
            eyeblossom/rule
            liquid/rule
            liquid/column-rule
            fire/rule
            dripleaf/rule
            crop/attached-stem-rule
            water/coral-rule
            support/rule
            dripstone/rule
            dripstone/cauldron-rule
            support/falling-rule
            water/sponge-rule
            support/scaffold-rule
            composter/rule
            lectern/rule
            leaves/rule])

(defn- find-rule [st]
  (reduce (fn [_ r] (when ((:match? r) nil st nil) (reduced r)))
          nil rules))

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
  "Returns what the rule owning pos asks for on a change.
  That is the tick of a scheduled tick, one per block and type,
  or :neighbor for its reply at once, or nil.
  The change is at pos or beside it. old is the state before the
  change. side is the side of the change seen from pos, as :up,
  or nil when the change was at pos itself. dim names the
  dimension."
  [chunks dim st tick pos old side]
  (when-let [r (rule-for st)]
    ((:wake r) chunks dim tick pos old side)))

(defn update-pass
  "Returns when the rule owning st hears of a change beside it:
  :neighbor for neighborChanged, :shape for updateShape, nil
  when no rule owns it."
  [st]
  (when-let [r (rule-for st)]
    (let [p (:pass r :shape)]
      (if (keyword? p) p (p st)))))

(defn fluid-of
  "Returns the fluid of st whose ticks it takes, or nil."
  [st]
  (liquid/fluid-of st))

(defn- still?
  "Tells whether the water in st at pos waits for a change on
  side: TallSeagrassBlock asks for no fluid tick in updateShape,
  kelp none when it turns between head and body."
  [chunks pos st side]
  (and (some? side)
       (or (= :tall-seagrass (block/type-of st))
           (water/kelp-still? chunks pos st side))))

(defn fluid-wake-tick [chunks dim st tick pos old side]
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
  "Returns the changes the rule owning pos makes on a neighbour
  update, the updateShape or neighborChanged of vanilla. The
  :side of ctx is the side of the change seen from pos, nil for
  a change at pos itself."
  [chunks st pos ctx]
  (when-let [r (rule-for st)]
    (when-let [f (:reshape r)] (f chunks pos ctx))))

(def ^:const light-reach
  "How far across a change moves the light: a block light fades in
  15 steps, and so does the sky light that leaves a column."
  15)

(defn lit?
  "Tells whether the tick of the rule owning st reads light."
  [st ctx]
  (let [r (rule-for st)]
    (boolean (when-let [f (:lit? r)] (f st ctx)))))

(defn reach
  "Returns how far across the tick of the rule owning st reads.
  Every block it reads is at most that many columns away from its
  own, at any height; light it reads makes it light-reach more.
  A rule reads its own column and the next ones unless it says
  otherwise."
  ^long [st ctx]
  (if-let [r (rule-for st)]
    (+ (long (:reach r 1)) (if (lit? st ctx) light-reach 0))
    0))

(defn fluid-reach ^long [st ctx]
  (if (block/liquid-class st) (liquid/reach (:dim ctx)) 0))

(defn fluid-changes [chunks st pos ctx]
  (when (block/liquid-class st)
    (liquid/update-cell chunks pos ctx)))
