(ns collider.world.blocks.connect
  "Blocks that take their shape from their neighbours."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.campfire :as campfire]
            [collider.world.blocks.chest :as chest]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.halves :as halves]
            [collider.world.blocks.leaves :as leaves]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.mushroom :as mushroom]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn- tag [t] (set (get-in (data/tags) ["block" t])))

(def ^:private ^:table fences (delay (tag "fences")))

(def ^:private ^:table wooden (delay (tag "wooden_fences")))

(def ^:private ^:table walls (delay (tag "walls")))

(def ^:private ^:table leaf-blocks (delay (tag "leaves")))

(def ^:private ^:table shulker-boxes (delay (tag "shulker_boxes")))

(def ^:private never-connect
  #{:barrier :carved-pumpkin :jack-o-lantern :melon :pumpkin})

(def ^:private snowy-types #{:grass :mycelium :snowy-dirt})

(def ^:private bar-types
  #{:iron-bars :stained-glass-pane :weathering-copper-bar})

(def ^:private stair-types #{:stair :weathering-copper-stair})

(def placed-types
  "The block types that take their shape from neighbours when placed."
  (into #{:fence :wall :fence-gate :concrete-powder :chorus-plant
          :potent-sulfur :tripwire :note}
        (concat bar-types stair-types)))

(def ^:private ^:table connecting-set
  (delay
    (into #{:fence :wall :fence-gate :door :weathering-copper-door
            :bed :concrete-powder :vine :glow-lichen :multiface
            :sculk-vein :campfire :huge-mushroom :mossy-carpet
            :hanging-moss :pointed-dripstone :sulfur-spike
            :big-dripleaf :fire :soul-fire :chest :trapped-chest
            :copper-chest :weathering-copper-chest :chorus-plant
            :potent-sulfur :tripwire :note :pitcher-crop :bell}
          (concat bar-types stair-types halves/pair-types
                  block/growing-plant-types snowy-types
                  (block/leaves-types)))))

(defn connecting?
  "Returns true when st may change its shape with its neighbours."
  [^long st]
  (contains? @connecting-set (block/type-of st)))

(defn- never-connects? [b]
  (or (contains? @leaf-blocks b) (contains? never-connect b)
      (contains? @shulker-boxes b)))

(defn- sturdy? [st b side]
  (and (block/face-sturdy? st (dir/opposite side))
       (not (never-connects? b))))

(defn- gate-connects? [st side]
  (let [f (block/facing-of st)]
    (if (#{:north :south} side)
      (contains? #{:east :west} f)
      (contains? #{:north :south} f))))

(defn- fence-connects? [self nst side]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (contains? @fences n)
      (= (contains? @wooden n) (contains? @wooden self))
      (= :fence-gate t) (gate-connects? nst side)
      :else (sturdy? nst n side))))

(defn- wall-connects? [nst side]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (or (contains? @walls n) (contains? @fences n)
          (contains? bar-types t)) true
      (= :fence-gate t) (gate-connects? nst side)
      :else (sturdy? nst n side))))

(defn- pane-connects? [nst side]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (or (contains? bar-types t) (contains? @walls n)) true
      :else (sturdy? nst n side))))

(defn- connects? [t self nst side]
  (case t
    :fence (fence-connects? self nst side)
    :wall (wall-connects? nst side)
    (pane-connects? nst side)))

(defn- axis-low? [sides a b c d]
  (and (= :low (a sides)) (= :low (b sides))
       (= :none (c sides)) (= :none (d sides))))

(defn- wall-post? [sides]
  (not (or (axis-low? sides :north :south :east :west)
           (axis-low? sides :east :west :north :south))))

(defn- wall-at? [st] (contains? @walls (block/block-of st)))

(defn- gate-state [_self st at]
  (let [z? (#{:north :south} (block/facing-of st))
        [a b] (if z? [[-1 0 0] [1 0 0]] [[0 0 -1] [0 0 1]])
        in-wall? (or (wall-at? (at a)) (wall-at? (at b)))]
    (block/with st :in-wall (block/flag in-wall?))))

(def ^:private side-of
  (into {} (for [[k off] dir/offset] [off k])))

(defn- partner-side [^long st] (side-of (halves/partner-offset st)))

(defn- door-state [chunks pos ^long st sides]
  (let [lower? (= :lower (:half (block/props-of st)))
        [_ pst] (halves/partner chunks pos st)
        below (chunk/at chunks (dir/down pos))]
    (cond
      (and lower? (contains? sides :down)
           (not (block/face-sturdy? below :up))) (block/emptied st)
      (not (contains? sides (partner-side st))) st
      (nil? pst) (block/emptied st)
      lower? st
      :else (block/with pst :half :upper))))

(defn- bed-state [chunks pos ^long st sides]
  (let [[_ pst] (halves/partner chunks pos st)]
    (cond
      (not (contains? sides (partner-side st))) st
      (nil? pst) (block/emptied st)
      :else (block/with st :occupied
                        (:occupied (block/props-of pst))))))

(defn- pair-state [chunks pos ^long st sides]
  (if (and (contains? sides (partner-side st))
           (nil? (halves/partner chunks pos st)))
    (block/emptied st)
    st))

(defn- pitcher-state [chunks pos ^long st sides]
  (if (>= (block/prop-long st :age) 3)
    (pair-state chunks pos st sides)
    st))

(defn- source-if-fluid? [^long st]
  (or (nil? (block/liquid-class st))
      (block/waterlogged? st)
      (block/source-state? st)))

(defn- erupts? [^long below t]
  (and (block/tagged? below t) (source-if-fluid? below)))

(defn- sulfur-phase [st above below]
  (cond
    (not (block/holds-water-source? above)) :dry
    (erupts? below "causes_continuous_geyser_eruptions") :continuous
    (erupts? below "causes_periodic_geyser_eruptions")
    (if (= :erupting (:potent-sulfur-state (block/props-of st)))
      :erupting
      :dormant)
    :else :wet))

(defn- sulfur-state [_self ^long st at]
  (let [phase (sulfur-phase st (at [0 1 0]) (at [0 -1 0]))]
    (block/with st :potent-sulfur-state phase)))

(defn- stair? [st half]
  (and (= :stair (block/shape-of st))
       (= half (:half (block/props-of st)))))

(defn- can-take-shape? [st at side]
  (let [n (at (dir/horizontal-offset side))]
    (not (and (stair? n (:half (block/props-of st)))
              (= (block/facing-of n) (block/facing-of st))))))

(defn- turned? [n half facing]
  (and (stair? n half)
       (not= (dir/axis (block/facing-of n)) (dir/axis facing))))

(defn- stair-shape [st at facing half]
  (let [behind (at (dir/horizontal-offset facing))
        front (at (dir/horizontal-offset (dir/opposite facing)))
        bf (block/facing-of behind)
        ff (block/facing-of front)
        left (dir/counter-clockwise facing)]
    (cond
      (and (turned? behind half facing)
           (can-take-shape? st at (dir/opposite bf)))
      (if (= bf left) :outer_left :outer_right)
      (and (turned? front half facing) (can-take-shape? st at ff))
      (if (= ff left) :inner_left :inner_right)
      :else :straight)))

(defn- stair-state [_self st at]
  (let [{:keys [facing half]} (block/props-of st)]
    (block/with st :shape (stair-shape st at facing half))))

(defn- open-water? [at side]
  (let [n (at (dir/offset side))]
    (and (block/water? n)
         (not (block/face-sturdy? n (dir/opposite side))))))

(defn- powder-state [st at]
  (if (or (block/water? (at [0 0 0]))
          (some #(open-water? at %) [:up :north :south :west :east]))
    (block/concrete-of st)
    st))

(defn- growing-plant-state [pos st at tick sides]
  (let [{:keys [head body dir]}
        (block/growing-plant (block/type-of st))
        next (block/block-of (at (dir/offset dir)))
        on? (contains? #{head body} next)
        berries (:berries (block/props-of st))
        props (cond-> {} berries (assoc :berries berries))
        self (block/block-of st)]
    (cond
      (not (some #(contains? sides %) [nil dir])) st
      (and (= head self) on?) (block/state body props)
      (and (= body self) (not on?))
      (block/state head
                   (assoc props :age (support/plant-age tick pos)))
      :else st)))

(defn- leaves-state [chunks pos st sides]
  (if (contains? sides nil) (leaves/distance-state chunks pos st) st))

(defn- snowy-as [v]
  (fn [^long st]
    (if (contains? snowy-types (block/type-of st))
      (block/with st :snowy v)
      st)))

(defn- snow? [st] (block/tagged? st "snow"))

(def ^:private ^:table snowy-states
  (delay {:off (block/state-table :long (snowy-as :false))
          :on (block/state-table :long (snowy-as :true))
          :snow (block/state-table :boolean snow?)}))

(defn- snowy-state [chunks pos ^long st sides]
  (when (or (contains? sides nil) (contains? sides :up))
    (let [{:keys [off on snow]} @snowy-states
          above (chunk/at chunks (dir/up pos))
          ^longs to (if (aget ^booleans snow above) on off)
          new (aget to st)]
      (when (not= new st) new))))

(defn- side-value [t c]
  (if (= :wall t) (if c :low :none) (block/flag c)))

(defn- sides-state [t self ^long st at]
  (let [side (fn [[d off]]
               [d (side-value t (connects? t self (at off) d))])
        sides (into {} (map side) dir/horizontal-offset)
        post (block/flag (wall-post? sides))
        props (cond-> (merge (block/props-of st) sides)
                (= :wall t) (assoc :up post))]
    (block/state self props)))

(defn- wire-connects? [nst side]
  (case (block/type-of nst)
    :trip-wire-hook (= (block/facing-of nst) (dir/opposite side))
    :tripwire true
    false))

(defn- tripwire-state [self ^long st at]
  (let [side (fn [[d off]]
               [d (block/flag (wire-connects? (at off) d))])]
    (->> (into (block/props-of st) (map side) dir/horizontal-offset)
         (block/state self))))

(defn- instrument [^long st]
  (let [b (get (data/blocks) (block/block-of st))]
    [(:instrument b :harp) (:instrument-above? b)]))

(defn- note-state [_self ^long st at]
  (let [[up up?] (instrument (at [0 1 0]))
        [down down?] (instrument (at [0 -1 0]))]
    (block/with st :instrument (cond up? up down? :harp :else down))))

(defn- unless-bare ^long [^long st ^long new]
  (if (seq (block/faces-of new)) new st))

(defn- vine-reshaped [chunks pos st sides]
  (if (some #(not= :down %) sides)
    (unless-bare st (support/vine-updated chunks pos st))
    st))

(defn- multiface-reshaped [chunks pos st sides]
  (->> (support/multiface-sides-updated chunks pos st sides)
       (unless-bare st)))

(defn- leaf-reshaped [chunks pos st sides]
  (if (some #{nil :up} sides)
    (dripleaf/leaf-topped chunks pos st)
    st))

(defn- carpet-reshaped [chunks pos st _sides]
  (let [new (moss/carpet-reshaped chunks pos st)]
    (if (zero? new) st new)))

(defn- chorus-reshaped [chunks pos st sides]
  (let [full (block/props-of (chorus/connected chunks pos st))
        dirs (if (contains? sides nil) dir/six (filter sides dir/six))
        props (merge (block/props-of st) (select-keys full dirs))]
    (block/state (block/block-of st) props)))

(defn- fire-reshaped [chunks pos st]
  (if (support/supported? chunks pos st)
    (fire/state-with-age chunks pos (fire/age st))
    0))

(defn- soul-fire-reshaped [chunks pos st]
  (if (support/supported? chunks pos st) st 0))

(defn- bell-props
  [{:keys [attachment facing]} side nst]
  (cond
    (and (= :double_wall attachment)
         (not (block/face-sturdy? nst side)))
    {:attachment :single_wall :facing (dir/opposite side)}
    (and (= :single_wall attachment) (= side (dir/opposite facing))
         (block/face-sturdy? nst facing))
    {:attachment :double_wall}))

(defn- bell-reshaped [chunks pos st sides]
  (let [side (first (remove nil? sides))
        props (block/props-of st)]
    (if (and side (= (dir/axis side) (dir/axis (:facing props))))
      (let [nst (chunk/at chunks (dir/toward pos side))]
        (if-let [m (bell-props props side nst)]
          (block/state (block/block-of st) (merge props m))
          st))
      st)))

(def ^:private sided-reshapers
  {:door                    door-state
   :weathering-copper-door  door-state
   :bed                     bed-state
   :double-plant            pair-state
   :tall-flower             pair-state
   :tall-seagrass           pair-state
   :small-dripleaf          pair-state
   :pitcher-crop            pitcher-state
   :vine                    vine-reshaped
   :glow-lichen             multiface-reshaped
   :multiface               multiface-reshaped
   :sculk-vein              multiface-reshaped
   :big-dripleaf            leaf-reshaped
   :mossy-carpet            carpet-reshaped
   :chorus-plant            chorus-reshaped
   :bell                    bell-reshaped
   :mangrove-leaves          leaves-state
   :tinted-particle-leaves   leaves-state
   :untinted-particle-leaves leaves-state})

(def ^:private pos-reshapers
  {:chest                   chest/updated
   :trapped-chest           chest/updated
   :copper-chest            chest/updated
   :weathering-copper-chest chest/updated
   :hanging-moss            moss/hanging-tip
   :pointed-dripstone       dripstone/updated
   :sulfur-spike            dripstone/updated
   :fire                    fire-reshaped
   :soul-fire               soul-fire-reshaped})

(def ^:private self-reshapers
  {:campfire                 campfire/updated
   :huge-mushroom            mushroom/updated
   :potent-sulfur            sulfur-state
   :fence-gate               gate-state
   :stair                    stair-state
   :weathering-copper-stair  stair-state
   :tripwire                 tripwire-state
   :note                     note-state})

(defn- reshaped-state [t chunks pos st at tick sides]
  (let [self (block/block-of st)]
    (cond
      (sided-reshapers t) ((sided-reshapers t) chunks pos st sides)
      (pos-reshapers t) ((pos-reshapers t) chunks pos st)
      (self-reshapers t) ((self-reshapers t) self st at)
      (= :concrete-powder t) (powder-state st at)
      (contains? block/growing-plant-types t)
      (growing-plant-state pos st at tick sides)
      :else (sides-state t self st at))))

(defn reshape
  "Returns the new state of st at pos after a change on its sides, or
  nil when it keeps its shape. A nil side means a change at pos."
  ([chunks pos st tick] (reshape chunks pos st tick #{nil}))
  ([chunks pos st tick sides]
   (let [t (block/type-of st)]
     (if (contains? snowy-types t)
       (snowy-state chunks pos st sides)
       (when (contains? @connecting-set t)
         (let [at (fn [d] (chunk/at chunks (mapv + pos d)))
               new (reshaped-state t chunks pos st at tick sides)]
           (when (not= (long new) (long st)) new)))))))
