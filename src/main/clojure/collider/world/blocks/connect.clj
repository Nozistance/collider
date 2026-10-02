(ns collider.world.blocks.connect
  "Blocks that take their shape from their neighbours."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.blocks.campfire :as campfire]
            [collider.world.blocks.chest :as chest]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.leaves :as leaves]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.mushroom :as mushroom]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn- tag [t] (set (get-in (data/tags) ["block" t])))

(def ^:private ^:table fences (delay (tag "fences")))

(def ^:private ^:table wooden (delay (tag "wooden_fences")))

(def ^:private ^:table walls (delay (tag "walls")))

(def ^:private ^:table leaves (delay (tag "leaves")))

(def ^:private ^:table shulker-boxes (delay (tag "shulker_boxes")))

(def ^:private exceptions
  #{:barrier :carved-pumpkin :jack-o-lantern :melon :pumpkin})

(def ^:private neighbours
  (conj (vec (vals dir/horizontal-offset)) [0 1 0] [0 -1 0]))

(def pair-types
  #{:double-plant :tall-flower :tall-seagrass :small-dripleaf})

(def snowy-types #{:grass :mycelium :snowy-dirt})

(def ^:private bar-types
  #{:iron-bars :stained-glass-pane :weathering-copper-bar})

(def ^:private stair-types #{:stair :weathering-copper-stair})

(def placed-types
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
          (concat bar-types stair-types pair-types
                  block/growing-plant-types snowy-types
                  (block/leaves-types)))))

(defn- connecting-types [] @connecting-set)

(defn connecting?
  "Returns true when st may change its shape with its neighbours."
  [^long st]
  (contains? (connecting-types) (block/type-of st)))

(def ^:private half-types
  (into block/door-types (conj pair-types :pitcher-crop)))

(defn- bed-offset [{:keys [part facing]}]
  (dir/horizontal-offset
    (if (= :foot part) facing (dir/opposite facing))))

(defn partner-offset
  "Returns the offset to the other half of the two-block st, or nil."
  [^long st]
  (let [{:keys [half] :as props} (block/props-of st)
        t (block/type-of st)]
    (cond
      (contains? half-types t) (if (= :lower half) [0 1 0] [0 -1 0])
      (= :bed t) (bed-offset props)
      (contains? chest/types t)
      (dir/offset (chest/connected-direction st)))))

(defn- paired? [^long st ^long other]
  (if (contains? chest/types (block/type-of st))
    (chest/paired? st other)
    (and (= (block/block-of st) (block/block-of other))
         (let [k (if (= :bed (block/type-of st)) :part :half)]
           (not= (k (block/props-of st))
                 (k (block/props-of other)))))))

(defn partner
  "Returns [pos state] of the other half of st at pos, or nil."
  [chunks pos ^long st]
  (when-let [off (partner-offset st)]
    (let [p (mapv + pos off) o (chunk/at chunks p)]
      (when (paired? st o) [p o]))))

(defn- exception? [n]
  (or (contains? @leaves n) (contains? exceptions n)
      (contains? @shulker-boxes n)))

(defn- sturdy? [st n dir]
  (and (block/face-sturdy? st (dir/opposite dir))
       (not (exception? n))))

(defn- gate-connects? [st dir]
  (let [f (block/facing-of st)]
    (if (#{:north :south} dir)
      (contains? #{:east :west} f)
      (contains? #{:north :south} f))))

(defn- fence-connects? [self nst dir]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (contains? @fences n)
      (= (contains? @wooden n) (contains? @wooden self))
      (= :fence-gate t) (gate-connects? nst dir)
      :else (sturdy? nst n dir))))

(defn- wall-connects? [nst dir]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (or (contains? @walls n) (contains? @fences n)
          (contains? bar-types t)) true
      (= :fence-gate t) (gate-connects? nst dir)
      :else (sturdy? nst n dir))))

(defn- pane-connects? [nst dir]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (or (contains? bar-types t) (contains? @walls n)) true
      :else (sturdy? nst n dir))))

(defn- connects? [t self nst dir]
  (case t
    :fence (fence-connects? self nst dir)
    :wall (wall-connects? nst dir)
    (pane-connects? nst dir)))

(defn- axis-low? [sides a b c d]
  (and (= :low (a sides)) (= :low (b sides))
       (= :none (c sides)) (= :none (d sides))))

(defn- wall-post? [sides]
  (not (or (axis-low? sides :north :south :east :west)
           (axis-low? sides :east :west :north :south))))

(defn- with-prop [self st k v]
  (block/state self (assoc (block/props-of st) k v)))

(defn- wall-at? [st] (contains? @walls (block/block-of st)))

(defn- gate-state [self st at]
  (let [z? (#{:north :south} (block/facing-of st))
        [a b] (if z? [[-1 0 0] [1 0 0]] [[0 0 -1] [0 0 1]])
        in-wall? (or (wall-at? (at a)) (wall-at? (at b)))]
    (with-prop self st :in-wall (if in-wall? :true :false))))

(defn- door-upper [pst]
  (block/state (block/block-of pst)
               (assoc (block/props-of pst) :half :upper)))

(def ^:private side-of
  (into {} (for [[k off] dir/offset] [off k])))

(defn- partner-side [^long st] (side-of (partner-offset st)))

(defn- door-state [chunks pos ^long st sides]
  (let [lower? (= :lower (:half (block/props-of st)))
        [_ pst] (partner chunks pos st)
        below (chunk/at chunks (dir/down pos))]
    (cond
      (and lower? (contains? sides :down)
           (not (block/face-sturdy? below :up))) 0
      (not (contains? sides (partner-side st))) st
      (nil? pst) 0
      lower? st
      :else (door-upper pst))))

(defn- bed-state [chunks pos ^long st sides]
  (let [[_ pst] (partner chunks pos st)
        occupied (:occupied (block/props-of (or pst 0)))]
    (cond
      (not (contains? sides (partner-side st))) st
      (nil? pst) 0
      :else (with-prop (block/block-of st) st :occupied occupied))))

(defn- pair-state [chunks pos ^long st sides]
  (if (and (contains? sides (partner-side st))
           (nil? (partner chunks pos st)))
    (block/emptied st)
    st))

(defn- pitcher-state [chunks pos ^long st sides]
  (if (>= (block/prop-long st :age) 3)
    (pair-state chunks pos st sides)
    st))

(defn- water-source-state? [^long st]
  (or (block/waterlogged? st)
      (block/water-source? st)))

(defn- source-if-fluid? [^long st]
  (or (nil? (block/liquid-class st))
      (block/waterlogged? st)
      (block/source-state? st)))

(defn- erupts? [below t]
  (and (block/tagged? (max 0 below) t) (source-if-fluid? below)))

(defn- sulfur-phase [st above below]
  (cond
    (not (water-source-state? above)) :dry
    (erupts? below "causes_continuous_geyser_eruptions") :continuous
    (erupts? below "causes_periodic_geyser_eruptions")
    (if (= :erupting (:potent-sulfur-state (block/props-of st)))
      :erupting
      :dormant)
    :else :wet))

(defn- sulfur-state [self ^long st at]
  (let [phase (sulfur-phase st (at [0 1 0]) (at [0 -1 0]))]
    (with-prop self st :potent-sulfur-state phase)))

(defn- stair? [st half]
  (and (= :stair (block/shape-of st))
       (= half (:half (block/props-of st)))))

(defn- can-take-shape? [st at dir]
  (let [n (at (dir/horizontal-offset dir))]
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

(defn- stair-state [self st at]
  (let [{:keys [facing half]} (block/props-of st)]
    (with-prop self st :shape (stair-shape st at facing half))))

(defn- water? [st] (or (block/water? st) (block/waterlogged? st)))

(defn- open-water? [at dir]
  (let [n (at (dir/offset dir))]
    (and (water? n) (not (block/face-sturdy? n (dir/opposite dir))))))

(defn- touches-water? [st at]
  (or (and (water? st) (water? (at (dir/offset :down))))
      (some #(open-water? at %) [:up :north :south :west :east])))

(defn- powder-state [st at]
  (if (or (water? (at [0 0 0])) (touches-water? (at [0 0 0]) at))
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
      (with-prop (block/block-of st) st :snowy v)
      st)))

(defn- snow? [st] (block/tagged? st "snow"))

(def ^:private ^:table snowy-states
  (delay {:off (block/state-table :long (snowy-as :false))
          :on (block/state-table :long (snowy-as :true))
          :snow (block/state-table :boolean snow?)}))

(defn- snowy-state [chunks [x y z] ^long st sides]
  (when (or (contains? sides nil) (contains? sides :up))
    (let [{:keys [off on snow]} @snowy-states
          above (chunk/at chunks [x (inc (long y)) z])
          ^longs to (if (aget ^booleans snow above) on off)
          new (aget to st)]
      (when (not= new st) new))))

(defn- side-value [t c]
  (if (= :wall t) (if c :low :none) (if c :true :false)))

(defn- sides-state [t self ^long st at]
  (let [side (fn [[dir off]]
               [dir (side-value t (connects? t self (at off) dir))])
        sides (into {} (map side) dir/horizontal-offset)
        post (if (wall-post? sides) :true :false)
        props (cond-> (merge (block/props-of st) sides)
                (= :wall t) (assoc :up post))]
    (block/state self props)))

(defn- wire-connects? [nst dir]
  (case (block/type-of nst)
    :trip-wire-hook (= (block/facing-of nst) (dir/opposite dir))
    :tripwire true
    false))

(defn- tripwire-state [self ^long st at]
  (let [side (fn [[dir off]]
               [dir (if (wire-connects? (at off) dir) :true :false)])]
    (->> (into (block/props-of st) (map side) dir/horizontal-offset)
         (block/state self))))

(defn- instrument [^long st]
  (let [b (get (data/blocks) (block/block-of st))]
    [(:instrument b :harp) (:instrument-above? b)]))

(defn- note-state [self ^long st at]
  (let [[up up?] (instrument (at [0 1 0]))
        [down down?] (instrument (at [0 -1 0]))
        i (cond up? up down? :harp :else down)]
    (with-prop self st :instrument i)))

(defn- kept ^long [^long st ^long new]
  (if (seq (block/faces-of new)) new st))

(defn- vine-reshaped [chunks pos st sides]
  (if (some #(not= :down %) sides)
    (kept st (support/vine-updated chunks pos st))
    st))

(defn- multiface-reshaped [chunks pos st sides]
  (kept st (support/multiface-sides-updated chunks pos st sides)))

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
      (let [nst (chunk/at chunks (mapv + pos (dir/offset side)))]
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

(def ^:private growing-reshaped
  #{:weeping-vines :weeping-vines-plant :twisting-vines
    :twisting-vines-plant :cave-vines :cave-vines-plant})

(defn- reshaped-state [t chunks pos st at tick sides]
  (let [self (block/block-of st)]
    (cond
      (sided-reshapers t) ((sided-reshapers t) chunks pos st sides)
      (pos-reshapers t) ((pos-reshapers t) chunks pos st)
      (self-reshapers t) ((self-reshapers t) self st at)
      (= :concrete-powder t) (powder-state st at)
      (contains? growing-reshaped t)
      (growing-plant-state pos st at tick sides)
      :else (sides-state t self st at))))

(defn- reshape-of [chunks pos st tick sides]
  (let [t (block/type-of st)]
    (if (contains? snowy-types t)
      (snowy-state chunks pos st sides)
      (when (contains? (connecting-types) t)
        (let [at (fn [d] (chunk/at chunks (mapv + pos d)))
              new (reshaped-state t chunks pos st at tick sides)]
          (when (not= (long new) (long st)) new))))))

(defn reshape
  "Returns the new state of st at pos after a change on its sides.
  Returns nil for none. A nil side stands for a change at pos itself."
  ([chunks pos st tick] (reshape-of chunks pos st tick #{nil}))
  ([chunks pos st tick sides] (reshape-of chunks pos st tick sides)))

(defn- hinge-balance ^long [at left right]
  (let [full (fn [off] (if (block/full-cube? (at off)) 1 0))]
    (+ (- (long (full left))) (- (long (full (mapv + left [0 1 0]))))
       (long (full right)) (long (full (mapv + right [0 1 0]))))))

(defn- lower-door? [st]
  (and (contains? block/door-types (block/type-of st))
       (= :lower (:half (block/props-of st)))))

(defn- cursor-hinge [facing ^double cx ^double cz]
  (let [[sx _ sz] (dir/horizontal-offset facing)]
    (if (and (or (>= (long sx) 0) (not (< cz 0.5)))
             (or (<= (long sx) 0) (not (> cz 0.5)))
             (or (>= (long sz) 0) (not (> cx 0.5)))
             (or (<= (long sz) 0) (not (< cx 0.5))))
      :left
      :right)))

(defn- forced-hinge [l r ^long balance]
  (cond
    (not (and (or (not l) r) (<= balance 0))) :right
    (not (and (or (not r) l) (>= balance 0))) :left))

(defn door-hinge
  "Returns :left or :right for a door placed at pos with that facing.
  The cursor coordinates are within the clicked face, in sixteenths."
  [chunks pos facing cursor-x cursor-z]
  (let [at (fn [d] (chunk/at chunks (mapv + pos d)))
        left (dir/horizontal-offset (dir/counter-clockwise facing))
        right (dir/horizontal-offset (dir/clockwise facing))
        [l r] (map (comp lower-door? at) [left right])]
    (or (forced-hinge l r (hinge-balance at left right))
        (cursor-hinge facing (/ (double cursor-x) 16.0)
                      (/ (double cursor-z) 16.0)))))

(defn around
  "Returns the cells next to the cell x y z."
  [[x y z]]
  (mapv (fn [[dx dy dz]]
          [(+ (long x) (long dx))
           (+ (long y) (long dy))
           (+ (long z) (long dz))])
        neighbours))

(defn- connecting-at
  [chunks [_ y _ :as p]]
  (when (chunk/in-range? y)
    (let [st (chunk/chunks-get-block chunks p)]
      (when (contains? (connecting-types) (block/type-of st)) st))))

(defn- bed-origin? [origin st p]
  (and (= :bed (block/type-of st)) (contains? @origin p)))

(defn- side-toward [d]
  (dir/opposite (side-of d)))

(defn- touched [positions]
  (let [beside (fn [p d] [(mapv + p d) (side-toward d)])
        reached #(cons [% nil] (map (partial beside %) neighbours))]
    (reduce (fn [[order sides] [q side]]
              [(if (contains? sides q) order (conj order q))
               (update sides q (fnil conj #{}) side)])
            [[] {}]
            (mapcat reached positions))))

(defn- reshape-step [chunks origin tick sides acc p]
  (let [st (connecting-at chunks p)]
    (if-let [new (when (and st (not (bed-origin? origin st p)))
                   (reshape chunks p st tick (sides p)))]
      (conj acc [p new])
      acc)))

(defn- reshaped [chunks positions tick]
  (let [origin (delay (set positions))
        [cells sides] (touched positions)]
    (reduce #(reshape-step chunks origin tick sides %1 %2)
            [] cells)))

(defn derived-changes
  "Returns the shape changes that the blocks at positions set off.
  They follow each other at most eight rounds deep."
  [chunks positions tick]
  (loop [chunks chunks positions positions acc [] n 0]
    (let [changes (reshaped chunks positions tick)]
      (if (or (empty? changes) (= n 8))
        acc
        (recur (chunk/chunks-set-blocks chunks changes)
               (map first changes)
               (into acc changes)
               (inc n))))))
