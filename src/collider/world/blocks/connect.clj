(ns collider.world.blocks.connect
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.blocks.chest :as chest]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.fire :as fire]
            [collider.world.gen :as gen]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn- tag [t] (set (get-in @data/tags ["block" t])))
(def ^:private fences (delay (tag "fences")))
(def ^:private wooden (delay (tag "wooden_fences")))
(def ^:private walls (delay (tag "walls")))
(def ^:private leaves (delay (tag "leaves")))
(def ^:private exceptions #{:barrier :carved-pumpkin :jack-o-lantern :melon :pumpkin})
(def ^:private neighbours (conj (vec (vals dir/horizontal-offset)) [0 1 0] [0 -1 0]))
(def pair-types #{:double-plant :tall-flower :tall-seagrass :small-dripleaf})
(def snowy-types #{:grass :mycelium :snowy-dirt})
(def placed-types
  #{:fence :wall :iron-bars :stained-glass-pane :fence-gate :stair :concrete-powder :chorus-plant
    :potent-sulfur})
(def connecting-types
  (into #{:fence :wall :iron-bars :stained-glass-pane :fence-gate :door :weathering-copper-door :bed
          :stair :concrete-powder :vine :glow-lichen :multiface :sculk-vein
          :mossy-carpet :hanging-moss :pointed-dripstone :sulfur-spike :big-dripleaf :fire :soul-fire
          :chest :trapped-chest :copper-chest :weathering-copper-chest :chorus-plant :potent-sulfur}
        (concat pair-types block/growing-plant-types snowy-types block/leaves-types [:pitcher-crop])))

(def ^:private half-types (into block/door-types (conj pair-types :pitcher-crop)))

(defn partner-offset [^long st]
  (let [{:keys [half part facing]} (block/props-of st)
        t (block/type-of st)]
    (cond
      (contains? half-types t) (if (= :lower half) [0 1 0] [0 -1 0])
      (= :bed t) (dir/horizontal-offset (if (= :foot part) facing (dir/opposite facing)))
      (contains? chest/types t) (dir/offset (chest/connected-direction st)))))

(defn- paired? [^long st ^long other]
  (if (contains? chest/types (block/type-of st))
    (chest/paired? st other)
    (and (= (block/block-of st) (block/block-of other))
         (let [k (if (= :bed (block/type-of st)) :part :half)]
           (not= (k (block/props-of st)) (k (block/props-of other)))))))

(defn partner [chunks pos ^long st]
  (when-let [off (partner-offset st)]
    (let [p (mapv + pos off) o (gen/at chunks p)]
      (when (paired? st o) [p o]))))

(defn- exception? [n]
  (or (contains? @leaves n) (contains? exceptions n) (str/ends-with? (name n) "shulker-box")))

(defn- sturdy? [st n dir]
  (and (block/face-sturdy? st (dir/opposite dir)) (not (exception? n))))

(defn- gate-connects? [st dir]
  (let [f (block/facing-of st)]
    (if (#{:north :south} dir) (contains? #{:east :west} f) (contains? #{:north :south} f))))

(defn- fence-connects? [self nst dir]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (contains? @fences n) (= (contains? @wooden n) (contains? @wooden self))
      (= :fence-gate t) (gate-connects? nst dir)
      :else (sturdy? nst n dir))))

(defn- wall-connects? [nst dir]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (or (contains? @walls n) (contains? @fences n) (#{:iron-bars :stained-glass-pane} t)) true
      (= :fence-gate t) (gate-connects? nst dir)
      :else (sturdy? nst n dir))))

(defn- pane-connects? [nst dir]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (or (#{:iron-bars :stained-glass-pane} t) (contains? @walls n)) true
      :else (sturdy? nst n dir))))

(defn- connects? [t self nst dir]
  (case t
    :fence (fence-connects? self nst dir)
    :wall (wall-connects? nst dir)
    (pane-connects? nst dir)))

(defn- wall-post? [sides]
  (not (or (and (= :low (:north sides)) (= :low (:south sides)) (= :none (:east sides)) (= :none (:west sides)))
           (and (= :low (:east sides)) (= :low (:west sides)) (= :none (:north sides)) (= :none (:south sides))))))

(defn- wall-at? [st] (contains? @walls (block/block-of st)))
(defn- gate-state [self st at]
  (let [axis (if (#{:north :south} (block/facing-of st)) :z :x)
        in-wall? (if (= axis :z)
                   (or (wall-at? (at [-1 0 0])) (wall-at? (at [1 0 0])))
                   (or (wall-at? (at [0 0 -1])) (wall-at? (at [0 0 1]))))]
    (block/state self (assoc (block/props-of st) :in-wall (if in-wall? :true :false)))))

(defn- door-state [chunks pos ^long st]
  (let [lower? (= :lower (:half (block/props-of st)))
        [_ pst] (partner chunks pos st)]
    (cond
      (nil? pst) 0
      (and lower? (not (block/face-sturdy? (gen/at chunks (dir/down pos)) :up))) 0
      lower? st
      :else (block/state (block/block-of pst) (assoc (block/props-of pst) :half :upper)))))

(defn- bed-state [chunks pos ^long st]
  (if-let [[_ pst] (partner chunks pos st)]
    (block/state (block/block-of st)
                 (assoc (block/props-of st) :occupied (:occupied (block/props-of pst))))
    0))

(defn- water-source-state? [^long st]
  (or (block/waterlogged? st)
      (and (= :water (liquid/liquid-class st)) (liquid/source-state? st))))

(defn- source-if-fluid? [^long st]
  (or (nil? (liquid/liquid-class st))
      (block/waterlogged? st)
      (liquid/source-state? st)))

(defn- sulfur-state
  [self ^long st at]
  (let [above (at [0 1 0]) below (at [0 -1 0])
        state (cond
                (not (water-source-state? above)) :dry
                (and (block/tagged? (max 0 below) "causes_continuous_geyser_eruptions")
                     (source-if-fluid? below))
                :continuous
                (and (block/tagged? (max 0 below) "causes_periodic_geyser_eruptions")
                     (source-if-fluid? below))
                (if (= :erupting (:potent-sulfur-state (block/props-of st))) :erupting :dormant)
                :else :wet)]
    (block/state self (assoc (block/props-of st) :potent-sulfur-state state))))

(defn- stair? [st half]
  (and (= :stair (block/type-of st)) (= half (:half (block/props-of st)))))

(defn- can-take-shape? [st at dir]
  (let [n (at (dir/horizontal-offset dir))]
    (not (and (stair? n (:half (block/props-of st)))
              (= (block/facing-of n) (block/facing-of st))))))

(defn- stair-state [self st at]
  (let [props  (block/props-of st)
        facing (:facing props)
        half   (:half props)
        behind (at (dir/horizontal-offset facing))
        front  (at (dir/horizontal-offset (dir/opposite facing)))
        bf     (block/facing-of behind)
        ff     (block/facing-of front)
        left   (dir/counter-clockwise facing)
        shape  (cond
                 (and (stair? behind half) (not= (dir/axis bf) (dir/axis facing)) (can-take-shape? st at (dir/opposite bf)))
                 (if (= bf left) :outer_left :outer_right)
                 (and (stair? front half) (not= (dir/axis ff) (dir/axis facing)) (can-take-shape? st at ff))
                 (if (= ff left) :inner_left :inner_right)
                 :else :straight)]
    (block/state self (assoc props :shape shape))))

(defn- water? [st] (or (= :water (liquid/liquid-class st)) (block/waterlogged? st)))
(defn- touches-water? [st at]
  (or (and (water? st) (water? (at (dir/offset :down))))
      (some (fn [dir]
              (let [n (at (dir/offset dir))]
                (and (water? n) (not (block/face-sturdy? n (dir/opposite dir))))))
            [:up :north :south :west :east])))

(defn- powder-state [st at]
  (if (or (water? (at [0 0 0])) (touches-water? (at [0 0 0]) at))
    (block/concrete-of st)
    st))

(defn- pair-state [chunks pos ^long st]
  (if (partner chunks pos st) st (block/emptied st)))

(defn- pitcher-state [chunks pos ^long st]
  (if (>= (block/prop-long st :age) 3) (pair-state chunks pos st) st))

(defn- growing-plant-state [pos st at tick]
  (let [{:keys [head body dir]} (block/growing-plant (block/type-of st))
        on? (contains? #{head body} (block/block-of (at (dir/offset dir))))
        berries (:berries (block/props-of st))
        props (cond-> {} berries (assoc :berries berries))]
    (cond
      (and (= head (block/block-of st)) on?) (block/state body props)
      (and (= body (block/block-of st)) (not on?)) (block/state head (assoc props :age (support/plant-age tick pos)))
      :else st)))

(defn- leaf-distance ^long [^long st]
  (cond
    (block/tagged? st "prevents_nearby_leaf_decay") 0
    (block/leaves? st) (block/prop-long st :distance)
    :else 7))

(defn- leaves-state [self st at]
  (let [d (reduce (fn [d off] (min (long d) (inc (leaf-distance (at off))))) 7 neighbours)]
    (block/state self (assoc (block/props-of st) :distance (keyword (str d))))))

(defn- snowy-state [self st at]
  (block/state self (assoc (block/props-of st)
                           :snowy (if (block/tagged? (at [0 1 0]) "snow") :true :false))))

(defn reshape [chunks pos ^long st tick]
  (let [t (block/type-of st)]
    (when (contains? connecting-types t)
      (let [self  (block/block-of st)
            at    (fn [d] (gen/at chunks (mapv + pos d)))
            new   (case t
                    (:door :weathering-copper-door) (door-state chunks pos st)
                    :bed (bed-state chunks pos st)
                    :potent-sulfur (sulfur-state self st at)
                    :fence-gate (gate-state self st at)
                    :stair (stair-state self st at)
                    :concrete-powder (powder-state st at)
                    (:chest :trapped-chest :copper-chest :weathering-copper-chest) (chest/updated chunks pos st)
                    :mossy-carpet (moss/carpet-reshaped chunks pos st)
                    :hanging-moss (moss/hanging-tip chunks pos st)
                    (:pointed-dripstone :sulfur-spike) (dripstone/updated chunks pos st)
                    :vine (support/vine-updated chunks gen/flat-chunk pos st)
                    (:glow-lichen :multiface :sculk-vein) (support/multiface-updated chunks gen/flat-chunk pos st)
                    (:double-plant :tall-flower :tall-seagrass) (pair-state chunks pos st)
                    :big-dripleaf (dripleaf/leaf-updated chunks pos st)
                    :small-dripleaf (dripleaf/small-updated chunks pos st)
                    :pitcher-crop (pitcher-state chunks pos st)
                    :fire (if (support/supported? chunks gen/flat-chunk pos st)
                            (fire/state-with-age chunks pos (fire/age st))
                            0)
                    :soul-fire (if (support/supported? chunks gen/flat-chunk pos st) st 0)
                    (:grass :mycelium :snowy-dirt) (snowy-state self st at)
                    (:mangrove-leaves :tinted-particle-leaves :untinted-particle-leaves) (leaves-state self st at)
                    :chorus-plant (chorus/connected chunks pos st)
                    (:weeping-vines :weeping-vines-plant :twisting-vines :twisting-vines-plant :cave-vines :cave-vines-plant)
                    (growing-plant-state pos st at tick)
                    (let [sides (into {} (map (fn [[dir off]]
                                                (let [c (connects? t self (at off) dir)]
                                                  [dir (if (= :wall t) (if c :low :none) (if c :true :false))])))
                                      dir/horizontal-offset)
                          props (cond-> (merge (block/props-of st) sides)
                                  (= :wall t) (assoc :up (if (wall-post? sides) :true :false)))]
                      (block/state self props)))]
        (when (not= (long new) st) new)))))

(defn door-hinge [chunks pos facing cursor-x cursor-z]
  (let [at (fn [d] (gen/at chunks (mapv + pos d)))
        left (dir/horizontal-offset (dir/counter-clockwise facing))
        right (dir/horizontal-offset (dir/clockwise facing))
        full (fn [off] (if (block/full-cube? (at off)) 1 0))
        balance (+ (- (full left)) (- (full (mapv + left [0 1 0]))) (full right) (full (mapv + right [0 1 0])))
        lower-door? (fn [st] (and (contains? block/door-types (block/type-of st)) (= :lower (:half (block/props-of st)))))
        door-left (lower-door? (at left))
        door-right (lower-door? (at right))
        [sx _ sz] (dir/horizontal-offset facing)
        cx (/ (double cursor-x) 16.0) cz (/ (double cursor-z) 16.0)]
    (cond
      (not (and (or (not door-left) door-right) (<= balance 0))) :right
      (not (and (or (not door-right) door-left) (>= balance 0))) :left
      (and (or (>= (long sx) 0) (not (< cz 0.5)))
           (or (<= (long sx) 0) (not (> cz 0.5)))
           (or (>= (long sz) 0) (not (> cx 0.5)))
           (or (<= (long sz) 0) (not (< cx 0.5)))) :left
      :else :right)))

(defn around [[x y z]]
  (map (fn [[dx dy dz]]
         [(+ (long x) (long dx))
          (+ (long y) (long dy))
          (+ (long z) (long dz))])
       neighbours))

(defn- reshaped [chunks positions tick]
  (let [origin (set positions)]
    (into []
          (keep (fn [[_ y _ :as p]]
                  (when (chunk/in-range? y)
                    (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
                      (when-not (and (contains? origin p) (= :bed (block/type-of st)))
                        (when-let [new (reshape chunks p st tick)]
                          [p new]))))))
          (distinct (concat positions (mapcat around positions))))))

(defn derived-changes [chunks positions tick]
  (loop [chunks chunks positions positions acc [] n 0]
    (let [changes (reshaped chunks positions tick)]
      (if (or (empty? changes) (= n 8))
        acc
        (recur (chunk/chunks-set-blocks chunks gen/flat-chunk changes)
               (map first changes)
               (into acc changes)
               (inc n))))))
