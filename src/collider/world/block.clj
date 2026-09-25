(ns collider.world.block
  "Block states and their placement."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.direction :as dir])
  (:import (java.util Arrays)))

(set! *warn-on-reflection* true)

(def ^:const air 0)

(defn state
  "Returns the global state id of block with props.
  Without props it is the id of the default state."
  (^long [block] (data/state-id block))
  (^long [block props] (data/state-id block props)))

(defn name-of [^long st] (data/state-block st))

(defn props-of [^long st] (second (data/state-props st)))

(defn prop-long ^long [^long st k]
  (Long/parseLong (name (get (props-of st) k :0))))

(defn- block-table [f]
  (let [a (object-array (data/block-state-count))]
    (doseq [[block b] (data/blocks)
            :let [v (f block b)]
            :when (some? v)
            i (range (reduce * 1 (map count (vals (:props b)))))]
      (aset a (+ (long (:first b)) (long i)) v))
    a))

(def ^:private ^:table type-arr
  (delay (block-table (fn [_ b] (:type b)))))

(def ^:private ^:table name-arr
  (delay (block-table (fn [block _] block))))

(defn- known? [^long st] (< -1 st (data/block-state-count)))

(defn type-of [^long st]
  (when (known? st) (aget ^objects @type-arr st)))

(defn block-of [^long st]
  (when (known? st) (aget ^objects @name-arr st)))

(defn- clone-table []
  (let [^objects a (block-table (fn [block b] (get b :clone block)))]
    (doseq [[_ b] (data/blocks)
            [st item] (:clones b)]
      (aset a (long st) item))
    a))

(def ^:private ^:table clone-arr (delay (clone-table)))

(defn clone-of [^long st]
  (when (known? st)
    (let [item (aget ^objects @clone-arr st)]
      (when-not (= :air item) item))))

(defn clone-props [^long st include-data]
  (let [b (get (data/blocks) (block-of st))]
    (cond-> (:clone-props b)
      include-data (into (:data-props b)))))

(defn prop-name [k] (str/replace (name k) "-" "_"))

(def door-types #{:door :weathering-copper-door})

(def trapdoor-types #{:trapdoor :weathering-copper-trapdoor})

(def torch-types #{:torch :redstone-torch})

(def wall-torch-types #{:wall-torch :redstone-wall-torch})

(def side-types
  #{:ladder :wall-sign :wall-hanging-sign :wall-banner
    :coral-wall-fan :base-coral-wall-fan})

(def ground-types
  #{:sapling :powered-rail :detector-rail :rail :tall-grass
    :double-plant :dry-vegetation :short-dry-grass :tall-dry-grass
    :flower :mushroom :fire :soul-fire :redstone-wire :crop
    :carrot :potato :beetroot :stem :attached-stem :standing-sign
    :pressure-plate :weighted-pressure-plate :snow-layer
    :wool-carpet :carpet :bush :sugar-cane :nether-wart
    :torchflower-crop :pitcher-crop :lily-pad :flower-bed
    :leaf-litter :eyeblossom :firefly-bush :kelp :kelp-plant
    :seagrass :tall-seagrass})

(def water-holder-types
  #{:kelp :kelp-plant :seagrass :tall-seagrass :bubble-column})

(def falling-types
  #{:sand :colored-falling :concrete-powder :anvil :scaffolding
    :pointed-dripstone :sulfur-spike :dragon-egg})

(def coral-types #{:coral :coral-plant :coral-fan :coral-wall-fan})

(def cauldron-types #{:cauldron :layered-cauldron :lava-cauldron})

(def multiface-types #{:glow-lichen :multiface :sculk-vein})

(def ^:private ^:table leaves-set
  (delay (into #{}
               (map (fn [b] (:type (get (data/blocks) b))))
               (data/tag-values "block" "leaves"))))

(defn leaves-types [] @leaves-set)

(def growing-plant
  (into {}
        (for [[head body dir]
              [[:weeping-vines :weeping-vines-plant :down]
               [:twisting-vines :twisting-vines-plant :up]
               [:cave-vines :cave-vines-plant :down]]
              t [head body]]
          [t {:head head :body body :dir dir}])))

(def growing-plant-types (set (keys growing-plant)))

(def needs-support-types
  (into ground-types
        (concat torch-types
                wall-torch-types
                side-types
                #{:ceiling-hanging-sign :tall-flower :cactus
                  :cactus-flower :bamboo-sapling :bamboo-stalk
                  :sweet-berry-bush :banner :spore-blossom
                  :hanging-roots :coral-plant :coral-fan
                  :coral-wall-fan :base-coral-plant
                  :base-coral-fan :base-coral-wall-fan :vine}
                multiface-types
                growing-plant-types)))

(def attached-types
  #{:lantern :weathering-lantern :bell :farmland :dirt-path
    :candle :sea-pickle :cocoa :cake :candle-cake :button :lever
    :amethyst-cluster :hanging-moss :big-dripleaf
    :big-dripleaf-stem :small-dripleaf :azalea :wither-rose
    :nether-sprouts :nether-fungus :nether-roots
    :mangrove-propagule :chorus-flower :chorus-plant
    :trip-wire-hook :repeater :comparator :frogspawn})

(def stack-props
  {:candle :candles
   :sea-pickle :pickles
   :flower-bed :flower-amount
   :leaf-litter :segment-amount
   :turtle-egg :eggs})

(def replaceable-types
  (disj (into ground-types (concat torch-types wall-torch-types))
        :standing-sign))

(defn- boolean-table [pred]
  (let [a (boolean-array (data/block-state-count))]
    (dotimes [i (data/block-state-count)]
      (when-let [t (aget ^objects @type-arr i)]
        (aset a i (boolean (pred i t (aget ^objects @name-arr i))))))
    a))

(def ^:private ^:table needs-support-arr
  (delay (boolean-table
           (fn [_ t _] (contains? needs-support-types t)))))

(def ^:private ^:table attached-arr
  (delay (boolean-table
           (fn [_ t _]
             (or (contains? needs-support-types t)
                 (contains? attached-types t))))))

(def ^:private ^:table replaceable-arr
  (delay (boolean-table
           (fn [_ t _] (contains? replaceable-types t)))))

(def ^:private ^:table liquid-arr
  (delay (boolean-table (fn [_ t _] (= :liquid t)))))

(def ^:private ^:table waterlogged-arr
  (delay (boolean-table
           (fn [st t _]
             (or (contains? water-holder-types t)
                 (= :true (:waterlogged (props-of st))))))))

(def ^:private ^:table water-state (delay (state :water)))

(def ^:private ^:table falls-arr
  (delay (boolean-table (fn [_ t _] (contains? falling-types t)))))

(def ^:private ^:table can-be-replaced-arr
  (delay
    (let [tagged (set (get-in (data/tags) ["block" "replaceable"]))]
      (boolean-table (fn [_ _ n] (contains? tagged n))))))

(defn leaves? [^long st] (contains? (leaves-types) (type-of st)))

(defn needs-support? [^long st]
  (and (known? st) (aget ^booleans @needs-support-arr st)))

(defn attached? [^long st]
  (and (known? st) (aget ^booleans @attached-arr st)))

(defn replaceable? [^long st]
  (and (known? st) (aget ^booleans @replaceable-arr st)))

(defn liquid? [^long st]
  (and (known? st) (aget ^booleans @liquid-arr st)))

(defn waterlogged? [^long st]
  (and (known? st) (aget ^booleans @waterlogged-arr st)))

(defn emptied ^long [^long st] (if (waterlogged? st) @water-state 0))

(def ^:private ^:table lava-state (delay (state :lava)))

(defn liquid-class
  "Returns :water or :lava for a liquid state, else nil.
  A waterlogged state answers :water."
  [st]
  (let [st (long st)]
    (cond
      (liquid? st) (if (= (block-of st) :lava) :lava :water)
      (waterlogged? st) :water)))

(defn liquid-level
  "Returns the level of a liquid state.
  A source is 0 and a fall is 8."
  ^long [st]
  (let [st (long st)]
    (if (liquid? st)
      (- st (long (if (= :lava (liquid-class st))
                    @lava-state
                    @water-state)))
      0)))

(defn source-state? [st]
  (and (liquid? (long st)) (zero? (liquid-level st))))

(defn water? [st] (= :water (liquid-class st)))

(defn lava? [st] (= :lava (liquid-class st)))

(defn water-source? [st] (and (water? st) (source-state? st)))

(defn full-fluid?
  "Whether the fluid of the state fills its cell whole.
  A source does, a fall does and held water does,
  FluidState.isFull."
  [st]
  (and (some? (liquid-class st))
       (let [l (liquid-level st)] (or (zero? l) (>= l 8)))))

(defn full-water?
  "Whether the state is water filling its cell whole."
  [st]
  (and (water? st) (full-fluid? st)))

(defn air? [^long st] (zero? st))

(defn fire? [^long st] (= :fire (type-of st)))

(defn tnt? [^long st] (= :tnt (type-of st)))

(defn falls? [^long st]
  (and (known? st) (aget ^booleans @falls-arr st)))

(defn can-be-replaced? [^long st]
  (or (zero? st)
      (and (known? st) (aget ^booleans @can-be-replaced-arr st))))

(defn free? [^long st]
  (or (zero? st) (fire? st) (liquid? st) (can-be-replaced? st)))

(defn without-water ^long [^long st]
  (if (= :true (:waterlogged (props-of st)))
    (state (block-of st) (assoc (props-of st) :waterlogged :false))
    st))

(defn with-water ^long [^long st]
  (if (contains? (props-of st) :waterlogged)
    (state (block-of st) (assoc (props-of st) :waterlogged :true))
    st))

(defn concrete-of ^long [^long st]
  (state (:concrete (get (data/blocks) (block-of st)))))

(def ^:private shape-classes
  {:fence-block :fence :wall-block :fence :fence-gate-block :gate
   :slab-block  :slab :weathering-copper-slab-block :slab
   :stair-block :stair :weathering-copper-stair-block :stair})

(defn- shape-type [b] (shape-classes (:class b)))

(def ^:private ^:table shape-arr
  (delay (block-table (fn [_ b] (shape-type b)))))

(defn shape-of [^long st]
  (when (known? st) (aget ^objects @shape-arr st)))

(defn fence? [^long st] (= :fence (shape-of st)))

(defn shaped? [^long st] (some? (shape-of st)))

(defn solid? [^long st]
  (and (pos? st) (known? st)
       (not (aget ^booleans @liquid-arr st))
       (not (aget ^booleans @needs-support-arr st))))

(def ^:private ^:table solid-table
  (delay (boolean-table (fn [st _ _] (solid? st)))))

(defn solid-arr ^booleans [] @solid-table)

(defn- int-runs [^long default t]
  (let [a (int-array (data/block-state-count) (int default))]
    (data/each-run! t (fn [i v] (aset a (int i) (int v))))
    a))

(defn- bool-runs [t]
  (let [a (boolean-array (data/block-state-count))]
    (data/each-run! t (fn [i v] (aset a (int i) (boolean v))))
    a))

(def ^:private ^:table dampening-arr
  (delay (int-runs 15 (:dampening (data/light)))))

(def ^:private ^:table emission-arr
  (delay (int-runs 0 (:emission (data/light)))))

(def ^:private ^:table use-shape-arr
  (delay (bool-runs (:use-shape (data/light)))))

(def ^:private ^:table can-occlude-arr
  (delay (bool-runs (:occludes (data/light)))))

(defn- face-mask ^longs [boxes]
  (let [m (long-array 4)]
    (doseq [[u0 v0 u1 v1] boxes
            v (range (long v0) (long v1))
            u (range (long u0) (long u1))]
      (let [b (+ (* 16 (long v)) (long u))
            w (bit-shift-right b 6)]
        (aset m w (bit-or (aget m w) (bit-shift-left 1 b)))))
    m))

(def ^:private full-mask (long-array 4 -1))

(defn- full-face? [^longs m]
  (and (== -1 (aget m 0)) (== -1 (aget m 1))
       (== -1 (aget m 2)) (== -1 (aget m 3))))

(defn- faces-of-kind [kind]
  (mapv (fn [d]
          (let [f (get (:faces kind) (nth dir/six d))]
            (cond (= :full f) full-mask
                  (nil? f) nil
                  :else (face-mask f))))
        (range 6)))

(defn- box-edge? [b axis lo?]
  (if lo?
    (zero? (long (nth b axis)))
    (== 16 (long (nth b (+ 3 axis))))))

(defn- touch-of-kind [kind]
  (reduce (fn [m d]
            (let [axis (nth [1 1 2 2 0 0] d)
                  lo? (even? d)
                  hit? (some #(box-edge? % axis lo?) (:shape kind))]
              (if hit?
                (bit-or (long m) (bit-shift-left 1 d))
                m)))
          0 (range 6)))

(defn- by-kind [f]
  (let [kinds (:palette (:faces (data/light)))]
    (zipmap kinds (map f kinds))))

(def ^:private ^:table kind-faces
  (delay (by-kind faces-of-kind)))

(def ^:private ^:table kind-touch
  (delay (by-kind touch-of-kind)))

(defn- set-faces! [^objects a ^long i faces]
  (let [o (* 6 i)]
    (dotimes [d 6] (aset a (+ o d) (nth faces d)))))

(def ^:private ^:table face-arr
  (delay
    (let [a (object-array (* (data/block-state-count) 6))]
      (data/each-run! (:faces (data/light))
                      (fn [i k]
                        (set-faces! a (long i) (@kind-faces k))))
      a)))

(def ^:private ^:table touch-arr
  (delay
    (let [a (int-array (data/block-state-count))]
      (data/each-run! (:faces (data/light))
                      (fn [i k]
                        (aset a (int i) (int (@kind-touch k)))))
      a)))

(defn dampening ^long [^long st]
  (if (< -1 st (data/block-state-count))
    (aget ^ints @dampening-arr st)
    15))

(defn opacity ^long [^long st] (max 1 (dampening st)))

(defn emits ^long [^long st]
  (if (< -1 st (data/block-state-count))
    (aget ^ints @emission-arr st)
    0))

(defn use-shape-for-light-occlusion? [^long st]
  (and (< -1 st (data/block-state-count))
       (aget ^booleans @use-shape-arr st)))

(defn can-occlude? [^long st]
  (and (< -1 st (data/block-state-count))
       (aget ^booleans @can-occlude-arr st)))

(defn- occlusion-face ^longs [^long st ^long d]
  (when (< -1 st (data/block-state-count))
    (aget ^objects @face-arr (+ (* 6 st) d))))

(defn- covers-block? [^longs a ^longs b]
  (and (== -1 (bit-or (aget a 0) (aget b 0)))
       (== -1 (bit-or (aget a 1) (aget b 1)))
       (== -1 (bit-or (aget a 2) (aget b 2)))
       (== -1 (bit-or (aget a 3) (aget b 3)))))

(defn shape-occludes?
  "Returns true when the faces meeting along d seal.
  The faces are those of from and to, and a seal lets no
  light through."
  [^long from ^long to ^long d]
  (let [a (occlusion-face from d)
        b (occlusion-face to (aget ^ints dir/opposite-index d))]
    (cond
      (and (nil? a) (nil? b)) false
      (nil? a) (full-face? b)
      (nil? b) (full-face? a)
      :else (covers-block? a b))))

(defn- touches? [^long st ^long d]
  (and (< -1 st (data/block-state-count))
       (pos? (bit-and (aget ^ints @touch-arr st)
                      (bit-shift-left 1 d)))))

(defn- merged-side ^longs [^long st ^long d]
  (if (touches? st d) (occlusion-face st d) nil))

(defn light-dampening-into
  "Returns the light cost of crossing from into to along dir.
  The cost is simple when their faces do not seal."
  ^long [^long from ^long to dir ^long simple]
  (let [d (long (dir/index dir))
        a (merged-side from d)
        b (merged-side to (aget ^ints dir/opposite-index d))]
    (cond
      (and (nil? a) (nil? b)) simple
      (nil? a) (if (full-face? b) 16 simple)
      (nil? b) (if (full-face? a) 16 simple)
      :else (if (covers-block? a b) 16 simple))))

(def ^:private ^:table resist-arr
  (delay
    (let [a (double-array (data/block-state-count))]
      (Arrays/fill a 3.0)
      (doseq [[_ b] (data/blocks)
              i (range (reduce * 1 (map count (vals (:props b)))))]
        (aset a (+ (long (:first b)) (long i))
              (double (:resistance b 3.0))))
      a)))

(defn resist ^double [^long st]
  (if (< -1 st (data/block-state-count))
    (aget ^doubles @resist-arr st)
    3.0))

(defn- behind [facing] (dir/offset (dir/opposite facing)))

(defn facing-of [^long st] (:facing (props-of st)))

(defn support-offset [^long st]
  (let [t (type-of st)]
    (cond
      (contains? torch-types t) [0 -1 0]
      (contains? wall-torch-types t) (behind (facing-of st))
      (contains? side-types t) (behind (facing-of st))
      (contains? ground-types t) [0 -1 0])))

(defn- info-of [^long st] (get (data/blocks) (block-of st)))

(defn- with-props-of ^long [block ^long st]
  (state block
         (select-keys (props-of st)
                      (keys (:props (data/info block))))))

(defn- related [^long st k]
  (when-let [b (k (info-of st))] (with-props-of b st)))

(defn weathering? [^long st]
  (let [b (info-of st)]
    (boolean (and b (or (:next b) (:previous b))))))

(defn weather-stage ^long [^long st]
  (loop [b (info-of st) n 0]
    (if-let [p (:previous b)]
      (recur (get (data/blocks) p) (inc n))
      n)))

(defn weathered-next [^long st] (related st :next))

(defn weathered-prev [^long st] (related st :previous))

(defn waxed [^long st] (related st :waxed))

(defn unwaxed [^long st] (related st :unwaxed))

(defn dead-coral ^long [^long st]
  (with-props-of (:dead (info-of st)) (without-water st)))

(defn stripped [^long st] (related st :stripped))

(def named-block-items
  {:redstone :redstone-wire
   :string :tripwire
   :wheat-seeds :wheat
   :cocoa-beans :cocoa
   :pumpkin-seeds :pumpkin-stem
   :melon-seeds :melon-stem
   :carrot :carrots
   :potato :potatoes
   :torchflower-seeds :torchflower-crop
   :pitcher-pod :pitcher-crop
   :beetroot-seeds :beetroots
   :sweet-berries :sweet-berry-bush
   :glow-berries :cave-vines
   :powder-snow-bucket :powder-snow})

(defn wall-block [block] (get-in (data/items) [block :wall]))

(def ^:private standing-and-wall-types
  #{:standing-sign :skull :wither-skull :player-head :torch
    :redstone-torch :banner :ceiling-hanging-sign :coral-fan
    :base-coral-fan})

(defn item->block [item face]
  (let [t (:type (get (data/blocks) item))]
    (cond
      (contains? standing-and-wall-types t) item
      (and (<= 2 (long face) 5) (wall-block item)) (wall-block item)
      (contains? (data/blocks) item) item
      :else (named-block-items item))))

(defn rotation-segment [yaw]
  (let [n (/ (* (+ (double yaw) 180.0) 16.0) 360.0)]
    (bit-and (long (Math/floor (+ n 0.5))) 15)))

(defn skull-rotation [yaw]
  (let [n (/ (* (double yaw) 16.0) 360.0)]
    (bit-and (long (Math/floor (+ n 0.5))) 15)))

(defn- orientation [front top]
  (keyword (str (name front) "_" (name top))))

(defn- crafter-top [front side]
  (case front
    :down (dir/opposite side)
    :up side
    :up))

(defn- crafter-orientation [{:keys [look yaw]}]
  (let [front (dir/opposite (first look))
        top (crafter-top front (dir/player-direction yaw))]
    {:orientation (orientation front top)}))

(defn- jigsaw-orientation [{:keys [face yaw]}]
  (let [front (dir/from-index face)
        top (if (<= (long face) 1)
              (dir/opposite (dir/player-direction yaw))
              :up)]
    {:orientation (orientation front top)}))

(defn- rail-shape [{:keys [yaw]}]
  {:shape (if (#{:east :west} (dir/player-direction yaw))
            :east_west
            :north_south)})

(defn- hopper-facing [{:keys [face]}]
  {:facing (if (<= (long face) 1)
             :down
             (dir/opposite (dir/from-index face)))})

(def ^:private placement-rules
  [[(fn [t _b]
      (#{:rotated-pillar :infested-rotated-pillar :chain
         :weathering-copper-chain :hay :creaking-heart} t))
    (fn [{:keys [face]}]
      {:axis (case (long face) (0 1) :y, (4 5) :x, :z)})]
   [(fn [t _b]
      (#{:end-rod :weathering-lightning-rod :lightning-rod
         :amethyst-cluster :shulker-box} t))
    (fn [{:keys [face]}] {:facing (dir/from-index face)})]
   [(fn [t _b] (#{:piston-base :dispenser :dropper :command} t))
    (fn [{:keys [look]}] {:facing (dir/opposite (first look))})]
   [(fn [t _b] (= :observer t))
    (fn [{:keys [look]}] {:facing (first look)})]
   [(fn [t _b] (= :crafter t)) crafter-orientation]
   [(fn [t _b] (= :jigsaw t)) jigsaw-orientation]
   [(fn [t _b] (= :hopper t)) hopper-facing]
   [(fn [t _b] (#{:rail :powered-rail :detector-rail} t)) rail-shape]
   [(fn [t _b] (= :calibrated-sculk-sensor t))
    (fn [{:keys [yaw]}] {:facing (dir/player-direction yaw)})]
   [(fn [t _b]
      (#{:standing-sign :banner :ceiling-hanging-sign} t))
    (fn [{:keys [yaw]}]
      {:rotation (keyword (str (rotation-segment yaw)))})]
   [(fn [t _b] (#{:skull :wither-skull :player-head} t))
    (fn [{:keys [yaw]}]
      {:rotation (keyword (str (skull-rotation yaw)))})]
   [(fn [t _b] (= :decorated-pot t))
    (fn [{:keys [f]}] {:facing (nth [:south :west :north :east] f)})]
   [(fn [t _b] (= :lantern t))
    (fn [{:keys [face]}]
      {:hanging (if (= 0 (long face)) :true :false)})]
   [(fn [t _b] (contains? (leaves-types) t))
    (fn [_] {:persistent :true})]
   [(fn [t _b] (= :mangrove-propagule t))
    (fn [_] {:age :4})]
   [(fn [t _b] (contains? door-types t))
    (fn [{:keys [yaw]}]
      {:facing (dir/player-direction yaw) :half :lower})]
   [(fn [t _b] (= :bed t))
    (fn [{:keys [yaw]}]
      {:facing (dir/player-direction yaw)
       :part :foot
       :occupied :false})]
   [(fn [t _b] (= :anvil t))
    (fn [{:keys [yaw]}]
      {:facing (dir/clockwise (dir/player-direction yaw))})]
   [(fn [t _b] (= :fence-gate t))
    (fn [{:keys [yaw]}] {:facing (dir/player-direction yaw)})]
   [(fn [t _b] (contains? trapdoor-types t))
    (fn [{:keys [face yaw cursor-y replacing?]}]
      (if (and (not replacing?) (>= (long face) 2))
        {:facing (dir/face-facing face)
         :half (if (> (long cursor-y) 8) :top :bottom)}
        {:facing (dir/opposite (dir/player-direction yaw))
         :half (if (= 1 (long face)) :bottom :top)}))]
   [(fn [_t b] (= :stair (shape-type b)))
    (fn [{:keys [f top?]}]
      {:facing (nth [:south :west :north :east] f)
       :half (if top? :top :bottom)})]
   [(fn [_t b] (= :slab (shape-type b)))
    (fn [{:keys [top?]}] {:type (if top? :top :bottom)})]
   [(fn [t _b] (contains? wall-torch-types t))
    (fn [{:keys [face]}] {:facing (dir/face-facing face)})]
   [(fn [t _b] (contains? side-types t))
    (fn [{:keys [face]}]
      {:facing (get dir/face-facing face :north)})]
   [(fn [_t b] (contains? (:props b) :facing))
    (fn [{:keys [f]}]
      {:facing (nth [:north :east :south :west] f)})]])

(defn- placement-props [t b ctx]
  (let [match (fn [[pred _]] (pred t b))]
    (when-let [[_ f] (first (filter match placement-rules))]
      (f ctx))))

(defn- placement-ctx [{:keys [face yaw pitch cursor-y] :as ctx}]
  (let [face (long face)]
    (assoc ctx
           :face face
           :f (dir/player-index yaw)
           :look (dir/look-order yaw (or pitch 0.0))
           :top? (or (= face 0)
                     (and (not= face 1) (> (long cursor-y) 8))))))

(defn placement [item ctx]
  (when-let [block (item->block item (:face ctx))]
    (let [b (data/info block)
          props (placement-props (:type b) b (placement-ctx ctx))
          props (merge {:waterlogged :false} props)]
      (state block (select-keys props (keys (:props b)))))))

(def ^:private full-box [[0 0 0 16 16 16]])

(defn- shape-at [^long st] (aget ^objects (data/shapes) st))

(defn collision-boxes [^long st]
  (if (known? st)
    (let [v (shape-at st)] (if (nil? v) full-box v))
    full-box))

(defn outline-boxes [^long st]
  (if (known? st)
    (let [v (aget ^objects (data/outlines) st)]
      (if (nil? v) full-box v))
    full-box))

(defn- box-span [boxes ^long lo ^long hi]
  (let [a (reduce min (map #(nth % lo) boxes))
        b (reduce max (map #(nth % hi) boxes))]
    (/ (- (double b) (double a)) 16.0)))

(defn- legacy-solid-box? [boxes]
  (let [xs (box-span boxes 0 3)
        ys (box-span boxes 1 4)
        zs (box-span boxes 2 5)]
    (or (>= (/ (+ xs ys zs) 3.0) 0.7291666666666666)
        (>= ys 1.0))))

(def ^:private ^:table legacy-solid-arr
  (delay
    (let [a (boolean-array (data/block-state-count))]
      (dotimes [i (data/block-state-count)]
        (let [boxes (collision-boxes i)]
          (when (seq boxes)
            (aset a i (boolean (legacy-solid-box? boxes))))))
      a)))

(defn legacy-solid? [^long st]
  (and (pos? st) (known? st) (aget ^booleans @legacy-solid-arr st)))

(def ^:private ^:table full-cube-arr
  (delay
    (let [a (boolean-array (data/block-state-count))]
      (dotimes [i (data/block-state-count)]
        (aset a i (boolean (and (pos? i) (nil? (shape-at i))))))
      a)))

(defn full-cube? [^long st]
  (and (known? st) (aget ^booleans @full-cube-arr st)))

(defn- flags-of ^long [^long st]
  (long (aget ^bytes (data/flags) st)))

(defn blocks-motion? [^long st]
  (and (known? st)
       (pos? (bit-and (flags-of st) 1))
       (not (contains? #{:cobweb :bamboo-sapling} (block-of st)))))

(def ^:private respawnable-types
  #{:banner :wall-banner :standing-sign :wall-sign
    :ceiling-hanging-sign :wall-hanging-sign :pressure-plate
    :weighted-pressure-plate})

(defn possible-to-respawn-in? [^long st]
  (or (contains? respawnable-types (type-of st))
      (and (known? st)
           (zero? (bit-and (flags-of st) 1))
           (not (liquid? st)))))

(defn ignited-by-lava? [^long st]
  (and (known? st) (pos? (bit-and (flags-of st) 2))))

(defn solid-render? [^long st]
  (and (known? st) (pos? (bit-and (flags-of st) 8))))

(defn collision-face-full-up? [^long st]
  (and (known? st) (pos? (bit-and (flags-of st) 16))))

(defn randomly-ticking? [^long st]
  (and (known? st) (pos? (bit-and (flags-of st) 4))))

(defn burnable? [^long st]
  (let [ignite (get-in (data/fire) [(block-of st) :ignite] 0)]
    (and (known? st) (pos? (long ignite)))))

(defn- drop-count ^long [entry roll]
  (let [[lo hi] (:count entry [1 1])
        r (double (roll [:count (:item entry)]))
        span (inc (- (long hi) (long lo)))]
    (+ (long lo) (long (Math/floor (* r span))))))

(defn- survives-explosion? [e roll radius]
  (or (not (:survives-explosion e))
      (nil? radius)
      (<= (double (roll [:survives (:item e)]))
          (/ 1.0 (double radius)))))

(defn- drop-entry [e roll radius props]
  (when (and (not (:entity? e))
             (every? (fn [[k v]] (= v (get props k))) (:props e))
             (survives-explosion? e roll radius)
             (< (double (roll [:chance (:item e)]))
                (double (:chance e 1.0))))
    (let [n (drop-count e roll)]
      (when (pos? n) {:item (:item e) :count n}))))

(defn drops
  "Returns the stacks st drops, rolled with roll. radius, when given,
  is the radius of the explosion that broke it and lowers the drops."
  ([^long st roll] (drops st roll nil))
  ([^long st roll radius]
   (let [table (get (data/drops) (block-of st))
         props (props-of st)]
     (if (vector? table)
       (into [] (keep (fn [e] (drop-entry e roll radius props)))
             table)
       []))))

(defn face-sturdy? [^long st face]
  (and (known? st)
       (pos? (bit-and (long (aget ^bytes (data/sturdy) st))
                      (bit-shift-left 1 (long (dir/index face)))))))

(defn face-holds-rigid? [^long st face]
  (and (known? st)
       (pos? (bit-and (long (aget ^bytes (data/sturdy-rigid) st))
                      (bit-shift-left 1 (long (dir/index face)))))))

(defn face-holds-center? [^long st face]
  (and (known? st)
       (pos? (bit-and (long (aget ^bytes (data/sturdy-center) st))
                      (bit-shift-left 1 (long (dir/index face)))))))

(def ^:private tag-sets (atom {}))

(defn tag-set [tag]
  (or (get @tag-sets tag)
      (let [s (set (get-in (data/tags) ["block" tag]))]
        (get (swap! tag-sets assoc tag s) tag))))

(defn tagged? [^long st tag]
  (contains? (tag-set tag) (block-of st)))

(def face-props [:up :north :south :west :east :down])

(defn faces-of [^long st]
  (let [props (props-of st)]
    (filterv #(= :true (get props %)) face-props)))

(defn- vacant-face? [^long st]
  (some (fn [k] (= :false (get (props-of st) k))) face-props))

(defn stackable? [^long st item]
  (and (= item (block-of st))
       (if-let [k (stack-props (type-of st))]
         (< (prop-long st k) 4)
         (let [t (type-of st)]
           (and (or (= :vine t) (contains? multiface-types t))
                (boolean (vacant-face? st)))))))

(defn stacked ^long [^long st]
  (let [k (stack-props (type-of st))
        n (prop-long st k)]
    (state (block-of st)
           (assoc (props-of st) k (keyword (str (inc n)))))))

(defn same-slab? [^long st item]
  (and (= item (block-of st)) (= :slab (shape-of st))))

(defn slab-part [^long st] (:type (props-of st)))

(defn double-slab ^long [item] (state item {:type :double}))
