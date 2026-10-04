(ns collider.world.block
  "Block states and the facts of each state."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.data.state :as states]
            [collider.world.direction :as dir])
  (:import (collider.world Block BlockTables)
           (java.util Arrays)))

(set! *warn-on-reflection* true)

(def ^:const air 0)

(defn state
  "Returns the state id of block with props, or of its default state."
  (^long [block] (states/id block))
  (^long [block props] (states/id block props)))

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

(def ^:private ^:table props-arr
  (delay
    (let [n (data/block-state-count) a (object-array n)]
      (dotimes [i n] (aset a i (second (states/props i))))
      a)))

(defn props-of
  "Returns the properties of st, or nil for an unknown state."
  [^long st]
  (when (known? st) (aget ^objects @props-arr st)))

(defn prop-long
  "Returns the whole number property k of st, 0 when absent."
  ^long [^long st k]
  (Long/parseLong (name (get (props-of st) k :0))))

(declare tables)

(defn- via [m & args]
  (fn [st] `(~m (tables) ~st ~@args)))

(defn type-of
  "Returns the class of the block of st."
  {:inline (via `Block/type)}
  [^long st]
  (Block/type (tables) st))

(defn block-of
  {:inline (via `Block/name)}
  [^long st]
  (Block/name (tables) st))

(defn flag
  [b]
  (if b :true :false))

(defn- prop-value [v] (if (keyword? v) v (keyword (str v))))

(defn with
  "Returns st with the properties in kvs set. A value that is not a
  keyword becomes the keyword of its printed form."
  [st & kvs]
  (let [st (long st)]
    (state (block-of st)
           (reduce (fn [m [k v]] (assoc m k (prop-value v)))
                   (props-of st) (partition 2 kvs)))))

(defn with-long
  ^long [^long st k ^long n]
  (state (block-of st) (assoc (props-of st) k (keyword (str n)))))

(defn- clone-table []
  (let [^objects a (block-table (fn [block b] (get b :clone block)))]
    (doseq [[_ b] (data/blocks)
            [st item] (:clones b)]
      (aset a (long st) item))
    a))

(def ^:private ^:table clone-arr (delay (clone-table)))

(defn clone-of
  "Returns the item a player picks from st, or nil."
  [^long st]
  (when (known? st)
    (let [item (aget ^objects @clone-arr st)]
      (when-not (= :air item) item))))

(defn clone-props
  "Returns the properties a picked item of st keeps."
  [^long st include-data]
  (let [b (get (data/blocks) (block-of st))]
    (cond-> (:clone-props b)
      include-data (into (:data-props b)))))

(defn prop-name
  "Returns the wire name of property k."
  [k] (str/replace (name k) "-" "_"))

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

(defn leaves-types
  [] @leaves-set)

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

(defn- boolean-table ^booleans [pred]
  (Block/table @type-arr @name-arr pred))

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

(def ^:private ^:table has-waterlogged-arr
  (delay (boolean-table
           (fn [st _ _] (contains? (props-of st) :waterlogged)))))

(defn has-waterlogged-prop?
  [^long st]
  (and (known? st) (aget ^booleans @has-waterlogged-arr st)))

(def ^:private ^:table double-slab-arr
  (delay (boolean-table
           (fn [st _ _] (= :double (:type (props-of st)))))))

(defn double-slab?
  [^long st]
  (and (known? st) (aget ^booleans @double-slab-arr st)))

(def ^:private ^:table water-state (delay (state :water)))

(def ^:private ^:table falls-arr
  (delay (boolean-table (fn [_ t _] (contains? falling-types t)))))

(def ^:private ^:table can-be-replaced-arr
  (delay
    (let [tagged (set (get-in (data/tags) ["block" "replaceable"]))]
      (boolean-table (fn [_ _ n] (contains? tagged n))))))

(defn leaves?
  [^long st] (contains? (leaves-types) (type-of st)))

(defn needs-support?
  "Returns true when st breaks without a block to hold it."
  {:inline (via `Block/needsSupport)}
  [^long st]
  (Block/needsSupport (tables) st))

(defn attached?
  "Returns true when st hangs on a neighbour."
  {:inline (via `Block/attached)}
  [^long st]
  (Block/attached (tables) st))

(defn replaceable?
  "Returns true when a placed block replaces st."
  {:inline (via `Block/replaceable)}
  [^long st]
  (Block/replaceable (tables) st))

(defn liquid?
  "Returns true when st is a fluid, not a block that holds one."
  {:inline (via `Block/liquid)}
  [^long st]
  (Block/liquid (tables) st))

(defn waterlogged?
  "Returns true when st holds water, as a water plant does."
  {:inline (via `Block/waterlogged)}
  [^long st]
  (Block/waterlogged (tables) st))

(defn emptied
  "Returns the state that st leaves when it breaks."
  ^long [^long st] (if (waterlogged? st) @water-state 0))

(def ^:private ^:table lava-state (delay (state :lava)))

(defn liquid-class
  "Returns the fluid of st as a keyword, or nil. Held water
  counts as water."
  [st]
  (let [st (long st)]
    (cond
      (liquid? st) (if (= (block-of st) :lava) :lava :water)
      (waterlogged? st) :water)))

(defn liquid-level
  "Returns the level of fluid st, 0 for a source and 8 for a fall."
  ^long [st]
  (let [st (long st)]
    (if (liquid? st)
      (- st (long (if (= :lava (liquid-class st))
                    @lava-state
                    @water-state)))
      0)))

(defn source-state?
  [st]
  (and (liquid? (long st)) (zero? (liquid-level st))))

(defn water?
  "Returns true when st is or holds water."
  [st] (= :water (liquid-class st)))

(defn lava?
  [st] (= :lava (liquid-class st)))

(defn water-source?
  [st] (and (water? st) (source-state? st)))

(defn holds-water-source?
  "Returns true when st is a water source or holds water."
  [st]
  (let [st (long st)]
    (or (waterlogged? st) (water-source? st))))

(defn full-fluid?
  "Returns true when the fluid of st fills its cell. A source, a
  fall and held water do."
  [st]
  (and (some? (liquid-class st))
       (let [l (liquid-level st)] (or (zero? l) (>= l 8)))))

(defn full-water?
  [st]
  (and (water? st) (full-fluid? st)))

(defn air?
  "Returns true when st is plain air."
  [^long st] (zero? st))

(def ^:private air-types #{:air :cave-air :void-air})

(defn air-type?
  "Returns true when st is air of any kind."
  [^long st]
  (contains? air-types (block-of st)))

(defn state-table
  "Returns (f st) for every block state, typed by k as boolean,
  byte or long."
  [k f]
  (let [xs (map f (range (data/block-state-count)))]
    (case k
      :boolean (boolean-array (map boolean xs))
      :byte (byte-array (map byte xs))
      :long (long-array (map long xs)))))

(defn fire?
  [^long st] (= :fire (type-of st)))

(defn tnt?
  [^long st] (= :tnt (type-of st)))

(defn destroyed
  "Returns the change that destroying the block at p makes. The block
  leaves its fluid and drops, and shows its break unless it is fire."
  [p ^long st]
  [p (emptied st)
   (if (contains? #{:fire :soul-fire} (type-of st))
     [[:drop st]]
     [[:break st] [:drop st]])])

(defn falls?
  {:inline (via `Block/falls)}
  [^long st]
  (Block/falls (tables) st))

(defn can-be-replaced?
  "Returns true when the replaceable tag holds st."
  {:inline (via `Block/canBeReplaced)}
  [^long st]
  (Block/canBeReplaced (tables) st))

(defn free?
  "Returns true when a falling block passes through st."
  [^long st]
  (or (zero? st) (fire? st) (liquid? st) (can-be-replaced? st)))

(defn without-water
  ^long [^long st]
  (if (= :true (:waterlogged (props-of st)))
    (state (block-of st) (assoc (props-of st) :waterlogged :false))
    st))

(defn with-water
  "Returns st with water, or st itself when it cannot hold water."
  ^long [^long st]
  (if (contains? (props-of st) :waterlogged)
    (state (block-of st) (assoc (props-of st) :waterlogged :true))
    st))

(defn concrete-of
  "Returns the concrete that powder st turns into."
  ^long [^long st]
  (state (:concrete (get (data/blocks) (block-of st)))))

(def ^:private shape-classes
  {:fence-block :fence :wall-block :fence :fence-gate-block :gate
   :slab-block  :slab :weathering-copper-slab-block :slab
   :stair-block :stair :weathering-copper-stair-block :stair})

(defn shape-type
  "Returns the shape kind of block facts b, or nil."
  [b]
  (shape-classes (:class b)))

(def ^:private ^:table shape-arr
  (delay (block-table (fn [_ b] (shape-type b)))))

(defn shape-of
  {:inline (via `Block/shape)}
  [^long st]
  (Block/shape (tables) st))

(defn fence?
  "Returns true when st joins like a fence."
  [^long st] (= :fence (shape-of st)))

(defn shaped?
  [^long st] (some? (shape-of st)))

(defn- stops? [^long st]
  (and (pos? st)
       (not (aget ^booleans @liquid-arr st))
       (not (aget ^booleans @needs-support-arr st))))

(def ^:private ^:table solid-table
  (delay (boolean-table (fn [st _ _] (stops? st)))))

(defn solid?
  "Returns true when st stops a walking body."
  {:inline (via `Block/solid)}
  [^long st]
  (Block/solid (tables) st))

(defn solid-arr
  ^booleans [] @solid-table)

(defn- int-runs [^long default t]
  (let [a (int-array (data/block-state-count) (int default))]
    (states/each-run! t (fn [i v] (aset a (int i) (int v))))
    a))

(defn- bool-runs [t]
  (let [a (boolean-array (data/block-state-count))]
    (states/each-run! t (fn [i v] (aset a (int i) (boolean v))))
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
  (Block/faceMask
    (long-array (into [] (comp (mapcat #(take 4 %)) (map long))
                      boxes))))

(def ^:private full-mask (long-array 4 -1))

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
      (states/each-run! (:faces (data/light))
                      (fn [i k]
                        (set-faces! a (long i) (@kind-faces k))))
      a)))

(def ^:private ^:table touch-arr
  (delay
    (let [a (int-array (data/block-state-count))]
      (states/each-run! (:faces (data/light))
                      (fn [i k]
                        (aset a (int i) (int (@kind-touch k)))))
      a)))

(defn dampening
  "Returns how much light st takes away."
  {:inline (via `Block/dampening)}
  ^long [^long st]
  (Block/dampening (tables) st))

(defn emits
  "Returns the light level st gives."
  {:inline (via `Block/emission)}
  ^long [^long st]
  (Block/emission (tables) st))

(defn use-shape-for-light-occlusion?
  {:inline (via `Block/useShape)}
  [^long st]
  (Block/useShape (tables) st))

(defn can-occlude?
  "Returns true when st may stop light."
  {:inline (via `Block/canOcclude)}
  [^long st]
  (Block/canOcclude (tables) st))

(defn shape-occludes?
  "Returns true when the faces of from and to that meet along d seal
  and let no light through."
  {:inline (fn [from to d] `(Block/occludes (tables) ~from ~to ~d))}
  [^long from ^long to ^long d]
  (Block/occludes (tables) from to d))

(defn light-dampening-into
  "Returns the light cost of crossing from into to along dir."
  {:inline (fn [from to dir simple]
             `(Block/dampeningInto
                (tables) ~from ~to (dir/index ~dir) ~simple))}
  ^long [^long from ^long to dir ^long simple]
  (Block/dampeningInto (tables) from to (dir/index dir) simple))

(def ^:private ^:table resist-table
  (delay
    (let [a (double-array (data/block-state-count))]
      (Arrays/fill a 3.0)
      (doseq [[_ b] (data/blocks)
              i (range (reduce * 1 (map count (vals (:props b)))))]
        (aset a (+ (long (:first b)) (long i))
              (double (:resistance b 3.0))))
      a)))

(defn resist
  "Returns the blast resistance of st."
  {:inline (via `Block/resist)}
  ^double [^long st]
  (Block/resist (tables) st))

(defn resist-arr
  ^doubles []
  @resist-table)

(defn- behind [facing] (dir/offset (dir/opposite facing)))

(defn facing-of
  [^long st] (:facing (props-of st)))

(defn support-offset
  "Returns the offset to the block that holds st up, or nil."
  [^long st]
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

(defn related
  "Returns the state of the block that the facts of st name under k,
  with the properties the two share, or nil."
  [^long st k]
  (when-let [b (k (info-of st))] (with-props-of b st)))

(defn dead-coral
  "Returns the dead form of coral st, without water."
  ^long [^long st]
  (with-props-of (:dead (info-of st)) (without-water st)))

(defn stripped
  [^long st] (related st :stripped))

(def ^:private full-box [[0 0 0 16 16 16]])

(defn- shape-at [^long st] (aget ^objects (states/shapes) st))

(defn collision-boxes
  "Returns the collision boxes of st in sixteenths."
  [^long st]
  (if (known? st)
    (let [v (shape-at st)] (if (nil? v) full-box v))
    full-box))

(defn outline-boxes
  "Returns the outline boxes of st in sixteenths."
  [^long st]
  (if (known? st)
    (let [v (aget ^objects (states/outlines) st)]
      (if (nil? v) full-box v))
    full-box))

(defn- block-units ^doubles [boxes]
  (double-array (for [b boxes c b] (/ (double c) 16.0))))

(def ^:private ^:table collision-table
  (delay
    (let [a (object-array (data/block-state-count))]
      (dotimes [i (data/block-state-count)]
        (aset a i (block-units (collision-boxes i))))
      a)))

(defn collision-arr
  "Returns the collision boxes of every state in blocks."
  ^objects []
  @collision-table)

(def ^:private ^:const motion-bit 1)

(def ^:private ^:const lava-bit 2)

(def ^:private ^:const ticking-bit 4)

(def ^:private ^:const render-bit 8)

(def ^:private ^:const top-face-bit 16)

(def ^:private ^:const signal-bit 32)

(def ^:private ^:const legacy-solid-bit 64)

(defn- flagged? [^long st ^long mask]
  (pos? (bit-and (long (aget ^bytes (states/flags) st)) mask)))

(def ^:private ^:table legacy-solid-arr
  (delay (boolean-table
           (fn [st _ _] (flagged? st legacy-solid-bit)))))

(defn legacy-solid?
  "Returns true when st counts as solid for old rules."
  {:inline (via `Block/legacySolid)}
  [^long st]
  (Block/legacySolid (tables) st))

(def ^:private ^:table full-cube-arr
  (delay (Block/fullCubes (states/shapes))))

(defn full-cube?
  "Returns true when the collision shape of st is a full cube."
  {:inline (via `Block/fullCube)}
  [^long st]
  (Block/fullCube (tables) st))

(defn cube-arr
  ^booleans []
  @full-cube-arr)

(def ^:private passable #{:cobweb :bamboo-sapling})

(defn- motion? [^long st n]
  (and (flagged? st motion-bit)
       (not (contains? passable n))))

(def ^:private ^:table blocks-motion-arr
  (delay (boolean-table (fn [st _ n] (motion? st n)))))

(def ^:private ^:table table-set
  (delay
    (BlockTables/install
     (BlockTables.
       @type-arr @name-arr @shape-arr @needs-support-arr @attached-arr
       @replaceable-arr @liquid-arr @waterlogged-arr @falls-arr
       @can-be-replaced-arr @solid-table @legacy-solid-arr
       @full-cube-arr @blocks-motion-arr @use-shape-arr
       @can-occlude-arr @dampening-arr @emission-arr @touch-arr
       @face-arr @resist-table (states/flags) (states/sturdy)
       (states/sturdy-rigid) (states/sturdy-center)))))

(defn tables
  ^BlockTables []
  (or (BlockTables/current) @table-set))

(defn blocks-motion?
  {:inline (via `Block/blocksMotion)}
  [^long st]
  (Block/blocksMotion (tables) st))

(defn- flag? [^long st ^long mask] (Block/flag (tables) st mask))

(def ^:private respawnable-types
  #{:banner :wall-banner :standing-sign :wall-sign
    :ceiling-hanging-sign :wall-hanging-sign :pressure-plate
    :weighted-pressure-plate})

(defn possible-to-respawn-in?
  [^long st]
  (or (contains? respawnable-types (type-of st))
      (and (known? st) (not (flag? st motion-bit))
           (not (liquid? st)))))

(defn ignited-by-lava?
  {:inline (via `Block/flag lava-bit)}
  [^long st]
  (flag? st lava-bit))

(defn solid-render?
  {:inline (via `Block/flag render-bit)}
  [^long st]
  (flag? st render-bit))

(defn collision-face-full-up?
  "Returns true when the top face of st is full."
  {:inline (via `Block/flag top-face-bit)}
  [^long st]
  (flag? st top-face-bit))

(defn signal-source?
  "Returns true when st gives a redstone signal."
  {:inline (via `Block/flag signal-bit)}
  [^long st]
  (flag? st signal-bit))

(defn randomly-ticking?
  {:inline (via `Block/flag ticking-bit)}
  [^long st]
  (flag? st ticking-bit))

(defn burnable?
  "Returns true when fire can catch st."
  [^long st]
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
  "Returns the stacks that st drops, rolled with roll. An explosion
  of radius lowers the drops."
  ([^long st roll] (drops st roll nil))
  ([^long st roll radius]
   (let [table (get (data/drops) (block-of st))
         props (props-of st)]
     (if (vector? table)
       (into [] (keep (fn [e] (drop-entry e roll radius props)))
             table)
       []))))

(defn- face-form [m]
  (fn [st face] `(~m (tables) ~st (dir/index ~face))))

(defn face-sturdy?
  "Returns true when face of st holds a block."
  {:inline (face-form `Block/sturdy)}
  [^long st face]
  (Block/sturdy (tables) st (dir/index face)))

(defn face-holds-rigid?
  "Returns true when face of st holds a block rigidly."
  {:inline (face-form `Block/sturdyRigid)}
  [^long st face]
  (Block/sturdyRigid (tables) st (dir/index face)))

(defn face-holds-center?
  "Returns true when face of st holds a block at its center."
  {:inline (face-form `Block/sturdyCenter)}
  [^long st face]
  (Block/sturdyCenter (tables) st (dir/index face)))

(def ^:private ^:table tag-sets
  (delay (update-vals (get (data/tags) "block") set)))

(defn tag-set
  "Returns the blocks of block tag, an empty set when absent."
  [tag] (get @tag-sets tag #{}))

(defn tagged?
  [^long st tag]
  (contains? (tag-set tag) (block-of st)))

(def ^:private ^:table conductor-arr
  (delay
    (let [a (boolean-array (data/block-state-count))
          top (dec (alength a))]
      (doseq [[lo hi] (:conductors (data/spawns))
              id (range lo (inc (min (long hi) top)))]
        (aset a (int id) true))
      a)))

(defn conductor?
  "Returns true when st conducts redstone."
  [^long st]
  (aget ^booleans @conductor-arr st))

(def face-props [:up :north :south :west :east :down])

(defn faces-of
  "Returns the faces that st covers."
  [^long st]
  (let [props (props-of st)]
    (filterv #(= :true (get props %)) face-props)))

(defn- vacant-face? [^long st]
  (some (fn [k] (= :false (get (props-of st) k))) face-props))

(defn stackable?
  "Returns true when item adds one more to st."
  [^long st item]
  (and (= item (block-of st))
       (if-let [k (stack-props (type-of st))]
         (< (prop-long st k) 4)
         (let [t (type-of st)]
           (and (or (= :vine t) (contains? multiface-types t))
                (boolean (vacant-face? st)))))))

(defn stacked
  "Returns st with one more of its stacked parts."
  ^long [^long st]
  (let [k (stack-props (type-of st))
        n (prop-long st k)]
    (state (block-of st)
           (assoc (props-of st) k (keyword (str (inc n)))))))

(defn same-slab?
  "Returns true when st is a slab of item."
  [^long st item]
  (and (= item (block-of st)) (= :slab (shape-of st))))

(defn slab-part
  "Returns the slab type of st."
  [^long st] (:type (props-of st)))

(defn double-slab
  ^long [item] (state item {:type :double}))
