(ns collider.world.block
  (:require [clojure.string :as str]
            [collider.data :as data])
  (:import (java.util Arrays)))

(set! *warn-on-reflection* true)

(def ^:const air 0)
(defn state
  (^long [block] (data/state-id block))
  (^long [block props] (data/state-id block props)))

(defn name-of [^long st] (data/state-block st))
(defn props-of [^long st] (second (data/state-props st)))
(def ^:private state-count
  (long (reduce max 0 (map (fn [[_ b]] (+ (long (:first b)) (reduce * 1 (map count (vals (:props b))))))
                           @data/blocks))))

(defn- block-table [f]
  (let [a (object-array state-count)]
    (doseq [[block b] @data/blocks
            :let [v (f block b)]
            :when (some? v)
            i (range (reduce * 1 (map count (vals (:props b)))))]
      (aset a (+ (long (:first b)) (long i)) v))
    a))

(def ^:private type-arr (block-table (fn [_ b] (:type b))))
(def ^:private name-arr (block-table (fn [block _] block)))
(defn- known? [^long st] (< -1 st state-count))
(defn type-of [^long st] (when (known? st) (aget ^objects type-arr st)))
(defn block-of [^long st] (when (known? st) (aget ^objects name-arr st)))
(def door-types #{:door :weathering-copper-door})
(def trapdoor-types #{:trapdoor :weathering-copper-trapdoor})
(def torch-types #{:torch :redstone-torch})
(def wall-torch-types #{:wall-torch :redstone-wall-torch})
(def side-types #{:ladder :wall-sign :wall-hanging-sign :coral-wall-fan :base-coral-wall-fan})
(def ground-types
  #{:sapling :powered-rail :detector-rail :rail :tall-grass :double-plant :dry-vegetation
    :short-dry-grass :tall-dry-grass :flower :mushroom :fire :soul-fire :redstone-wire
    :crop :carrot :potato :beetroot :stem :attached-stem :standing-sign :pressure-plate
    :weighted-pressure-plate :snow-layer :wool-carpet :carpet :bush :sugar-cane
    :nether-wart :torchflower-crop :pitcher-crop :lily-pad :flower-bed :leaf-litter
    :eyeblossom :firefly-bush :kelp :kelp-plant :seagrass :tall-seagrass})
(def water-holder-types #{:kelp :kelp-plant :seagrass :tall-seagrass :bubble-column})
(def falling-types #{:sand :colored-falling :concrete-powder :anvil :scaffolding})
(def coral-types #{:coral :coral-plant :coral-fan :coral-wall-fan})
(def multiface-types #{:glow-lichen :multiface :sculk-vein})
(def growing-plant
  (into {} (for [[head body dir] [[:weeping-vines :weeping-vines-plant :down]
                                  [:twisting-vines :twisting-vines-plant :up]
                                  [:cave-vines :cave-vines-plant :down]]
                 t [head body]]
             [t {:head head :body body :dir dir}])))
(def growing-plant-types (set (keys growing-plant)))
(def needs-support-types (into ground-types (concat torch-types wall-torch-types side-types #{:ceiling-hanging-sign :tall-flower :cactus :cactus-flower :bamboo-sapling :bamboo-stalk :sweet-berry-bush :spore-blossom :hanging-roots :coral-plant :coral-fan :coral-wall-fan :base-coral-plant :base-coral-fan :base-coral-wall-fan :vine} multiface-types growing-plant-types)))
(def attached-types
  #{:lantern :weathering-lantern :bell :farmland :dirt-path :candle :sea-pickle :cocoa
    :amethyst-cluster})
(def stack-props
  {:candle :candles :sea-pickle :pickles :flower-bed :flower-amount :leaf-litter :segment-amount})
(def replaceable-types (disj (into ground-types (concat torch-types wall-torch-types)) :standing-sign))
(defn- boolean-table [pred]
  (let [a (boolean-array state-count)]
    (dotimes [i state-count]
      (when-let [t (aget ^objects type-arr i)]
        (aset a i (boolean (pred i t (aget ^objects name-arr i))))))
    a))

(def ^:private needs-support-arr (boolean-table (fn [_ t _] (contains? needs-support-types t))))
(def ^:private attached-arr (boolean-table (fn [_ t _] (or (contains? needs-support-types t) (contains? attached-types t)))))
(def ^:private replaceable-arr (boolean-table (fn [_ t _] (contains? replaceable-types t))))
(def ^:private liquid-arr (boolean-table (fn [_ t _] (= :liquid t))))
(def ^:private waterlogged-arr
  (boolean-table (fn [st t _] (or (contains? water-holder-types t) (= :true (:waterlogged (props-of st)))))))
(def ^:private water-state (state :water))
(def ^:private falls-arr (boolean-table (fn [_ t _] (contains? falling-types t))))
(def ^:private can-be-replaced-arr
  (let [tagged (set (get-in @data/tags ["block" "replaceable"]))]
    (boolean-table (fn [_ _ n] (contains? tagged n)))))

(defn needs-support? [^long st] (and (known? st) (aget ^booleans needs-support-arr st)))
(defn attached? [^long st] (and (known? st) (aget ^booleans attached-arr st)))
(defn replaceable? [^long st] (and (known? st) (aget ^booleans replaceable-arr st)))
(defn liquid? [^long st] (and (known? st) (aget ^booleans liquid-arr st)))
(defn waterlogged? [^long st] (and (known? st) (aget ^booleans waterlogged-arr st)))
(defn emptied ^long [^long st] (if (waterlogged? st) water-state 0))
(defn air? [^long st] (zero? st))
(defn fire? [^long st] (= :fire (type-of st)))
(defn tnt? [^long st] (= :tnt (type-of st)))
(defn falls? [^long st] (and (known? st) (aget ^booleans falls-arr st)))
(defn can-be-replaced? [^long st] (or (zero? st) (and (known? st) (aget ^booleans can-be-replaced-arr st))))
(defn free? [^long st] (or (zero? st) (fire? st) (liquid? st) (can-be-replaced? st)))
(defn without-water ^long [^long st]
  (if (= :true (:waterlogged (props-of st)))
    (state (block-of st) (assoc (props-of st) :waterlogged :false))
    st))
(defn with-water ^long [^long st]
  (if (contains? (props-of st) :waterlogged)
    (state (block-of st) (assoc (props-of st) :waterlogged :true))
    st))
(defn concrete-of ^long [^long st]
  (let [n (name (block-of st))]
    (state (keyword (subs n 0 (- (count n) 7))))))

(defn- shape-type [t]
  (let [n (name t)]
    (cond
      (or (= t :fence) (= t :wall)) :fence
      (= t :fence-gate) :gate
      (str/ends-with? n "slab") :slab
      (str/ends-with? n "stair") :stair)))

(def ^:private shape-arr
  (let [a (object-array state-count)]
    (dotimes [i state-count]
      (when-let [t (aget ^objects type-arr i)]
        (aset a i (shape-type t))))
    a))

(defn shape-of [^long st] (when (known? st) (aget ^objects shape-arr st)))
(defn fence? [^long st] (= :fence (shape-of st)))
(defn shaped? [^long st] (some? (shape-of st)))
(defn solid? [^long st]
  (and (pos? st) (known? st) (not (aget ^booleans liquid-arr st)) (not (aget ^booleans needs-support-arr st))))

(def solid-arr (boolean-table (fn [st _ _] (solid? st))))
(def ^:private emission
  {:lava 15 :torch 14 :wall-torch 14 :fire 15 :soul-fire 10 :soul-torch 10 :soul-wall-torch 10
   :furnace 13 :redstone-torch 7 :redstone-wall-torch 7 :glowstone 15 :jack-o-lantern 15
   :redstone-lamp 15 :beacon 15 :sea-lantern 15 :lantern 15 :soul-lantern 10 :shroomlight 15
   :end-rod 14 :magma-block 3})

(def ^:private emission-arr
  (let [a (int-array state-count)]
    (doseq [[block em] emission
            :let [b (get @data/blocks block)]
            :when b
            i (range (reduce * 1 (map count (vals (:props b)))))]
      (aset a (+ (long (:first b)) (long i)) (int em)))
    a))

(defn- opacity-of ^long [block t full?]
  (let [n (name block)]
    (cond
      (= :liquid t) (if (= :water block) 3 255)
      (#{:ice :frosted-ice :half-transparent :slime :honey} t) 3
      (#{:transparent :stained-glass :stained-glass-pane :iron-bars} t) 0
      (= :tinted-glass t) 255
      (str/ends-with? (name t) "leaves") 1
      (#{:slab :stair} (shape-type t)) 255
      full? 255
      :else 0)))

(def ^:private opacity-arr
  (let [a (int-array state-count)]
    (doseq [[block b] @data/blocks
            :let [op (opacity-of block (:type b) (:full-cube? b))]
            i (range (reduce * 1 (map count (vals (:props b)))))]
      (aset a (+ (long (:first b)) (long i)) (int op)))
    (aset a 0 (int 0))
    a))

(defn opacity ^long [^long st] (if (< -1 st state-count) (aget ^ints opacity-arr st) 255))
(defn emits ^long [^long st] (if (< -1 st state-count) (aget ^ints emission-arr st) 0))
(defn- resistance-of ^double [block t]
  (let [n (name block)]
    (cond
      (= :bedrock block) 3.6E7
      (= :obsidian block) 1200.0
      (= :liquid t) 100.0
      (= :tnt t) 0.0
      (contains? needs-support-types t) 0.0
      (= :grass block) 0.6
      (= :grass-block block) 0.6
      (#{:dirt :sand :red-sand :ice} block) 0.5
      (= :gravel block) 0.6
      (= :snow-block block) 0.2
      (= :end-stone block) 9.0
      (str/ends-with? n "-planks") 3.0
      (str/ends-with? n "-log") 2.0
      (str/ends-with? n "-wood") 2.0
      (str/ends-with? n "leaves") 0.2
      (str/includes? n "glass") 0.3
      (str/ends-with? n "-wool") 0.8
      (str/ends-with? n "sandstone") 0.8
      (str/ends-with? n "quartz-block") 0.8
      (= :fence t) 3.0
      (#{:block :stair :slab :wall} t) 6.0
      :else 3.0)))

(def ^:private resist-arr
  (let [a (double-array state-count)]
    (Arrays/fill a 3.0)
    (doseq [[block b] @data/blocks
            :let [r (resistance-of block (:type b))]
            i (range (reduce * 1 (map count (vals (:props b)))))]
      (aset a (+ (long (:first b)) (long i)) (double r)))
    a))

(defn resist ^double [^long st] (if (< -1 st state-count) (aget ^doubles resist-arr st) 3.0))
(def ^:private facing-offsets {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})
(def ^:private opposite {:north :south :south :north :west :east :east :west})
(defn facing-offset [facing] (get facing-offsets facing))
(defn behind [facing] (get facing-offsets (get opposite facing)))
(defn facing-of [^long st] (:facing (props-of st)))
(defn support-offset [^long st]
  (let [t (type-of st)]
    (cond
      (contains? torch-types t) [0 -1 0]
      (contains? wall-torch-types t) (behind (facing-of st))
      (contains? side-types t) (behind (facing-of st))
      (contains? ground-types t) [0 -1 0])))

(def face-offsets
  {0 [0 -1 0], 1 [0 1 0], 2 [0 0 -1], 3 [0 0 1], 4 [-1 0 0], 5 [1 0 0]})

(def ^:private face->facing {2 :north 3 :south 4 :west 5 :east})
(def ^:private face->direction {0 :down 1 :up 2 :north 3 :south 4 :west 5 :east})
(defn player-facing ^long [yaw]
  (bit-and (long (Math/floor (+ (/ (* (double yaw) 4.0) 360.0) 0.5))) 3))

(defn player-direction [yaw] (nth [:south :west :north :east] (player-facing yaw)))
(def opposite-facing {:north :south :south :north :west :east :east :west})
(def clockwise {:north :east :east :south :south :west :west :north})
(def counter-clockwise {:north :west :west :south :south :east :east :north})
(def ^:private wall-torches {:torch :wall-torch :soul-torch :soul-wall-torch :redstone-torch :redstone-wall-torch})
(defn- skull-wall [n]
  (cond
    (str/ends-with? n "-skull") (keyword (str (subs n 0 (- (count n) 6)) "-wall-skull"))
    (str/ends-with? n "-head") (keyword (str (subs n 0 (- (count n) 5)) "-wall-head"))))

(defn- wall-variant [item]
  (let [n (name item)
        candidate (or (wall-torches item)
                      (when (and (str/ends-with? n "-sign") (not (str/includes? n "hanging")))
                        (keyword (str (subs n 0 (- (count n) 5)) "-wall-sign")))
                      (when (str/ends-with? n "-coral-fan")
                        (keyword (str (subs n 0 (- (count n) 4)) "-wall-fan")))
                      (when (str/ends-with? n "-banner")
                        (keyword (str (subs n 0 (- (count n) 7)) "-wall-banner")))
                      (skull-wall n))]
    (when (and candidate (contains? @data/blocks candidate)) candidate)))

(def ^:private weather-prefixes ["exposed-" "weathered-" "oxidized-"])
(defn weathering? [^long st]
  (let [t (type-of st)] (and t (str/starts-with? (name t) "weathering-"))))

(defn weather-stage ^long [^long st]
  (let [n (name (block-of st))]
    (long (or (first (keep-indexed (fn [i p] (when (str/starts-with? n p) (inc i))) weather-prefixes)) 0))))

(defn- with-props-of ^long [block ^long st]
  (state block (select-keys (props-of st) (keys (:props (data/info block))))))

(defn- weather-base [^long st]
  (let [n (name (block-of st)) stage (weather-stage st)]
    (cond
      (= n "copper-block") "copper"
      (zero? stage) n
      :else (subs n (count (weather-prefixes (dec stage)))))))

(defn- weather-name [^long st ^long stage]
  (let [base (weather-base st)]
    (keyword (cond
               (zero? stage) (if (= base "copper") "copper-block" base)
               :else (str (weather-prefixes (dec stage)) base)))))

(defn weathered-next [^long st]
  (when (and (weathering? st) (< (weather-stage st) 3))
    (with-props-of (weather-name st (inc (weather-stage st))) st)))

(defn weathered-prev [^long st]
  (when (and (weathering? st) (pos? (weather-stage st)))
    (with-props-of (weather-name st (dec (weather-stage st))) st)))

(defn waxed [^long st]
  (when (weathering? st)
    (let [b (keyword (str "waxed-" (name (block-of st))))]
      (when (contains? @data/blocks b) (with-props-of b st)))))

(defn unwaxed [^long st]
  (let [n (name (block-of st))]
    (when (str/starts-with? n "waxed-")
      (with-props-of (keyword (subs n 6)) st))))

(defn dead-coral ^long [^long st]
  (with-props-of (keyword (str "dead-" (name (block-of st)))) (without-water st)))

(defn stripped [^long st]
  (when (contains? #{:rotated-pillar} (type-of st))
    (let [b (keyword (str "stripped-" (name (block-of st))))]
      (when (contains? @data/blocks b) (with-props-of b st)))))

(def named-block-items
  {:redstone :redstone-wire :string :tripwire :wheat-seeds :wheat :cocoa-beans :cocoa
   :pumpkin-seeds :pumpkin-stem :melon-seeds :melon-stem :carrot :carrots :potato :potatoes
   :torchflower-seeds :torchflower-crop :pitcher-pod :pitcher-crop :beetroot-seeds :beetroots
   :sweet-berries :sweet-berry-bush :glow-berries :cave-vines})

(defn wall-block [block]
  (let [n (name block)]
    (cond
      (str/ends-with? n "-hanging-sign") (keyword (str (subs n 0 (- (count n) 13)) "-wall-hanging-sign"))
      :else (wall-variant block))))

(def ^:private standing-and-wall-types
  #{:standing-sign :skull :player-head})

(defn item->block [item face]
  (let [face (long face)
        n (name item)]
    (cond
      (contains? standing-and-wall-types (:type (get @data/blocks item))) item
      (and (<= 2 face 5) (not (str/ends-with? n "-sign")) (wall-variant item)) (wall-variant item)
      (contains? @data/blocks item) item
      :else (named-block-items item))))

(defn rotation-segment [yaw]
  (bit-and (long (Math/floor (+ (/ (* (+ (double yaw) 180.0) 16.0) 360.0) 0.5))) 15))

(defn skull-rotation [yaw]
  (bit-and (long (Math/floor (+ (/ (* (double yaw) 16.0) 360.0) 0.5))) 15))

(defn placement
  ([item face yaw cursor-y] (placement item face yaw cursor-y false))
  ([item face yaw cursor-y replacing?]
  (when-let [block (item->block item face)]
    (let [b    (data/info block)
          t    (:type b)
          face (long face)
          f    (player-facing yaw)
          top? (or (= face 0) (and (not= face 1) (> (long cursor-y) 8)))
          props (cond
                  (#{:rotated-pillar :infested-rotated-pillar :chain :weathering-copper-chain} t)
                  {:axis (case face (0 1) :y, (4 5) :x, :z)}
                  (#{:end-rod :weathering-lightning-rod :amethyst-cluster} t)
                  {:facing (face->direction face)}
                  (#{:standing-sign :banner :ceiling-hanging-sign} t)
                  {:rotation (keyword (str (rotation-segment yaw)))}
                  (#{:skull :player-head} t)
                  {:rotation (keyword (str (skull-rotation yaw)))}
                  (= :lantern t)
                  {:hanging (if (= face 0) :true :false)}
                  (str/ends-with? (name t) "leaves")
                  {:persistent :true}
                  (contains? door-types t)
                  {:facing (player-direction yaw) :half :lower}
                  (= :bed t)
                  {:facing (player-direction yaw) :part :foot :occupied :false}
                  (= :fence-gate t)
                  {:facing (player-direction yaw)}
                  (contains? trapdoor-types t)
                  (if (and (not replacing?) (>= face 2))
                    {:facing (face->facing face) :half (if (> (long cursor-y) 8) :top :bottom)}
                    {:facing (opposite-facing (player-direction yaw)) :half (if (= face 1) :bottom :top)})
                  (= :stair (shape-type t))
                  {:facing (nth [:south :west :north :east] f) :half (if top? :top :bottom)}
                  (= :slab (shape-type t))
                  {:type (if top? :top :bottom)}
                  (contains? wall-torch-types t)
                  {:facing (face->facing face)}
                  (contains? side-types t)
                  {:facing (get face->facing face :north)}
                  (contains? (:props b) :facing)
                  {:facing (nth [:north :east :south :west] f)})]
      (state block (select-keys (merge {:waterlogged :false} props) (keys (:props b))))))))

(def ^:private full-box [[0 0 0 16 16 16]])
(defn collision-boxes [^long st]
  (get @data/shapes st full-box))

(def ^:private full-cube-arr
  (let [a (boolean-array state-count)]
    (dotimes [i state-count]
      (aset a i (boolean (and (pos? i) (not (contains? @data/shapes i))))))
    a))

(defn full-cube? [^long st]
  (and (known? st) (aget ^booleans full-cube-arr st)))

(def ^:private flag-arr
  (let [a (byte-array state-count)]
    (doseq [[id m] @data/flags] (aset a (int id) (byte m)))
    a))

(defn blocks-motion? [^long st]
  (and (known? st)
       (pos? (bit-and (long (aget ^bytes flag-arr st)) 1))
       (not (contains? #{:cobweb :bamboo-sapling} (block-of st)))))

(defn ignited-by-lava? [^long st]
  (and (known? st) (pos? (bit-and (long (aget ^bytes flag-arr st)) 2))))

(defn solid-render? [^long st]
  (and (known? st) (pos? (bit-and (long (aget ^bytes flag-arr st)) 8))))

(defn collision-face-full-up? [^long st]
  (and (known? st) (pos? (bit-and (long (aget ^bytes flag-arr st)) 16))))

(defn randomly-ticking? [^long st]
  (and (known? st) (pos? (bit-and (long (aget ^bytes flag-arr st)) 4))))

(defn burnable? [^long st]
  (and (known? st) (pos? (long (get-in @data/fire [(block-of st) :ignite] 0)))))

(defn- drop-count ^long [entry rnd]
  (let [[lo hi] (:count entry [1 1])]
    (+ (long lo) (long (Math/floor (* (double (rnd [:count (:item entry)])) (inc (- (long hi) (long lo)))))))))

(defn drops [^long st rnd]
  (let [table (get @data/drops (block-of st))
        props (props-of st)]
    (if (vector? table)
      (into []
            (keep (fn [e]
                    (when (and (not (:entity? e))
                               (every? (fn [[k v]] (= v (get props k))) (:props e))
                               (< (double (rnd [:chance (:item e)])) (double (:chance e 1.0))))
                      (let [n (drop-count e rnd)]
                        (when (pos? n) {:item (:item e) :count n})))))
            table)
      [])))

(def ^:private face-bit {:down 0 :up 1 :north 2 :south 3 :west 4 :east 5})
(defn face-sturdy? [^long st face]
  (and (known? st)
       (pos? (bit-and (long (get @data/sturdy st 63)) (bit-shift-left 1 (long (face-bit face)))))))

(defn face-holds-rigid? [^long st face]
  (and (known? st)
       (pos? (bit-and (long (get @data/sturdy-rigid st 63)) (bit-shift-left 1 (long (face-bit face)))))))

(defn face-holds-center? [^long st face]
  (and (known? st)
       (pos? (bit-and (long (get @data/sturdy-center st 63)) (bit-shift-left 1 (long (face-bit face)))))))

(def ^:private tag-sets (atom {}))
(defn tag-set [tag]
  (or (get @tag-sets tag)
      (get (swap! tag-sets assoc tag (set (get-in @data/tags ["block" tag]))) tag)))

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
         (< (Long/parseLong (name (get (props-of st) k))) 4)
         (and (or (= :vine (type-of st)) (contains? multiface-types (type-of st)))
              (boolean (vacant-face? st))))))

(defn stacked ^long [^long st]
  (let [k (stack-props (type-of st))
        n (Long/parseLong (name (get (props-of st) k)))]
    (state (block-of st) (assoc (props-of st) k (keyword (str (inc n)))))))

(defn same-slab? [^long st item]
  (and (= item (block-of st)) (= :slab (shape-type (type-of st)))))

(defn slab-part [^long st] (:type (props-of st)))
(defn double-slab ^long [item] (state item {:type :double}))
