(ns collider.world.block
  "Per-state block tables built from resources/mc: type, name, shape, light,
   resistance, placement. A state is its global 26.2 id; `state` gives the id
   for a block name and its properties."
  (:require [clojure.string :as str]
            [collider.data :as data])
  (:import (java.util Arrays)))

(set! *warn-on-reflection* true)

(def ^:const air 0)

(defn state
  "Global state id of a block by name and properties; properties not given
   take their defaults."
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

(def torch-types #{:torch :redstone-torch})
(def wall-torch-types #{:wall-torch :redstone-wall-torch})
(def side-types #{:ladder :wall-sign :wall-hanging-sign})
(def ground-types
  #{:sapling :powered-rail :detector-rail :rail :tall-grass :double-plant :dry-vegetation
    :short-dry-grass :tall-dry-grass :flower :mushroom :fire :soul-fire :redstone-wire
    :crop :carrot :potato :beetroot :stem :attached-stem :standing-sign :pressure-plate
    :weighted-pressure-plate :snow-layer :wool-carpet :carpet :bush :sugar-cane
    :nether-wart :torchflower-crop :pitcher-crop :lily-pad :flower-bed :leaf-litter
    :eyeblossom :firefly-bush :kelp :kelp-plant :seagrass :tall-seagrass})
(def water-holder-types #{:kelp :kelp-plant :seagrass :tall-seagrass :bubble-column})
(def needs-support-types (into ground-types (concat torch-types wall-torch-types side-types)))
(def replaceable-types (disj (into ground-types (concat torch-types wall-torch-types)) :standing-sign))

(defn- boolean-table [pred]
  (let [a (boolean-array state-count)]
    (dotimes [i state-count]
      (when-let [t (aget ^objects type-arr i)]
        (aset a i (boolean (pred i t (aget ^objects name-arr i))))))
    a))

(def ^:private needs-support-arr (boolean-table (fn [_ t _] (contains? needs-support-types t))))
(def ^:private replaceable-arr (boolean-table (fn [_ t _] (contains? replaceable-types t))))
(def ^:private liquid-arr (boolean-table (fn [_ t _] (= :liquid t))))
(def ^:private waterlogged-arr
  (boolean-table (fn [st t _] (or (contains? water-holder-types t) (= :true (:waterlogged (props-of st)))))))
(def ^:private water-state (state :water))

(defn needs-support?
  "True for blocks that drop when the block they rest on is gone: plants,
   torches, rails, ladders, signs."
  [^long st] (and (known? st) (aget ^booleans needs-support-arr st)))
(defn replaceable?
  "True for blocks that flowing water or lava replaces."
  [^long st] (and (known? st) (aget ^booleans replaceable-arr st)))
(defn liquid? [^long st] (and (known? st) (aget ^booleans liquid-arr st)))
(defn waterlogged?
  "True for states with water inside: waterlogged fences, slabs and the like,
   kelp, seagrass, bubble columns. They act as a water source for the flow."
  [^long st] (and (known? st) (aget ^booleans waterlogged-arr st)))
(defn emptied
  "What is left when the block is removed: water for a waterlogged state, air otherwise."
  ^long [^long st] (if (waterlogged? st) water-state 0))
(defn air? [^long st] (zero? st))
(defn fire? [^long st] (= :fire (type-of st)))
(defn tnt? [^long st] (= :tnt (type-of st)))

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
      (#{:transparent :stained-glass :tinted-glass :stained-glass-pane :iron-bars} t) 0
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

(defn player-facing
  ^long [yaw]
  (bit-and (long (Math/floor (+ (/ (* (double yaw) 4.0) 360.0) 0.5))) 3))

(defn player-direction
  "Where the player looks, as vanilla getHorizontalDirection."
  [yaw] (nth [:south :west :north :east] (player-facing yaw)))

(def opposite-facing {:north :south :south :north :west :east :east :west})
(def clockwise {:north :east :east :south :south :west :west :north})
(def counter-clockwise {:north :west :west :south :south :east :east :north})

(def ^:private wall-torches {:torch :wall-torch :soul-torch :soul-wall-torch :redstone-torch :redstone-wall-torch})

(defn- wall-variant
  "Block an item turns into on a wall: wall torches, wall signs, wall banners."
  [item]
  (let [n (name item)
        candidate (or (wall-torches item)
                      (when (and (str/ends-with? n "-sign") (not (str/includes? n "hanging")))
                        (keyword (str (subs n 0 (- (count n) 5)) "-wall-sign")))
                      (when (str/ends-with? n "-banner")
                        (keyword (str (subs n 0 (- (count n) 7)) "-wall-banner"))))]
    (when (and candidate (contains? @data/blocks candidate)) candidate)))

(defn item->block [item face]
  (let [face (long face)]
    (cond
      (and (<= 2 face 5) (wall-variant item)) (wall-variant item)
      (contains? @data/blocks item) item)))

(defn rotation-segment
  "One of 16 rotations for signs and banners, as vanilla RotationSegment."
  [yaw]
  (bit-and (long (Math/floor (+ (/ (* (+ (double yaw) 180.0) 16.0) 360.0) 0.5))) 15))

(defn placement
  "State the client places for item on face (0..5, vanilla order) with the
   player's yaw and the cursor height in 16ths, before connections. nil if the
   item is not a block."
  ([item face yaw cursor-y] (placement item face yaw cursor-y false))
  ([item face yaw cursor-y replacing?]
  (when-let [block (item->block item face)]
    (let [b    (data/info block)
          t    (:type b)
          face (long face)
          f    (player-facing yaw)
          top? (or (= face 0) (and (not= face 1) (> (long cursor-y) 8)))
          props (cond
                  (#{:rotated-pillar :infested-rotated-pillar} t)
                  {:axis (case face (0 1) :y, (4 5) :x, :z)}
                  (#{:standing-sign :banner} t)
                  {:rotation (keyword (str (rotation-segment yaw)))}
                  (= :lantern t)
                  {:hanging (if (= face 0) :true :false)}
                  (str/ends-with? (name t) "leaves")
                  {:persistent :true}
                  (= :door t)
                  {:facing (player-direction yaw) :half :lower}
                  (= :fence-gate t)
                  {:facing (player-direction yaw)}
                  (= :trapdoor t)
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
      (state block (select-keys props (keys (:props b))))))))

(def ^:private full-box [[0 0 0 16 16 16]])

(defn collision-boxes
  "Collision boxes of the state as [x0 y0 z0 x1 y1 z1] in 16ths of a block,
   from the vanilla shapes."
  [^long st]
  (get @data/shapes st full-box))

(def ^:private full-cube-arr
  (let [a (boolean-array state-count)]
    (dotimes [i state-count]
      (aset a i (boolean (and (pos? i) (not (contains? @data/shapes i))))))
    a))

(defn full-cube? [^long st]
  (and (known? st) (aget ^booleans full-cube-arr st)))

(def ^:private face-bit {:down 0 :up 1 :north 2 :south 3 :west 4 :east 5})

(defn face-sturdy?
  "True if the face (:down :up :north :south :west :east) of the state is full
   and solid, as vanilla isFaceSturdy. Fences and walls connect to sturdy faces."
  [^long st face]
  (and (known? st)
       (pos? (bit-and (long (get @data/sturdy st 63)) (bit-shift-left 1 (long (face-bit face)))))))

(defn same-slab? [^long st item]
  (and (= item (block-of st)) (= :slab (shape-type (type-of st)))))

(defn slab-part [^long st] (:type (props-of st)))
(defn double-slab ^long [item] (state item {:type :double}))
