(ns collider.world.block
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.direction :as dir])
  (:import (java.util Arrays)))

(set! *warn-on-reflection* true)

(def ^:const air 0)
(defn state
  (^long [block] (data/state-id block))
  (^long [block props] (data/state-id block props)))

(defn name-of [^long st] (data/state-block st))
(defn props-of [^long st] (second (data/state-props st)))
(defn prop-long ^long [^long st k] (Long/parseLong (name (get (props-of st) k :0))))
(def ^:private state-count
  (long (reduce max 0 (map (fn [[_ b]] (+ (long (:first b)) (reduce * 1 (map count (vals (:props b))))))
                           data/blocks))))

(defn- block-table [f]
  (let [a (object-array state-count)]
    (doseq [[block b] data/blocks
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
(def side-types #{:ladder :wall-sign :wall-hanging-sign :wall-banner :coral-wall-fan :base-coral-wall-fan})
(def ground-types
  #{:sapling :powered-rail :detector-rail :rail :tall-grass :double-plant :dry-vegetation
    :short-dry-grass :tall-dry-grass :flower :mushroom :fire :soul-fire :redstone-wire
    :crop :carrot :potato :beetroot :stem :attached-stem :standing-sign :pressure-plate
    :weighted-pressure-plate :snow-layer :wool-carpet :carpet :bush :sugar-cane
    :nether-wart :torchflower-crop :pitcher-crop :lily-pad :flower-bed :leaf-litter
    :eyeblossom :firefly-bush :kelp :kelp-plant :seagrass :tall-seagrass})
(def water-holder-types #{:kelp :kelp-plant :seagrass :tall-seagrass :bubble-column})
(def falling-types #{:sand :colored-falling :concrete-powder :anvil :scaffolding :pointed-dripstone :sulfur-spike})
(def coral-types #{:coral :coral-plant :coral-fan :coral-wall-fan})
(def cauldron-types #{:cauldron :layered-cauldron :lava-cauldron})
(def multiface-types #{:glow-lichen :multiface :sculk-vein})
(def leaves-types #{:mangrove-leaves :tinted-particle-leaves :untinted-particle-leaves})
(def growing-plant
  (into {} (for [[head body dir] [[:weeping-vines :weeping-vines-plant :down]
                                  [:twisting-vines :twisting-vines-plant :up]
                                  [:cave-vines :cave-vines-plant :down]]
                 t [head body]]
             [t {:head head :body body :dir dir}])))
(def growing-plant-types (set (keys growing-plant)))
(def needs-support-types (into ground-types (concat torch-types wall-torch-types side-types #{:ceiling-hanging-sign :tall-flower :cactus :cactus-flower :bamboo-sapling :bamboo-stalk :sweet-berry-bush :banner :spore-blossom :hanging-roots :coral-plant :coral-fan :coral-wall-fan :base-coral-plant :base-coral-fan :base-coral-wall-fan :vine} multiface-types growing-plant-types)))
(def attached-types
  #{:lantern :weathering-lantern :bell :farmland :dirt-path :candle :sea-pickle :cocoa
    :cake :candle-cake :button :lever
    :amethyst-cluster :hanging-moss :big-dripleaf :big-dripleaf-stem :small-dripleaf
    :azalea :wither-rose :nether-sprouts :nether-fungus :nether-roots
    :mangrove-propagule :chorus-flower :chorus-plant})
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
  (let [tagged (set (get-in data/tags ["block" "replaceable"]))]
    (boolean-table (fn [_ _ n] (contains? tagged n)))))

(defn leaves? [^long st] (contains? leaves-types (type-of st)))
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

(defn- each-run! [runs f]
  (doseq [r runs]
    (let [[lo hi] (if (number? r) [r r] [(nth r 0) (nth r (if (= 2 (count r)) 0 1))])
          v (if (number? r) nil (peek r))]
      (dotimes [i (inc (- (long hi) (long lo)))]
        (let [id (+ (long lo) i)] (when (< id state-count) (f id v)))))))

(defn- flag-run! [runs f]
  (doseq [r runs]
    (let [[lo hi] (if (number? r) [r r] r)]
      (dotimes [i (inc (- (long hi) (long lo)))]
        (let [id (+ (long lo) i)] (when (< id state-count) (f id)))))))

(defn- int-runs [^long default runs]
  (let [a (int-array state-count (int default))]
    (each-run! runs (fn [i v] (aset a (int i) (int v))))
    a))

(defn- bool-runs [runs]
  (let [a (boolean-array state-count)]
    (flag-run! runs (fn [i] (aset a (int i) true)))
    a))

(def ^:private dampening-arr (int-runs 15 (:dampening data/light)))
(def ^:private emission-arr (int-runs 0 (:emission data/light)))
(def ^:private use-shape-arr (bool-runs (:use-shape data/light)))
(def ^:private can-occlude-arr (bool-runs (:occludes data/light)))

(defn- face-mask ^longs [boxes]
  (let [m (long-array 4)]
    (doseq [[u0 v0 u1 v1] boxes
            v (range (long v0) (long v1))
            u (range (long u0) (long u1))]
      (let [b (+ (* 16 (long v)) (long u))]
        (aset m (bit-shift-right b 6) (bit-or (aget m (bit-shift-right b 6)) (bit-shift-left 1 b)))))
    m))

(def ^:private full-mask (long-array 4 -1))
(defn- full-face? [^longs m]
  (and (== -1 (aget m 0)) (== -1 (aget m 1)) (== -1 (aget m 2)) (== -1 (aget m 3))))


(defn- faces-of-kind [kind]
  (mapv (fn [d]
          (let [f (get (:faces kind) (nth dir/six d))]
            (cond (= :full f) full-mask (nil? f) nil :else (face-mask f))))
        (range 6)))

(defn- touch-of-kind [kind]
  (reduce (fn [m d]
            (let [axis (nth [1 1 2 2 0 0] d)
                  lo? (even? d)
                  hit? (some (fn [b] (if lo? (zero? (long (nth b axis))) (== 16 (long (nth b (+ 3 axis))))))
                             (:shape kind))]
              (if hit? (bit-or (long m) (bit-shift-left 1 d)) m)))
          0 (range 6)))

(def ^:private kind-faces (mapv faces-of-kind (:kinds data/light)))
(def ^:private kind-touch (int-array (map touch-of-kind (:kinds data/light))))

(def ^:private face-arr
  (let [a (object-array (* state-count 6))]
    (each-run! (:faces data/light)
               (fn [i k] (dotimes [d 6] (aset a (+ (* 6 (long i)) d) (nth (nth kind-faces k) d)))))
    a))

(def ^:private touch-arr
  (let [a (int-array state-count)]
    (each-run! (:faces data/light) (fn [i k] (aset a (int i) (aget ^ints kind-touch (int k)))))
    a))

(defn dampening ^long [^long st] (if (< -1 st state-count) (aget ^ints dampening-arr st) 15))
(defn opacity ^long [^long st] (max 1 (dampening st)))
(defn emits ^long [^long st] (if (< -1 st state-count) (aget ^ints emission-arr st) 0))
(defn use-shape-for-light-occlusion? [^long st]
  (and (< -1 st state-count) (aget ^booleans use-shape-arr st)))
(defn can-occlude? [^long st] (and (< -1 st state-count) (aget ^booleans can-occlude-arr st)))

(defn- occlusion-face ^longs [^long st ^long d]
  (when (< -1 st state-count) (aget ^objects face-arr (+ (* 6 st) d))))

(defn- covers-block? [^longs a ^longs b]
  (and (== -1 (bit-or (aget a 0) (aget b 0))) (== -1 (bit-or (aget a 1) (aget b 1)))
       (== -1 (bit-or (aget a 2) (aget b 2))) (== -1 (bit-or (aget a 3) (aget b 3)))))

(defn shape-occludes? [^long from ^long to ^long d]
  (let [a (occlusion-face from d)
        b (occlusion-face to (aget ^ints dir/opposite-index d))]
    (cond
      (and (nil? a) (nil? b)) false
      (nil? a) (full-face? b)
      (nil? b) (full-face? a)
      :else (covers-block? a b))))

(defn- touches? [^long st ^long d]
  (and (< -1 st state-count) (pos? (bit-and (aget ^ints touch-arr st) (bit-shift-left 1 d)))))

(defn- merged-side ^longs [^long st ^long d]
  (if (touches? st d) (occlusion-face st d) nil))

(defn light-dampening-into ^long [^long from ^long to dir ^long simple]
  (let [d (long (dir/index dir))
        a (merged-side from d)
        b (merged-side to (aget ^ints dir/opposite-index d))]
    (cond
      (and (nil? a) (nil? b)) simple
      (nil? a) (if (full-face? b) 16 simple)
      (nil? b) (if (full-face? a) 16 simple)
      :else (if (covers-block? a b) 16 simple))))

(def ^:private resist-arr
  (let [a (double-array state-count)]
    (Arrays/fill a 3.0)
    (doseq [[_ b] data/blocks
            i (range (reduce * 1 (map count (vals (:props b)))))]
      (aset a (+ (long (:first b)) (long i)) (double (:resistance b 3.0))))
    a))

(defn resist ^double [^long st] (if (< -1 st state-count) (aget ^doubles resist-arr st) 3.0))
(defn- behind [facing] (dir/offset (dir/opposite facing)))
(defn facing-of [^long st] (:facing (props-of st)))
(defn support-offset [^long st]
  (let [t (type-of st)]
    (cond
      (contains? torch-types t) [0 -1 0]
      (contains? wall-torch-types t) (behind (facing-of st))
      (contains? side-types t) (behind (facing-of st))
      (contains? ground-types t) [0 -1 0])))

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
    (when (and candidate (contains? data/blocks candidate)) candidate)))

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
      (when (contains? data/blocks b) (with-props-of b st)))))

(defn unwaxed [^long st]
  (let [n (name (block-of st))]
    (when (str/starts-with? n "waxed-")
      (with-props-of (keyword (subs n 6)) st))))

(defn dead-coral ^long [^long st]
  (with-props-of (keyword (str "dead-" (name (block-of st)))) (without-water st)))

(defn stripped [^long st]
  (when (contains? #{:rotated-pillar} (type-of st))
    (let [b (keyword (str "stripped-" (name (block-of st))))]
      (when (contains? data/blocks b) (with-props-of b st)))))

(def named-block-items
  {:redstone           :redstone-wire :string :tripwire :wheat-seeds :wheat :cocoa-beans :cocoa
   :pumpkin-seeds      :pumpkin-stem :melon-seeds :melon-stem :carrot :carrots :potato :potatoes
   :torchflower-seeds  :torchflower-crop :pitcher-pod :pitcher-crop :beetroot-seeds :beetroots
   :sweet-berries      :sweet-berry-bush :glow-berries :cave-vines
   :powder-snow-bucket :powder-snow})

(defn wall-block [block]
  (let [n (name block)]
    (cond
      (str/ends-with? n "-hanging-sign") (keyword (str (subs n 0 (- (count n) 13)) "-wall-hanging-sign"))
      :else (wall-variant block))))

(def ^:private standing-and-wall-types
  #{:standing-sign :skull :wither-skull :player-head})

(defn item->block [item face]
  (let [face (long face)
        n (name item)]
    (cond
      (contains? standing-and-wall-types (:type (get data/blocks item))) item
      (and (<= 2 face 5) (not (str/ends-with? n "-sign")) (wall-variant item)) (wall-variant item)
      (contains? data/blocks item) item
      :else (named-block-items item))))

(defn rotation-segment [yaw]
  (bit-and (long (Math/floor (+ (/ (* (+ (double yaw) 180.0) 16.0) 360.0) 0.5))) 15))

(defn skull-rotation [yaw]
  (bit-and (long (Math/floor (+ (/ (* (double yaw) 16.0) 360.0) 0.5))) 15))

(defn placement
  ([item face yaw cursor-y] (placement item face yaw cursor-y false))
  ([item face yaw cursor-y replacing?]
   (when-let [block (item->block item face)]
     (let [b (data/info block)
           t (:type b)
           face (long face)
           f (dir/player-index yaw)
           top? (or (= face 0) (and (not= face 1) (> (long cursor-y) 8)))
           props (cond
                   (#{:rotated-pillar :infested-rotated-pillar :chain :weathering-copper-chain} t)
                   {:axis (case face (0 1) :y, (4 5) :x, :z)}
                   (#{:end-rod :weathering-lightning-rod :amethyst-cluster :shulker-box} t)
                   {:facing (dir/from-index face)}
                   (#{:standing-sign :banner :ceiling-hanging-sign} t)
                   {:rotation (keyword (str (rotation-segment yaw)))}
                   (#{:skull :wither-skull :player-head} t)
                   {:rotation (keyword (str (skull-rotation yaw)))}
                   (= :decorated-pot t)
                   {:facing (nth [:south :west :north :east] f)}
                   (= :lantern t)
                   {:hanging (if (= face 0) :true :false)}
                   (str/ends-with? (name t) "leaves")
                   {:persistent :true}
                   (= :mangrove-propagule t)
                   {:age :4}
                   (contains? door-types t)
                   {:facing (dir/player-direction yaw) :half :lower}
                   (= :bed t)
                   {:facing (dir/player-direction yaw) :part :foot :occupied :false}
                   (= :fence-gate t)
                   {:facing (dir/player-direction yaw)}
                   (contains? trapdoor-types t)
                   (if (and (not replacing?) (>= face 2))
                     {:facing (dir/face-facing face) :half (if (> (long cursor-y) 8) :top :bottom)}
                     {:facing (dir/opposite (dir/player-direction yaw)) :half (if (= face 1) :bottom :top)})
                   (= :stair (shape-type t))
                   {:facing (nth [:south :west :north :east] f) :half (if top? :top :bottom)}
                   (= :slab (shape-type t))
                   {:type (if top? :top :bottom)}
                   (contains? wall-torch-types t)
                   {:facing (dir/face-facing face)}
                   (contains? side-types t)
                   {:facing (get dir/face-facing face :north)}
                   (contains? (:props b) :facing)
                   {:facing (nth [:north :east :south :west] f)})]
       (state block (select-keys (merge {:waterlogged :false} props) (keys (:props b))))))))

(def ^:private full-box [[0 0 0 16 16 16]])
(defn collision-boxes [^long st]
  (get data/shapes st full-box))

(defn outline-boxes [^long st]
  (get data/outlines st full-box))

(def ^:private legacy-solid-arr
  (let [a (boolean-array state-count)]
    (dotimes [i state-count]
      (let [boxes (get data/shapes i full-box)]
        (when (seq boxes)
          (let [x0 (reduce min (map #(nth % 0) boxes)) y0 (reduce min (map #(nth % 1) boxes))
                z0 (reduce min (map #(nth % 2) boxes)) x1 (reduce max (map #(nth % 3) boxes))
                y1 (reduce max (map #(nth % 4) boxes)) z1 (reduce max (map #(nth % 5) boxes))
                xs (/ (- (double x1) (double x0)) 16.0)
                ys (/ (- (double y1) (double y0)) 16.0)
                zs (/ (- (double z1) (double z0)) 16.0)]
            (aset a i (boolean (or (>= (/ (+ xs ys zs) 3.0) 0.7291666666666666) (>= ys 1.0))))))))
    a))

(defn legacy-solid? [^long st]
  (and (pos? st) (known? st) (aget ^booleans legacy-solid-arr st)))

(def ^:private full-cube-arr
  (let [a (boolean-array state-count)]
    (dotimes [i state-count]
      (aset a i (boolean (and (pos? i) (not (contains? data/shapes i))))))
    a))

(defn full-cube? [^long st]
  (and (known? st) (aget ^booleans full-cube-arr st)))

(def ^:private flag-arr
  (let [a (byte-array state-count)]
    (doseq [[id m] data/flags] (aset a (int id) (byte m)))
    a))

(defn blocks-motion? [^long st]
  (and (known? st)
       (pos? (bit-and (long (aget ^bytes flag-arr st)) 1))
       (not (contains? #{:cobweb :bamboo-sapling} (block-of st)))))

(def ^:private respawnable-types
  #{:banner :wall-banner :standing-sign :wall-sign :ceiling-hanging-sign
    :wall-hanging-sign :pressure-plate :weighted-pressure-plate})

(defn possible-to-respawn-in? [^long st]
  (or (contains? respawnable-types (type-of st))
      (and (known? st)
           (zero? (bit-and (long (aget ^bytes flag-arr st)) 1))
           (not (liquid? st)))))

(defn ignited-by-lava? [^long st]
  (and (known? st) (pos? (bit-and (long (aget ^bytes flag-arr st)) 2))))

(defn solid-render? [^long st]
  (and (known? st) (pos? (bit-and (long (aget ^bytes flag-arr st)) 8))))

(defn collision-face-full-up? [^long st]
  (and (known? st) (pos? (bit-and (long (aget ^bytes flag-arr st)) 16))))

(defn randomly-ticking? [^long st]
  (and (known? st) (pos? (bit-and (long (aget ^bytes flag-arr st)) 4))))

(defn burnable? [^long st]
  (and (known? st) (pos? (long (get-in data/fire [(block-of st) :ignite] 0)))))

(defn- drop-count ^long [entry roll]
  (let [[lo hi] (:count entry [1 1])]
    (+ (long lo) (long (Math/floor (* (double (roll [:count (:item entry)])) (inc (- (long hi) (long lo)))))))))

(defn drops [^long st roll]
  (let [table (get data/drops (block-of st))
        props (props-of st)]
    (if (vector? table)
      (into []
            (keep (fn [e]
                    (when (and (not (:entity? e))
                               (every? (fn [[k v]] (= v (get props k))) (:props e))
                               (< (double (roll [:chance (:item e)])) (double (:chance e 1.0))))
                      (let [n (drop-count e roll)]
                        (when (pos? n) {:item (:item e) :count n})))))
            table)
      [])))

(defn face-sturdy? [^long st face]
  (and (known? st)
       (pos? (bit-and (long (get data/sturdy st 63)) (bit-shift-left 1 (long (dir/index face)))))))

(defn face-holds-rigid? [^long st face]
  (and (known? st)
       (pos? (bit-and (long (get data/sturdy-rigid st 63)) (bit-shift-left 1 (long (dir/index face)))))))

(defn face-holds-center? [^long st face]
  (and (known? st)
       (pos? (bit-and (long (get data/sturdy-center st 63)) (bit-shift-left 1 (long (dir/index face)))))))

(def ^:private tag-sets (atom {}))
(defn tag-set [tag]
  (or (get @tag-sets tag)
      (get (swap! tag-sets assoc tag (set (get-in data/tags ["block" tag]))) tag)))

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
         (and (or (= :vine (type-of st)) (contains? multiface-types (type-of st)))
              (boolean (vacant-face? st))))))

(defn stacked ^long [^long st]
  (let [k (stack-props (type-of st))
        n (prop-long st k)]
    (state (block-of st) (assoc (props-of st) k (keyword (str (inc n)))))))

(defn same-slab? [^long st item]
  (and (= item (block-of st)) (= :slab (shape-type (type-of st)))))

(defn slab-part [^long st] (:type (props-of st)))
(defn double-slab ^long [item] (state item {:type :double}))
