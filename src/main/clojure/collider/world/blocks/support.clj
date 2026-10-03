(ns collider.world.blocks.support
  "Support of attached blocks, and the blocks that go when unheld."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.light :as light]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.grow.vine :as vine]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.multiface :as multiface]
            [collider.world.blocks.rail :as rail]
            [collider.world.blocks.scaffold :as scaffold]))

(set! *warn-on-reflection* true)

(defn- below ^long [chunks pos] (chunk/at chunks (dir/down pos)))

(defn- above ^long [chunks pos] (chunk/at chunks (dir/up pos)))

(defn- below-void ^long [chunks pos]
  (chunk/at-void chunks (dir/down pos)))

(defn- upper? [^long st] (= :upper (:half (block/props-of st))))

(defn- hanging? [^long st] (= :true (:hanging (block/props-of st))))

(defn attached-to?
  "Returns true when the block beside pos on side holds a block on its
  face toward pos."
  [chunks pos side]
  (block/face-sturdy? (chunk/at chunks (dir/toward pos side))
                      (dir/opposite side)))

(defn- center-below? [chunks pos _st]
  (let [b (below-void chunks pos)]
    (or (neg? b) (block/face-holds-center? b :up))))

(defn- center-above? [chunks pos _st]
  (let [a (above chunks pos)]
    (and (not (block/tagged? a "unstable_bottom_center"))
         (block/face-holds-center? a :down))))

(defn- sturdy-below? [chunks pos _st]
  (block/face-sturdy? (below chunks pos) :up))

(defn- sturdy-above? [chunks pos _st]
  (block/face-sturdy? (above chunks pos) :down))

(defn- motion-below? [chunks pos _st]
  (block/blocks-motion? (below chunks pos)))

(defn- solid-below? [chunks pos _st]
  (block/legacy-solid? (below chunks pos)))

(defn- rigid-below? [chunks pos _st]
  (block/face-holds-rigid? (below chunks pos) :up))

(defn- lower-half-of? [^long below ^long st]
  (and (= (block/block-of below) (block/block-of st))
       (= :lower (:half (block/props-of below)))))

(defn- seagrass-base? [^long below]
  (and (block/face-sturdy? below :up)
       (not (block/tagged? below "cannot_support_seagrass"))))

(def ^:private crop-types
  [:crop :carrot :potato :beetroot :torchflower-crop])

(def ^:private vegetation-tags
  (merge (zipmap crop-types (repeat "supports_crops"))
         {:dry-vegetation  "supports_dry_vegetation"
          :short-dry-grass "supports_dry_vegetation"
          :tall-dry-grass  "supports_dry_vegetation"
          :stem            "supports_stem_crops"
          :attached-stem   "supports_stem_crops"
          :bamboo-stalk    "supports_bamboo"
          :bamboo-sapling  "supports_bamboo"
          :nether-wart     "supports_nether_wart"
          :azalea          "supports_azalea"
          :wither-rose     "supports_wither_rose"
          :nether-sprouts  "supports_nether_sprouts"}))

(def ^:private halved-vegetation
  {:pitcher-crop "supports_crops"
   :double-plant "supports_vegetation"
   :tall-flower  "supports_vegetation"})

(defn- halved-held? [t ^long st ^long below]
  (if (upper? st)
    (lower-half-of? below st)
    (block/tagged? below (halved-vegetation t))))

(defn- cactus-flower-held? [chunks pos ^long below]
  (or (block/tagged? below "support_override_cactus_flower")
      (center-below? chunks pos nil)))

(defn- vegetation-tag [t]
  (get vegetation-tags t "supports_vegetation"))

(defn- vegetation-held? [chunks pos ^long st]
  (let [t (block/type-of st) b (below chunks pos)]
    (cond
      (halved-vegetation t) (halved-held? t st b)
      (= :seagrass t) (seagrass-base? b)
      (= :cactus-flower t) (cactus-flower-held? chunks pos b)
      :else (block/tagged? b (vegetation-tag t)))))

(def kelp-types
  "The block types of kelp."
  #{:kelp :kelp-plant})

(defn- kelp-held? [chunks pos _st]
  (let [b (below chunks pos)]
    (or (contains? kelp-types (block/type-of b))
        (and (block/face-sturdy? b :up)
             (not (block/tagged? b "cannot_support_kelp"))))))

(defn- tall-seagrass-held? [chunks pos ^long st]
  (let [b (below chunks pos)]
    (if (upper? st) (lower-half-of? b st) (seagrass-base? b))))

(defn- fire-held? [chunks pos ^long st]
  (let [b (below chunks pos)]
    (if (= :soul-fire (block/type-of st))
      (block/tagged? b "soul_fire_base_blocks")
      (boolean
        (or (block/face-sturdy? b :up)
            (some #(block/burnable? (chunk/at chunks (mapv + pos %)))
                  dir/around))))))

(defn- mushroom-held? [chunks [x y z :as pos] _st]
  (let [b (below chunks pos)]
    (or (block/tagged? b "overrides_mushroom_light_requirement")
        (and (< (long (light/light-at chunks x y z)) 13)
             (block/solid-render? b)))))

(defn- cane-beside? [chunks pos side]
  (let [n (chunk/at chunks (dir/down (dir/toward pos side)))]
    (or (block/water? n)
        (block/tagged? n "supports_sugar_cane_adjacently"))))

(defn- cane-water? [chunks pos]
  (boolean (some #(cane-beside? chunks pos %) dir/horizontal)))

(defn- sugar-cane-held? [chunks pos ^long st]
  (let [b (below chunks pos)]
    (or (= (block/block-of b) (block/block-of st))
        (and (block/tagged? b "supports_sugar_cane")
             (cane-water? chunks pos)))))

(defn- cactus-blocked? [chunks pos side]
  (let [n (chunk/at chunks (dir/toward pos side))]
    (or (block/blocks-motion? n) (block/lava? n))))

(defn- cactus-held? [chunks pos ^long st]
  (let [b (below chunks pos)]
    (and (not (some #(cactus-blocked? chunks pos %) dir/horizontal))
         (or (= (block/block-of b) (block/block-of st))
             (block/tagged? b "supports_cactus"))
         (not (block/liquid? (above chunks pos))))))

(defn- dry-cell? [chunks pos]
  (nil? (block/liquid-class (chunk/at chunks pos))))

(defn- lily-pad-held? [chunks pos _st]
  (let [b (below chunks pos)]
    (and (or (block/water? b) (block/tagged? b "supports_lily_pad"))
         (dry-cell? chunks pos))))

(defn- frogspawn-held? [chunks pos _st]
  (and (block/holds-water-source? (below chunks pos))
       (dry-cell? chunks pos)))

(defn- snow-held? [chunks pos _st]
  (let [b (below chunks pos)]
    (cond
      (block/tagged? b "cannot_support_snow_layer") false
      (block/tagged? b "support_override_snow_layer") true
      :else (or (block/collision-face-full-up? b)
                (and (= :snow-layer (block/type-of b))
                     (= 8 (block/prop-long b :layers)))))))

(defn- carpet-held? [chunks pos _st]
  (not (zero? (below-void chunks pos))))

(defn- crop-lit? [chunks [x y z]]
  (>= (long (light/light-at chunks x y z)) 8))

(defn- facing-attached? [chunks pos st]
  (attached-to? chunks pos (dir/opposite (block/facing-of st))))

(defn- bell-held? [chunks pos ^long st]
  (let [props (block/props-of st)]
    (case (:attachment props)
      :floor (sturdy-below? chunks pos st)
      :ceiling (center-above? chunks pos st)
      (attached-to? chunks pos (:facing props)))))

(defn plant-age
  "Returns the random age a growing plant takes when placed at pos."
  [tick pos]
  (let [r (random/of-key tick pos :plant-age)]
    (keyword (str (long (Math/floor (* 25.0 r)))))))

(defn- growing-plant-held? [chunks pos ^long st]
  (let [{:keys [head body dir]}
        (block/growing-plant (block/type-of st))
        n (chunk/at chunks (dir/toward pos (dir/opposite dir)))]
    (or (contains? #{head body} (block/block-of n))
        (block/face-sturdy? n dir))))

(defn- connected-direction [^long st]
  (let [props (block/props-of st)]
    (case (:face props)
      :ceiling :down
      :floor :up
      (:facing props))))

(defn- hanging-sign-attaches? [chunks st attach-pos attach-face]
  (let [n (chunk/at chunks attach-pos)]
    (if (= :wall-hanging-sign (block/type-of n))
      (= (#{:north :south} (block/facing-of n))
         (#{:north :south} (block/facing-of st)))
      (block/face-sturdy? n attach-face))))

(defn- hanging-sign-held? [chunks pos st]
  (let [f (block/facing-of st)
        cw (dir/clockwise f)
        ccw (dir/counter-clockwise f)]
    (or (hanging-sign-attaches? chunks st (dir/toward pos cw) ccw)
        (hanging-sign-attaches? chunks st (dir/toward pos ccw) cw))))

(defn- wall-attached? [chunks pos st]
  (let [behind (dir/toward pos (dir/opposite (block/facing-of st)))]
    (block/blocks-motion? (chunk/at chunks behind))))

(defn- propagule-held? [chunks pos ^long st]
  (if (hanging? st)
    (block/tagged? (above chunks pos)
                   "supports_hanging_mangrove_propagule")
    (block/tagged? (below chunks pos) "supports_mangrove_propagule")))

(defn- lantern-held? [chunks pos ^long st]
  (if (hanging? st)
    (center-above? chunks pos st)
    (center-below? chunks pos st)))

(defn- cluster-held? [chunks pos st]
  (let [f (block/facing-of st)
        n (chunk/at chunks (dir/toward pos (dir/opposite f)))]
    (block/face-sturdy? n f)))

(defn- cocoa-held? [chunks pos st]
  (let [n (chunk/at chunks (dir/toward pos (block/facing-of st)))]
    (block/tagged? n "supports_cocoa")))

(defn- farmland-held? [chunks pos _st]
  (let [a (above chunks pos)]
    (or (not (block/blocks-motion? a))
        (block/tagged? a "maintains_farmland"))))

(defn- dirt-path-held? [chunks pos _st]
  (let [a (above chunks pos)]
    (or (not (block/blocks-motion? a))
        (= :fence-gate (block/type-of a)))))

(defn- crop-held? [chunks pos st]
  (and (crop-lit? chunks pos) (vegetation-held? chunks pos st)))

(defn- pitcher-held? [chunks pos st]
  (and (or (upper? st) (crop-lit? chunks pos))
       (vegetation-held? chunks pos st)))

(defn- connected-attached? [chunks pos st]
  (attached-to? chunks pos (dir/opposite (connected-direction st))))

(defn- sea-pickle-held? [chunks pos _st]
  (let [b (below-void chunks pos)]
    (or (neg? b) (block/face-sturdy? b :up) (block/full-cube? b))))

(defn- spore-blossom-held? [chunks pos st]
  (and (center-above? chunks pos st)
       (not (block/water? (chunk/at chunks pos)))))

(defn- rail-held? [chunks pos _st]
  (let [b (below-void chunks pos)]
    (or (neg? b) (block/face-holds-rigid? b :up))))

(defn- plate-held? [chunks pos _st]
  (let [b (below-void chunks pos)]
    (or (neg? b)
        (block/face-holds-rigid? b :up)
        (block/face-holds-center? b :up))))

(defn- wire-held? [chunks pos _st]
  (let [b (below-void chunks pos)]
    (or (neg? b)
        (block/face-sturdy? b :up)
        (= :hopper (block/block-of b)))))

(defn- vine-held? [chunks pos st] (pos? (vine/updated chunks pos st)))

(defn- multiface-held? [chunks pos st]
  (boolean (seq (block/faces-of (multiface/updated chunks pos st)))))

(defn- scaffold-held? [chunks pos _st]
  (< (scaffold/distance chunks pos) 7))

(defn- moss-carpet-held? [chunks pos st]
  (pos? (moss/carpet-reshaped chunks pos st)))

(def ^:private support-rules
  [[[:torch :redstone-torch :candle] center-below?]
   [[:wall-torch :redstone-wall-torch :ladder] facing-attached?]
   [[:standing-sign :banner] motion-below?]
   [[:wall-banner :wall-sign] wall-attached?]
   [[:ceiling-hanging-sign] center-above?]
   [[:wall-hanging-sign] hanging-sign-held?]
   [kelp-types kelp-held?]
   [[:tall-seagrass] tall-seagrass-held?]
   [[:azalea :wither-rose :nether-sprouts :nether-fungus
     :nether-roots]
    vegetation-held?]
   [[:mangrove-propagule] propagule-held?]
   [[:chorus-flower :chorus-plant] chorus/supported?]
   [[:fire :soul-fire] fire-held?]
   [[:mushroom] mushroom-held?]
   [[:sugar-cane] sugar-cane-held?]
   [[:cactus] cactus-held?]
   [[:lily-pad] lily-pad-held?]
   [[:frogspawn] frogspawn-held?]
   [[:snow-layer] snow-held?]
   [[:wool-carpet :carpet] carpet-held?]
   [[:leaf-litter :coral-plant :coral-fan :base-coral-plant
     :base-coral-fan]
    sturdy-below?]
   [[:lantern :weathering-lantern] lantern-held?]
   [[:bell] bell-held?]
   [[:cake :candle-cake] solid-below?]
   [[:grindstone] (constantly true)]
   [[:button :lever] connected-attached?]
   [block/growing-plant-types growing-plant-held?]
   [[:amethyst-cluster] cluster-held?]
   [[:sea-pickle] sea-pickle-held?]
   [[:cocoa] cocoa-held?]
   [[:spore-blossom] spore-blossom-held?]
   [[:coral-wall-fan :base-coral-wall-fan :trip-wire-hook]
    facing-attached?]
   [[:repeater :comparator] rigid-below?]
   [[:vine] vine-held?]
   [block/multiface-types multiface-held?]
   [[:scaffolding] scaffold-held?]
   [[:mossy-carpet] moss-carpet-held?]
   [[:hanging-moss] moss/hanging-supported?]
   [[:pointed-dripstone :sulfur-spike] dripstone/supported?]
   [[:big-dripleaf :big-dripleaf-stem :small-dripleaf]
    dripleaf/supported?]
   [[:hanging-roots] sturdy-above?]
   [[:farmland] farmland-held?]
   [[:dirt-path] dirt-path-held?]
   [crop-types crop-held?]
   [[:pitcher-crop] pitcher-held?]
   [rail/rail-types rail-held?]
   [[:pressure-plate :weighted-pressure-plate] plate-held?]
   [[:redstone-wire] wire-held?]])

(def ^:private supports
  (into {} (for [[classes f] support-rules, k classes] [k f])))

(defn supported?
  "Returns true when the blocks around pos hold st where it stands."
  [chunks pos st]
  (let [st (long st)]
    (if-let [f (supports (block/type-of st))]
      (f chunks pos st)
      (or (not (block/needs-support? st))
          (vegetation-held? chunks pos st)))))

(def ^:private dirt-types #{:farmland :dirt-path})

(defn gone-state
  "Returns the state a block leaves when it loses its support."
  ^long [^long st]
  (if (contains? dirt-types (block/type-of st))
    (block/state :dirt)
    (block/emptied st)))

(defn gone
  "Returns the change of block st at p when it loses its support."
  [p ^long st]
  (if (contains? dirt-types (block/type-of st))
    [p (block/state :dirt)]
    (block/destroyed p st)))

(def ^:private tick-sides
  (merge {:sugar-cane some? :cactus some? :chorus-plant some?
          :bamboo-stalk some? :hanging-moss some?
          :chorus-flower #(and (some? %) (not= :up %))
          :farmland #{:up} :dirt-path #{:up}
          :twisting-vines #{:down} :twisting-vines-plant #{:down}}
         (zipmap [:weeping-vines :weeping-vines-plant :cave-vines
                  :cave-vines-plant]
                 (repeat #{:up}))))

(defn- behind-side [st] (dir/opposite (block/facing-of st)))

(defn- attach-side [st] (dir/opposite (connected-direction st)))

(defn- lantern-side [st] (if (hanging? st) :up :down))

(defn- bell-side [st]
  (case (:attachment (block/props-of st))
    :floor :down
    :ceiling :up
    :double_wall nil
    (block/facing-of st)))

(def ^:private shape-sides
  (merge
    (zipmap [:torch :redstone-torch :standing-sign :banner
             :cake :candle-cake :pressure-plate
             :weighted-pressure-plate :base-coral-plant
             :base-coral-fan]
            (repeat (constantly :down)))
    (zipmap [:ceiling-hanging-sign :hanging-roots :spore-blossom]
            (repeat (constantly :up)))
    (zipmap [:wall-torch :redstone-wall-torch :wall-sign :wall-banner
             :ladder :trip-wire-hook :amethyst-cluster
             :base-coral-wall-fan]
            (repeat behind-side))
    {:cocoa block/facing-of :button attach-side :lever attach-side
     :lantern lantern-side :weathering-lantern lantern-side
     :bell bell-side :wall-hanging-sign (constantly nil)
     :candle (constantly nil)}))

(defn- shape-side? [st side]
  (let [t (block/type-of st)]
    (and (some? side)
         (if-let [f (shape-sides t)]
           (= side (f st))
           (or (not= :vine t) (not= :down side))))))

(defn- popped? [chunks p st side]
  (if (contains? block/multiface-types (block/type-of st))
    (and (some? side)
         (empty? (block/faces-of
                   (multiface/sides-updated chunks p st #{side}))))
    (and (shape-side? st side) (not (supported? chunks p st)))))

(defn- wake [chunks _dim tick p _old side]
  (let [st (chunk/at chunks p)]
    (if-let [sides (tick-sides (block/type-of st))]
      (when (and (sides side) (not (supported? chunks p st)))
        (inc (long tick)))
      (when (popped? chunks p st side) :neighbor))))

(def ^:private removed-types
  #{:repeater :comparator :redstone-wire})

(defn- unsupported [chunks p _ctx]
  (let [st (chunk/at chunks p)]
    (when-not (supported? chunks p st)
      (if (removed-types (block/type-of st))
        [[p (block/emptied st) [[:drop st]]]]
        [(gone p st)]))))

(def ^:private popped-types
  #{:mossy-carpet})

(def ^:private lit-types
  (into #{:mushroom :pitcher-crop} crop-types))

(def rule
  "The block rule of attached blocks that go when unheld."
  {:name    :support
   :match?  (fn [_chunks st _p]
              (or (block/attached? st)
                  (contains? popped-types (block/type-of st))))
   :lit?    (fn [st _ctx] (contains? lit-types (block/type-of st)))
   :pass    (fn [st]
              (if (removed-types (block/type-of st))
                :neighbor
                :shape))
   :wake    wake
   :reshape unsupported
   :due     unsupported})

(def stem-fruits
  "The fruit, the attached stem and the soil tag of each fruit stem."
  {:pumpkin-stem
   [:pumpkin :attached-pumpkin-stem "supports_pumpkin_stem_fruit"]
   :melon-stem
   [:melon :attached-melon-stem "supports_melon_stem_fruit"]})

(def ^:private attached-stems
  (into {} (for [[stem [fruit attached _]] stem-fruits]
             [attached [fruit stem]])))

(defn- fruitless? [chunks p ^long st]
  (let [[fruit _] (attached-stems (block/block-of st))
        beside (dir/toward p (block/facing-of st))]
    (not= fruit (block/block-of (chunk/at chunks beside)))))

(defn- detached ^long [^long st]
  (let [[_ stem] (attached-stems (block/block-of st))]
    (block/state stem {:age :7})))

(defn- attached-due [chunks p ctx]
  (let [st (chunk/at chunks p)]
    (cond
      (and (= (:side ctx) (block/facing-of st))
           (fruitless? chunks p st))
      [[p (detached st)]]
      (not (supported? chunks p st))
      [(gone p st)])))

(def attached-stem-rule
  "The block rule of a stem attached to its fruit."
  {:name    :attached-stem
   :match?  (fn [_chunks st _p]
              (= :attached-stem (block/type-of st)))
   :wake    (fn [_chunks _dim _tick _p _old side]
              (when side :neighbor))
   :reshape attached-due})
