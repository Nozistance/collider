(ns collider.world.blocks.support
  "Support of attached blocks and the state they take when placed."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.blocks.campfire :as campfire]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dragonegg :as dragonegg]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.light :as light]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.mushroom :as mushroom]))

(set! *warn-on-reflection* true)

(defn- state-at ^long [chunks [_ y _ :as pos]]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (chunk/chunks-get-block chunks pos)
      -1)))

(defn- water? [st] (block/water? st))

(defn- water-source? [st]
  (or (block/waterlogged? st) (block/water-source? st)))

(def ^:private kelp-types #{:kelp :kelp-plant})

(defn- kelp-supported? [below]
  (or (contains? kelp-types (block/type-of below))
      (and (block/face-sturdy? below :up) (not (block/tagged? (max 0 below) "cannot_support_kelp")))))

(defn- seagrass-supported? [below]
  (and (not (neg? below))
       (block/face-sturdy? below :up)
       (not (block/tagged? below "cannot_support_seagrass"))))

(defn- fire-supported? [chunks pos st below]
  (if (= :soul-fire (block/type-of (long st)))
    (block/tagged? (max 0 below) "soul_fire_base_blocks")
    (boolean
      (or (and (not (neg? below)) (block/face-sturdy? below :up))
          (some (fn [d]
                  (let [n (state-at chunks (mapv + pos d))]
                    (and (pos? n) (block/burnable? n))))
                dir/around)))))

(defn- lower-half-of? [below st]
  (and (= (block/block-of below) (block/block-of st))
       (= :lower (:half (block/props-of below)))))

(defn- mushroom-supported? [chunks [x y z] below]
  (or (block/tagged? below "overrides_mushroom_light_requirement")
      (and (< (long (light/light-at chunks x y z)) 13)
           (block/solid-render? below))))

(defn- cane-beside? [chunks [x y z] [dx _ dz]]
  (let [q [(+ (long x) (long dx)) (dec (long y))
           (+ (long z) (long dz))]
        n (state-at chunks q)]
    (and (pos? n)
         (or (water? n)
             (block/tagged? n "supports_sugar_cane_adjacently")))))

(defn- sugar-cane-supported? [chunks pos st below]
  (or (= (block/block-of below) (block/block-of st))
      (and (block/tagged? below "supports_sugar_cane")
           (boolean
            (some #(cane-beside? chunks pos %)
                  (vals dir/horizontal-offset))))))

(defn- cactus-blocked? [chunks pos d]
  (let [n (state-at chunks (mapv + pos d))]
    (and (pos? n)
         (or (block/blocks-motion? n)
             (= :lava (block/liquid-class n))))))

(defn- cactus-supported? [chunks pos st below]
  (and (not (some #(cactus-blocked? chunks pos %)
                  (vals dir/horizontal-offset)))
       (or (= (block/block-of below) (block/block-of st))
           (block/tagged? below "supports_cactus"))
       (not (block/liquid?
             (max 0 (state-at chunks (mapv + pos [0 1 0])))))))

(defn- lily-pad-supported? [chunks pos below]
  (and (or (water? below) (block/tagged? below "supports_lily_pad"))
       (not (water? (max 0 (state-at chunks (mapv + pos [0 1 0])))))))

(defn- snow-supported? [below]
  (cond
    (block/tagged? below "cannot_support_snow_layer") false
    (block/tagged? below "support_override_snow_layer") true
    :else (or (block/collision-face-full-up? below)
              (and (= :snow-layer (block/type-of below)) (= :8 (:layers (block/props-of below)))))))

(defn- holds-center-below? [below]
  (or (neg? below) (block/face-holds-center? below :up)))

(defn- holds-center-above? [above]
  (and (not (neg? above))
       (not (block/tagged? above "unstable_bottom_center"))
       (block/face-holds-center? above :down)))

(defn- crop-lit? [chunks [x y z]]
  (>= (long (light/light-at chunks x y z)) 8))

(defn- attached-to? [chunks pos dir]
  (let [n (state-at chunks (mapv + pos (dir/horizontal-offset dir)))]
    (and (not (neg? n)) (block/face-sturdy? n (dir/opposite dir)))))

(defn- bell-supported? [chunks pos st below above]
  (let [props (block/props-of st)]
    (case (:attachment props)
      :floor (and (not (neg? below)) (block/face-sturdy? below :up))
      :ceiling (holds-center-above? above)
      (attached-to? chunks pos (:facing props)))))

(def ^:private vegetation-tags
  {:dry-vegetation "supports_dry_vegetation" :short-dry-grass "supports_dry_vegetation"
   :tall-dry-grass "supports_dry_vegetation"
   :crop           "supports_crops" :carrot "supports_crops" :potato "supports_crops"
   :beetroot       "supports_crops" :torchflower-crop "supports_crops"
   :stem           "supports_stem_crops" :attached-stem "supports_stem_crops"
   :bamboo-stalk   "supports_bamboo" :bamboo-sapling "supports_bamboo"
   :nether-wart    "supports_nether_wart" :azalea "supports_azalea"
   :wither-rose    "supports_wither_rose" :nether-sprouts "supports_nether_sprouts"})

(def ^:private halved-vegetation
  {:pitcher-crop "supports_crops" :double-plant "supports_vegetation" :tall-flower "supports_vegetation"})

(defn- halved-supported? [t st below]
  (if (= :upper (:half (block/props-of st)))
    (lower-half-of? below st)
    (block/tagged? below (halved-vegetation t))))

(defn- cactus-flower-supported? [below]
  (or (block/tagged? below "support_override_cactus_flower")
      (holds-center-below? below)))

(defn- vegetation-supported? [t st below]
  (let [tag (get vegetation-tags t "supports_vegetation")]
    (cond
      (halved-vegetation t) (halved-supported? t st below)
      (= :seagrass t) (seagrass-supported? below)
      (= :cactus-flower t) (cactus-flower-supported? below)
      :else (block/tagged? below tag))))

(defn plant-age [tick pos]
  (keyword (str (long (Math/floor (* 25.0 (random/of-key tick pos :plant-age)))))))

(defn- growing-plant-supported? [chunks pos st]
  (let [{:keys [head body dir]} (block/growing-plant (block/type-of st))
        n (state-at chunks (mapv - pos (dir/offset dir)))]
    (and (not (neg? n))
         (or (contains? #{head body} (block/block-of n)) (block/face-sturdy? n dir)))))

(declare vine-updated multiface-updated scaffold-distance attachable?)

(defn- connected-direction
  [^long st]
  (let [props (block/props-of st)]
    (case (:face props)
      :ceiling :down
      :floor :up
      (:facing props))))

(defn- hanging-sign-attaches? [chunks st attach-pos attach-face]
  (let [n (state-at chunks attach-pos)]
    (and (not (neg? n))
         (if (= :wall-hanging-sign (block/type-of n))
           (= (#{:north :south} (block/facing-of n)) (#{:north :south} (block/facing-of st)))
           (block/face-sturdy? n attach-face)))))

(defn- hanging-sign-held? [chunks pos st]
  (let [f (block/facing-of st) cw (dir/clockwise f) ccw (dir/counter-clockwise f)]
    (or (hanging-sign-attaches? chunks st (mapv + pos (dir/horizontal-offset cw)) ccw)
        (hanging-sign-attaches? chunks st (mapv + pos (dir/horizontal-offset ccw)) cw))))

(defn- wall-attached? [chunks pos st]
  (block/blocks-motion? (max 0 (state-at chunks (mapv + pos (dir/horizontal-offset (dir/opposite (block/facing-of st))))))))

(defn- tall-seagrass-supported? [st below]
  (if (= :upper (:half (block/props-of st)))
    (lower-half-of? below st)
    (seagrass-supported? below)))

(defn- propagule-supported? [st below above]
  (if (= :true (:hanging (block/props-of st)))
    (block/tagged? (max 0 above) "supports_hanging_mangrove_propagule")
    (block/tagged? (max 0 below) "supports_mangrove_propagule")))

(defn- lantern-supported? [st below above]
  (if (= :true (:hanging (block/props-of st)))
    (holds-center-above? above)
    (holds-center-below? below)))

(defn- cluster-supported? [chunks pos st]
  (let [f (block/facing-of st)
        n (state-at chunks (mapv + pos (dir/offset (dir/opposite f))))]
    (and (not (neg? n)) (block/face-sturdy? n f))))

(defn- cocoa-supported? [chunks pos st]
  (block/tagged? (max 0 (state-at chunks (mapv + pos (dir/horizontal-offset (block/facing-of st))))) "supports_cocoa"))

(defn- farmland-supported? [above]
  (or (not (block/blocks-motion? (max 0 above))) (block/tagged? (max 0 above) "maintains_farmland")))

(defn- dirt-path-supported? [above]
  (or (not (block/blocks-motion? (max 0 above))) (= :fence-gate (block/type-of (max 0 above)))))

(defn- crop-supported? [chunks pos st below]
  (and (crop-lit? chunks pos) (vegetation-supported? (block/type-of st) st below)))

(defn- pitcher-supported? [chunks pos st below]
  (and (or (= :upper (:half (block/props-of st))) (crop-lit? chunks pos))
       (vegetation-supported? (block/type-of st) st below)))

(defn- sturdy-face? [st dir]
  (and (not (neg? st)) (block/face-sturdy? st dir)))

(defn- facing-attached? [chunks pos st]
  (attached-to? chunks pos (dir/opposite (block/facing-of st))))

(defn- connected-attached? [chunks pos st]
  (attachable? chunks pos (dir/opposite (connected-direction st))))

(defn- sea-pickle-supported? [below]
  (or (neg? below)
      (block/face-sturdy? below :up)
      (block/full-cube? below)))

(defn- spore-blossom-supported? [chunks pos above]
  (and (holds-center-above? above)
       (not (water? (state-at chunks pos)))))

(defn- rail-supported? [below]
  (or (neg? below) (block/face-holds-rigid? below :up)))

(defn- plate-supported? [below]
  (or (neg? below)
      (block/face-holds-rigid? below :up)
      (block/face-holds-center? below :up)))

(defn- wire-supported? [below]
  (or (neg? below)
      (block/face-sturdy? below :up)
      (= :hopper (block/block-of below))))

(def ^:private growing-plant-types
  [:weeping-vines :weeping-vines-plant :twisting-vines
   :twisting-vines-plant :cave-vines :cave-vines-plant])

(def ^:private support-rules
  [[[:torch :redstone-torch :candle]
    (fn [_c _p _st below _a] (holds-center-below? below))]
   [[:wall-torch :redstone-wall-torch :ladder]
    (fn [c p st _b _a] (facing-attached? c p st))]
   [[:standing-sign :banner]
    (fn [_c _p _st below _a] (block/blocks-motion? (max 0 below)))]
   [[:wall-banner :wall-sign]
    (fn [c p st _b _a] (wall-attached? c p st))]
   [[:ceiling-hanging-sign]
    (fn [_c _p _st _b above] (holds-center-above? above))]
   [[:wall-hanging-sign]
    (fn [c p st _b _a] (hanging-sign-held? c p st))]
   [[:kelp :kelp-plant]
    (fn [_c _p _st below _a] (kelp-supported? below))]
   [[:tall-seagrass]
    (fn [_c _p st below _a] (tall-seagrass-supported? st below))]
   [[:azalea :wither-rose :nether-sprouts :nether-fungus
     :nether-roots]
    (fn [_c _p st below _a]
      (vegetation-supported? (block/type-of st) st below))]
   [[:mangrove-propagule]
    (fn [_c _p st below above] (propagule-supported? st below above))]
   [[:chorus-flower :chorus-plant]
    (fn [c p st _b _a] (chorus/supported? c p st))]
   [[:fire :soul-fire]
    (fn [c p st below _a] (fire-supported? c p st below))]
   [[:mushroom]
    (fn [c p _st below _a] (mushroom-supported? c p below))]
   [[:sugar-cane]
    (fn [c p st below _a] (sugar-cane-supported? c p st below))]
   [[:cactus]
    (fn [c p st below _a] (cactus-supported? c p st below))]
   [[:lily-pad]
    (fn [c p _st below _a] (lily-pad-supported? c p below))]
   [[:snow-layer]
    (fn [_c _p _st below _a] (snow-supported? below))]
   [[:wool-carpet :carpet]
    (fn [_c _p _st below _a] (not (zero? below)))]
   [[:leaf-litter :coral-plant :coral-fan :base-coral-plant
     :base-coral-fan]
    (fn [_c _p _st below _a] (sturdy-face? below :up))]
   [[:lantern :weathering-lantern]
    (fn [_c _p st below above] (lantern-supported? st below above))]
   [[:bell]
    (fn [c p st below above] (bell-supported? c p st below above))]
   [[:cake :candle-cake]
    (fn [_c _p _st below _a] (block/legacy-solid? (max 0 below)))]
   [[:grindstone]
    (fn [_c _p _st _b _a] true)]
   [[:button :lever]
    (fn [c p st _b _a] (connected-attached? c p st))]
   [growing-plant-types
    (fn [c p st _b _a] (growing-plant-supported? c p st))]
   [[:amethyst-cluster]
    (fn [c p st _b _a] (cluster-supported? c p st))]
   [[:sea-pickle]
    (fn [_c _p _st below _a] (sea-pickle-supported? below))]
   [[:cocoa]
    (fn [c p st _b _a] (cocoa-supported? c p st))]
   [[:spore-blossom]
    (fn [c p _st _b above] (spore-blossom-supported? c p above))]
   [[:coral-wall-fan :base-coral-wall-fan]
    (fn [c p st _b _a] (facing-attached? c p st))]
   [[:vine]
    (fn [c p st _b _a] (pos? (vine-updated c p st)))]
   [[:glow-lichen :multiface :sculk-vein]
    (fn [c p st _b _a] (pos? (multiface-updated c p st)))]
   [[:scaffolding]
    (fn [c p _st _b _a] (< (scaffold-distance c p) 7))]
   [[:mossy-carpet]
    (fn [c p st _b _a] (moss/carpet-supported? c p st))]
   [[:hanging-moss]
    (fn [c p st _b _a] (moss/hanging-supported? c p st))]
   [[:pointed-dripstone :sulfur-spike]
    (fn [c p st _b _a] (dripstone/supported? c p st))]
   [[:big-dripleaf :big-dripleaf-stem :small-dripleaf]
    (fn [c p st _b _a] (dripleaf/supported? c p st))]
   [[:hanging-roots]
    (fn [_c _p _st _b above] (sturdy-face? above :down))]
   [[:farmland]
    (fn [_c _p _st _b above] (farmland-supported? above))]
   [[:dirt-path]
    (fn [_c _p _st _b above] (dirt-path-supported? above))]
   [[:crop :carrot :potato :beetroot :torchflower-crop]
    (fn [c p st below _a] (crop-supported? c p st below))]
   [[:pitcher-crop]
    (fn [c p st below _a] (pitcher-supported? c p st below))]
   [[:rail :powered-rail :detector-rail]
    (fn [_c _p _st below _a] (rail-supported? below))]
   [[:pressure-plate :weighted-pressure-plate]
    (fn [_c _p _st below _a] (plate-supported? below))]
   [[:redstone-wire]
    (fn [_c _p _st below _a] (wire-supported? below))]])

(def ^:private supports
  (into {} (for [[classes f] support-rules, k classes] [k f])))

(defn- default-supported? [t st below]
  (if (block/needs-support? st) (vegetation-supported? t st below) true))

(defn supported? [chunks pos st]
  (let [st (long st) t (block/type-of st)
        below (state-at chunks (mapv + pos [0 -1 0]))
        above (state-at chunks (mapv + pos [0 1 0]))]
    (if-let [f (supports t)]
      (f chunks pos st below above)
      (default-supported? t st below))))

(defn- pick [chunks pos states]
  (first (filter #(supported? chunks pos %) states)))

(defn- lantern-fitted [chunks pos st {:keys [pitch]}]
  (let [self (block/block-of st) props (block/props-of st)
        standing (block/state self (assoc props :hanging :false))
        hanging (block/state self (assoc props :hanging :true))
        order (if (pos? (double pitch))
                [standing hanging]
                [hanging standing])]
    (pick chunks pos order)))

(defn- bell-fitted [chunks pos st {:keys [face yaw]}]
  (let [self (block/block-of st) props (block/props-of st)
        face (long face)
        with (fn [attachment facing] (block/state self (assoc props :attachment attachment :facing facing)))]
    (if (<= face 1)
      (pick chunks pos [(with (if (= 0 face) :ceiling :floor) (dir/player-direction yaw))])
      (let [facing (dir/opposite (dir/face-facing face))
            axis (if (#{:north :south} facing) [:north :south] [:west :east])
            double? (every? #(attached-to? chunks pos %) axis)
            wall (with (if double? :double_wall :single_wall) facing)
            below (state-at chunks (mapv + pos [0 -1 0]))
            fallback (with (if (and (not (neg? below)) (block/face-sturdy? below :up)) :floor :ceiling) facing)]
        (pick chunks pos [wall fallback])))))

(defn gone-state ^long [^long st]
  (if (#{:farmland :dirt-path} (block/type-of st)) (block/state :dirt) (block/emptied st)))

(defn look-order
  "Returns the six directions as a player sees them, nearest first.
  The player looks along yaw and pitch."
  [yaw pitch]
  (let [p (Math/toRadians (double pitch)) y (Math/toRadians (- (double yaw)))
        ps (Math/sin p) pc (Math/cos p) ys (Math/sin y) yc (Math/cos y)
        ax (if (pos? ys) :east :west) ay (if (neg? ps) :up :down) az (if (pos? yc) :south :north)
        xy (Math/abs ys) ym (Math/abs ps) zy (Math/abs yc)
        xm (* xy pc) zm (* zy pc)
        [a b c] (if (> xy zy)
                  (cond (> ym xm) [ay ax az] (> zm ym) [ax az ay] :else [ax ay az])
                  (cond (> ym zm) [ay az ax] (> xm ym) [az ax ay] :else [az ay ax]))]
    [a b c (dir/opposite c) (dir/opposite b) (dir/opposite a)]))

(defn- horizontal-look-order [yaw]
  (filterv #(contains? dir/horizontal-offset %) (look-order yaw 0.0)))

(defn attachable? [chunks pos dir]
  (let [n (state-at chunks (mapv + pos (dir/offset dir)))]
    (and (not (neg? n)) (block/face-sturdy? n (dir/opposite dir)))))

(defn- vine-face-held? [chunks pos st dir]
  (and (not= :down dir)
       (or (attachable? chunks pos dir)
           (and (contains? dir/horizontal-offset dir)
                (let [above (state-at chunks (mapv + pos [0 1 0]))]
                  (and (= (block/block-of above) (block/block-of st))
                       (= :true (get (block/props-of above) dir))))))))

(defn- vine-face-kept [chunks pos st m dir]
  (if (= :true (get m dir))
    (let [held? (vine-face-held? chunks pos st dir)]
      (assoc m dir (if held? :true :false)))
    m))

(defn vine-updated ^long [chunks pos ^long st]
  (let [step (fn [m dir] (vine-face-kept chunks pos st m dir))
        props' (reduce step (block/props-of st)
                       [:up :north :south :west :east])
        st' (block/state (block/block-of st) props')]
    (if (seq (block/faces-of st')) st' 0)))

(defn- multiface-face-kept [chunks pos m dir]
  (if (and (= :true (get m dir)) (not (attachable? chunks pos dir)))
    (assoc m dir :false)
    m))

(defn multiface-updated ^long [chunks pos ^long st]
  (let [step (fn [m dir] (multiface-face-kept chunks pos m dir))
        props' (reduce step (block/props-of st) block/face-props)
        st' (block/state (block/block-of st) props')]
    (if (seq (block/faces-of st')) st' (block/emptied st))))

(defn scaffold-distance ^long [chunks [x y z :as pos]]
  (let [below (state-at chunks [x (dec (long y)) z])
        scaffold? (fn [st] (= :scaffolding (block/type-of (max 0 st))))
        dist (fn [st] (block/prop-long st :distance))]
    (if (and (not (neg? below)) (not (scaffold? below)) (block/face-sturdy? below :up))
      0
      (reduce (fn [d dir]
                (let [n (state-at chunks (mapv + pos (dir/horizontal-offset dir)))]
                  (if (scaffold? n) (min (long d) (inc (dist n))) d)))
              (if (scaffold? below) (dist below) 7)
              dir/horizontal))))

(defn scaffold-state ^long [chunks pos ^long st]
  (let [d (scaffold-distance chunks pos)
        below (state-at chunks (mapv + pos [0 -1 0]))
        on-scaffold? (= :scaffolding (block/type-of (max 0 below)))
        bottom (if (and (pos? d) (not on-scaffold?)) :true :false)
        props (assoc (block/props-of st)
                     :distance (keyword (str d))
                     :bottom bottom)]
    (block/state (block/block-of st) props)))

(defn- bamboo-fitted [chunks pos _st _opts]
  (let [below (state-at chunks (mapv + pos [0 -1 0]))
        above (state-at chunks (mapv + pos [0 1 0]))
        age (fn [^long n] (:age (block/props-of n)))]
    (when (and (let [cur (max 0 (state-at chunks pos))]
                 (nil? (block/liquid-class cur)))
               (block/tagged? (max 0 below) "supports_bamboo"))
      (case (block/block-of (max 0 below))
        :bamboo-sapling (block/state :bamboo {:age :0})
        :bamboo (block/state :bamboo {:age (if (= :0 (age below)) :0 :1)})
        (if (= :bamboo (block/block-of (max 0 above)))
          (block/state :bamboo {:age (age above)})
          (block/state :bamboo-sapling))))))

(defn- cocoa-fitted [chunks pos st {:keys [yaw]}]
  (let [self (block/block-of st) props (block/props-of st)
        facing (fn [dir]
                 (block/state self (assoc props :facing dir)))]
    (pick chunks pos (map facing (horizontal-look-order yaw)))))

(defn- vine-fitted [chunks pos st {:keys [yaw pitch]}]
  (let [cur (state-at chunks pos)
        base (if (= (block/block-of cur) (block/block-of st)) cur st)
        free (fn [dir]
               (and (not= :down dir)
                    (= :false (get (block/props-of base) dir))
                    (vine-face-held? chunks pos base dir)))]
    (if-let [dir (first (filter free (look-order yaw pitch)))]
      (block/state (block/block-of base) (assoc (block/props-of base) dir :true))
      (when (= base cur) cur))))

(defn- multiface-fitted [chunks pos st {:keys [yaw pitch]}]
  (let [cur (state-at chunks pos)
        same? (= (block/block-of cur) (block/block-of st))
        base (cond same? cur
                   (water-source? (max 0 cur)) (block/with-water st)
                   :else st)
        free (fn [dir] (and (= :false (get (block/props-of base) dir)) (attachable? chunks pos dir)))]
    (when-let [dir (first (filter free (look-order yaw pitch)))]
      (block/state (block/block-of base) (assoc (block/props-of base) dir :true)))))

(defn- rod-fitted [chunks pos st {:keys [face]}]
  (let [f (block/facing-of st)
        clicked (state-at chunks (mapv - pos (dir/face-offset face)))]
    (if (and (= (block/block-of clicked) (block/block-of st)) (= f (block/facing-of clicked)))
      (block/state (block/block-of st) (assoc (block/props-of st) :facing (dir/opposite f)))
      st)))

(defn- placement-order [yaw pitch face replacing?]
  (let [order (look-order yaw pitch) first-dir (dir/opposite (dir/from-index face))]
    (if replacing? order (into [first-dir] (remove #{first-dir}) order))))

(defn- standing-or-wall [order standing wall-state]
  (first (for [dir order :when (not= :up dir)
               :let [cand (if (= :down dir) standing wall-state)]
               :when cand]
           cand)))

(defn- first-wall [order fit? on-wall]
  (first (for [dir order
               :when (contains? dir/horizontal-offset dir)
               :when (fit? dir)]
           (on-wall dir))))

(defn- wall-state-of [st dir]
  (let [wall (block/wall-block (block/block-of st))]
    (block/state wall {:facing (dir/opposite dir)})))

(defn- standing-or-wall-fitted [chunks pos st {:keys [yaw pitch face replacing?]}]
  (let [order (placement-order yaw pitch face replacing?)
        on-wall (fn [dir] (wall-state-of st dir))
        held? (fn [dir] (supported? chunks pos (on-wall dir)))
        standing (when (supported? chunks pos st) st)
        wall (first-wall order held? on-wall)]
    (standing-or-wall order standing wall)))

(defn- skull-wall-free? [chunks pos dir]
  (let [q (mapv + pos (dir/horizontal-offset dir))]
    (not (block/can-be-replaced? (max 0 (state-at chunks q))))))

(defn- skull-fitted [chunks pos st {:keys [yaw pitch face replacing?]}]
  (let [order (placement-order yaw pitch face replacing?)
        on-wall (fn [dir] (wall-state-of st dir))
        free? (fn [dir] (skull-wall-free? chunks pos dir))]
    (standing-or-wall order st (first-wall order free? on-wall))))

(defn- growing-plant-fitted [chunks pos st {:keys [tick]}]
  (let [{:keys [head body dir]} (block/growing-plant (block/type-of st))
        n (max 0 (state-at chunks (mapv + pos (dir/offset dir))))
        st' (if (contains? #{head body} (block/block-of n))
              (block/state body)
              (block/state head {:age (plant-age tick pos)}))]
    (when (supported? chunks pos st') st')))

(def ^:private north-south #{:north :south})

(defn- ns-facing? [st] (contains? north-south (block/facing-of st)))

(defn- ceiling-sign-axis [above]
  (let [r (block/prop-long above :rotation)]
    (when (zero? (mod r 4)) (contains? #{0 8} r))))

(defn- above-sign-axis [above]
  (case (block/type-of (max 0 above))
    :wall-hanging-sign (ns-facing? above)
    :ceiling-hanging-sign (ceiling-sign-axis above)
    nil))

(defn- sign-middle? [above axis ns? sneaking?]
  (and (not (and (some? axis) (= axis ns?) (not sneaking?)))
       (or sneaking?
           (neg? above)
           (not (block/face-sturdy? above :down)))))

(def ^:private sign-turns {:south 0 :west 4 :north 8 :east 12})

(defn- ceiling-sign [chunks pos st yaw sneaking?]
  (let [above (state-at chunks (mapv + pos [0 1 0]))
        dir (dir/player-direction yaw)
        ns? (contains? north-south dir)
        axis (above-sign-axis above)
        middle? (sign-middle? above axis ns? sneaking?)
        rotation (if middle?
                   (:rotation (block/props-of st))
                   (keyword (str (sign-turns (dir/opposite dir)))))
        props (assoc (block/props-of st)
                     :attached (if middle? :true :false)
                     :rotation rotation)]
    (block/state (block/block-of st) props)))

(defn- hanging-sign-fitted [chunks pos st opts]
  (let [{:keys [yaw pitch sneaking?]} opts
        order (look-order yaw pitch)
        on-wall (fn [dir] (wall-state-of st dir))
        held? (fn [dir] (hanging-sign-held? chunks pos (on-wall dir)))
        wall-state (first-wall order held? on-wall)
        ceiling (ceiling-sign chunks pos st yaw sneaking?)
        up (when (supported? chunks pos ceiling) ceiling)]
    (first (for [dir order :when (not= :down dir)
                 :let [cand (if (= :up dir) up wall-state)]
                 :when cand]
             cand))))

(defn place-order [face yaw pitch replacing?]
  (let [order (look-order yaw pitch)]
    (if replacing?
      order
      (let [first-dir (dir/opposite (nth dir/six (long face)))]
        (into [first-dir] (remove #(= % first-dir)) order)))))

(defn- face-attached-state [st dir horizontal]
  (let [wall? (not (contains? #{:up :down} dir))
        face (cond wall? :wall (= :up dir) :ceiling :else :floor)
        facing (if wall? (dir/opposite dir) horizontal)
        props (assoc (block/props-of st) :face face :facing facing)]
    (block/state (block/block-of st) props)))

(defn- face-attached-fitted [chunks pos st opts]
  (let [{:keys [face yaw pitch replacing?]} opts
        horizontal (dir/player-direction yaw)
        candidate (fn [dir] (face-attached-state st dir horizontal))]
    (first (for [dir (place-order face yaw pitch replacing?)
                 :let [cand (candidate dir)]
                 :when (supported? chunks pos cand)]
             cand))))

(defn- carpet-fitted [chunks pos ^long st _opts]
  (let [st' (moss/carpet-updated chunks pos st true)]
    (when (moss/carpet-supported? chunks pos st') st')))

(defn- scaffold-fitted [chunks pos st _opts]
  (when (< (scaffold-distance chunks pos) 7)
    (scaffold-state chunks pos st)))

(def ^:private fit-rules
  [[[:button :lever :grindstone] face-attached-fitted]
   [[:standing-sign :torch :redstone-torch :banner]
    standing-or-wall-fitted]
   [[:skull :wither-skull :player-head] skull-fitted]
   [[:ceiling-hanging-sign] hanging-sign-fitted]
   [[:lantern :weathering-lantern] lantern-fitted]
   [[:bell] bell-fitted]
   [[:cocoa] cocoa-fitted]
   [[:bamboo-stalk] bamboo-fitted]
   [growing-plant-types growing-plant-fitted]
   [[:end-rod] rod-fitted]
   [[:mossy-carpet] carpet-fitted]
   [[:pointed-dripstone :sulfur-spike]
    (fn [c p st {:keys [pitch sneaking?]}]
      (dripstone/placed c p st pitch sneaking?))]
   [[:big-dripleaf]
    (fn [c p st _o] (dripleaf/leaf-placed c p st))]
   [[:vine] vine-fitted]
   [[:glow-lichen :multiface :sculk-vein] multiface-fitted]
   [[:scaffolding] scaffold-fitted]
   [[:campfire]
    (fn [c p st {:keys [yaw]}] (campfire/placed c p st yaw))]
   [[:huge-mushroom]
    (fn [c p st _o] (mushroom/placed c p st))]
   [[:farmland :dirt-path]
    (fn [c p st _o] (if (supported? c p st) st (gone-state st)))]])

(def ^:private fits
  (into {} (for [[classes f] fit-rules, k classes] [k f])))

(defn fitted [chunks pos st face yaw pitch sneaking? tick replacing?]
  (if-let [f (fits (block/type-of st))]
    (f chunks pos st {:face face :yaw yaw :pitch pitch
                      :sneaking? sneaking? :tick tick
                      :replacing? replacing?})
    (when (or (not (block/attached? st)) (supported? chunks pos st)) st)))

(def rule
  {:name   :support
   :match? (fn [_chunks st _p] (block/attached? st))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p _rules]
             (let [st (chunk/chunks-get-block chunks p)]
               (when-not (supported? chunks p st)
                 [[p (gone-state st)]])))})

(defn free-below? [chunks [x y z]]
  (let [y' (dec (long y))]
    (and (chunk/in-range? y')
         (block/free? (chunk/chunks-get-block chunks [x y' z])))))

(defn- place-delay ^long [chunks p]
  (if (= :dragon-egg (block/type-of (chunk/chunks-get-block chunks p)))
    (dragonegg/delay-after-place)
    2))

(def falling-rule
  {:name   :falling
   :match? (fn [_chunks st _p] (and (block/falls? st) (not= :scaffolding (block/type-of st))))
   :wake   (fn [chunks tick p _old _self?] (+ (long tick) (place-delay chunks p)))
   :due    (fn [chunks p _ctx]
             (when (free-below? chunks p)
               [[p (block/emptied (chunk/chunks-get-block chunks p))]]))})

(def scaffold-rule
  {:name   :scaffold
   :match? (fn [_chunks st _p] (= :scaffolding (block/type-of st)))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p _ctx]
             (let [st (chunk/chunks-get-block chunks p)
                   st' (scaffold-state chunks p st)]
               (cond
                 (= :7 (:distance (block/props-of st'))) [[p (block/emptied st)]]
                 (not= st' st) [[p st']])))})
