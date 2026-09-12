(ns collider.world.blocks.support
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.gen :as gen]
            [collider.world.light :as light]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.liquid :as liquid]))

(set! *warn-on-reflection* true)

(defn- state-at ^long [chunks template [_ y _ :as pos]]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (chunk/chunks-get-block chunks template pos)
      -1)))

(defn- water? [st] (and (pos? st) (or (= :water (liquid/liquid-class st)) (block/waterlogged? st))))
(defn- water-source? [st] (and (pos? st) (or (block/waterlogged? st) (and (= :water (liquid/liquid-class st)) (liquid/source-state? st)))))
(def ^:private kelp-types #{:kelp :kelp-plant})
(defn- kelp-supported? [below]
  (or (contains? kelp-types (block/type-of below))
      (and (block/face-sturdy? below :up) (not (block/tagged? (max 0 below) "cannot_support_kelp")))))

(defn- seagrass-supported? [below]
  (and (not (neg? below))
       (block/face-sturdy? below :up)
       (not (block/tagged? below "cannot_support_seagrass"))))

(defn- fire-supported? [chunks template pos st below]
  (if (= :soul-fire (block/type-of (long st)))
    (block/tagged? (max 0 below) "soul_fire_base_blocks")
    (boolean
      (or (and (not (neg? below)) (block/face-sturdy? below :up))
          (some (fn [d]
                  (let [n (state-at chunks template (mapv + pos d))]
                    (and (pos? n) (block/burnable? n))))
                dir/around)))))

(defn- lower-half-of? [below st]
  (and (= (block/block-of below) (block/block-of st))
       (= :lower (:half (block/props-of below)))))

(defn- mushroom-supported? [chunks template [x y z] below]
  (or (block/tagged? below "overrides_mushroom_light_requirement")
      (and (< (long (light/light-at chunks template x y z)) 13)
           (block/solid-render? below))))

(defn- sugar-cane-supported? [chunks template [x y z] st below]
  (or (= (block/block-of below) (block/block-of st))
      (and (block/tagged? below "supports_sugar_cane")
           (boolean (some (fn [[dx _ dz]]
                            (let [n (state-at chunks template [(+ (long x) (long dx)) (dec (long y)) (+ (long z) (long dz))])]
                              (and (pos? n) (or (water? n) (block/tagged? n "supports_sugar_cane_adjacently")))))
                          (vals dir/horizontal-offset))))))

(defn- cactus-supported? [chunks template pos st below]
  (and (not (some (fn [d]
                    (let [n (state-at chunks template (mapv + pos d))]
                      (and (pos? n) (or (block/blocks-motion? n) (= :lava (liquid/liquid-class n))))))
                  (vals dir/horizontal-offset)))
       (or (= (block/block-of below) (block/block-of st)) (block/tagged? below "supports_cactus"))
       (not (block/liquid? (max 0 (state-at chunks template (mapv + pos [0 1 0])))))))

(defn- lily-pad-supported? [chunks template pos below]
  (and (or (water? below) (block/tagged? below "supports_lily_pad"))
       (not (water? (max 0 (state-at chunks template (mapv + pos [0 1 0])))))))

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

(defn- crop-lit? [chunks template [x y z]]
  (>= (long (light/light-at chunks template x y z)) 8))

(defn- attached-to? [chunks template pos dir]
  (let [n (state-at chunks template (mapv + pos (dir/horizontal-offset dir)))]
    (and (not (neg? n)) (block/face-sturdy? n (dir/opposite dir)))))

(defn- bell-supported? [chunks template pos st below above]
  (let [props (block/props-of st)]
    (case (:attachment props)
      :floor (and (not (neg? below)) (block/face-sturdy? below :up))
      :ceiling (holds-center-above? above)
      (attached-to? chunks template pos (:facing props)))))

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

(defn- vegetation-supported? [t st below]
  (cond
    (halved-vegetation t) (if (= :upper (:half (block/props-of st)))
                            (lower-half-of? below st)
                            (block/tagged? below (halved-vegetation t)))
    (= :seagrass t) (seagrass-supported? below)
    (= :cactus-flower t) (or (block/tagged? below "support_override_cactus_flower")
                             (holds-center-below? below))
    :else (block/tagged? below (get vegetation-tags t "supports_vegetation"))))

(defn plant-age [tick pos]
  (keyword (str (long (Math/floor (* 25.0 (random/of-key [tick pos :plant-age])))))))

(defn- growing-plant-supported? [chunks template pos st]
  (let [{:keys [head body dir]} (block/growing-plant (block/type-of st))
        n (state-at chunks template (mapv - pos (dir/offset dir)))]
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

(defn- hanging-sign-attaches? [chunks template st attach-pos attach-face]
  (let [n (state-at chunks template attach-pos)]
    (and (not (neg? n))
         (if (= :wall-hanging-sign (block/type-of n))
           (= (#{:north :south} (block/facing-of n)) (#{:north :south} (block/facing-of st)))
           (block/face-sturdy? n attach-face)))))

(defn- hanging-sign-held? [chunks template pos st]
  (let [f (block/facing-of st) cw (dir/clockwise f) ccw (dir/counter-clockwise f)]
    (or (hanging-sign-attaches? chunks template st (mapv + pos (dir/horizontal-offset cw)) ccw)
        (hanging-sign-attaches? chunks template st (mapv + pos (dir/horizontal-offset ccw)) cw))))

(defn- wall-attached? [chunks template pos st]
  (block/blocks-motion? (max 0 (state-at chunks template (mapv + pos (dir/horizontal-offset (dir/opposite (block/facing-of st))))))))

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

(defn- cluster-supported? [chunks template pos st]
  (let [f (block/facing-of st)
        n (state-at chunks template (mapv + pos (dir/offset (dir/opposite f))))]
    (and (not (neg? n)) (block/face-sturdy? n f))))

(defn- cocoa-supported? [chunks template pos st]
  (block/tagged? (max 0 (state-at chunks template (mapv + pos (dir/horizontal-offset (block/facing-of st))))) "supports_cocoa"))

(defn- farmland-supported? [above]
  (or (not (block/blocks-motion? (max 0 above))) (block/tagged? (max 0 above) "maintains_farmland")))

(defn- dirt-path-supported? [above]
  (or (not (block/blocks-motion? (max 0 above))) (= :fence-gate (block/type-of (max 0 above)))))

(defn- crop-supported? [chunks template pos st below]
  (and (crop-lit? chunks template pos) (vegetation-supported? (block/type-of st) st below)))

(defn- pitcher-supported? [chunks template pos st below]
  (and (or (= :upper (:half (block/props-of st))) (crop-lit? chunks template pos))
       (vegetation-supported? (block/type-of st) st below)))

(def ^:private supports
  (into {}
        (for [[classes f] [[[:torch :redstone-torch :candle] (fn [_c _t _p _st below _a] (holds-center-below? below))]
                           [[:wall-torch :redstone-wall-torch :ladder]
                            (fn [c t p st _b _a] (attached-to? c t p (dir/opposite (block/facing-of st))))]
                           [[:standing-sign :banner] (fn [_c _t _p _st below _a] (block/blocks-motion? (max 0 below)))]
                           [[:wall-banner :wall-sign] (fn [c t p st _b _a] (wall-attached? c t p st))]
                           [[:ceiling-hanging-sign] (fn [_c _t _p _st _b above] (holds-center-above? above))]
                           [[:wall-hanging-sign] (fn [c t p st _b _a] (hanging-sign-held? c t p st))]
                           [[:kelp :kelp-plant] (fn [_c _t _p _st below _a] (kelp-supported? below))]
                           [[:tall-seagrass] (fn [_c _t _p st below _a] (tall-seagrass-supported? st below))]
                           [[:azalea :wither-rose :nether-sprouts :nether-fungus :nether-roots]
                            (fn [_c _t _p st below _a] (vegetation-supported? (block/type-of st) st below))]
                           [[:mangrove-propagule] (fn [_c _t _p st below above] (propagule-supported? st below above))]
                           [[:chorus-flower :chorus-plant] (fn [c _t p st _b _a] (chorus/supported? c p st))]
                           [[:fire :soul-fire] (fn [c t p st below _a] (fire-supported? c t p st below))]
                           [[:mushroom] (fn [c t p _st below _a] (mushroom-supported? c t p below))]
                           [[:sugar-cane] (fn [c t p st below _a] (sugar-cane-supported? c t p st below))]
                           [[:cactus] (fn [c t p st below _a] (cactus-supported? c t p st below))]
                           [[:lily-pad] (fn [c t p _st below _a] (lily-pad-supported? c t p below))]
                           [[:snow-layer] (fn [_c _t _p _st below _a] (snow-supported? below))]
                           [[:wool-carpet :carpet] (fn [_c _t _p _st below _a] (not (zero? below)))]
                           [[:leaf-litter :coral-plant :coral-fan :base-coral-plant :base-coral-fan]
                            (fn [_c _t _p _st below _a] (and (not (neg? below)) (block/face-sturdy? below :up)))]
                           [[:lantern :weathering-lantern] (fn [_c _t _p st below above] (lantern-supported? st below above))]
                           [[:bell] (fn [c t p st below above] (bell-supported? c t p st below above))]
                           [[:cake :candle-cake] (fn [_c _t _p _st below _a] (block/legacy-solid? (max 0 below)))]
                           [[:grindstone] (fn [_c _t _p _st _b _a] true)]
                           [[:button :lever] (fn [c t p st _b _a] (attachable? c t p (dir/opposite (connected-direction st))))]
                           [[:weeping-vines :weeping-vines-plant :twisting-vines :twisting-vines-plant :cave-vines :cave-vines-plant]
                            (fn [c t p st _b _a] (growing-plant-supported? c t p st))]
                           [[:amethyst-cluster] (fn [c t p st _b _a] (cluster-supported? c t p st))]
                           [[:sea-pickle] (fn [_c _t _p _st below _a] (or (neg? below) (block/face-sturdy? below :up) (block/full-cube? below)))]
                           [[:cocoa] (fn [c t p st _b _a] (cocoa-supported? c t p st))]
                           [[:spore-blossom] (fn [c t p _st _b above] (and (holds-center-above? above) (not (water? (state-at c t p)))))]
                           [[:coral-wall-fan :base-coral-wall-fan]
                            (fn [c t p st _b _a] (attached-to? c t p (dir/opposite (block/facing-of st))))]
                           [[:vine] (fn [c t p st _b _a] (pos? (vine-updated c t p st)))]
                           [[:glow-lichen :multiface :sculk-vein] (fn [c t p st _b _a] (pos? (multiface-updated c t p st)))]
                           [[:scaffolding] (fn [c t p _st _b _a] (< (scaffold-distance c t p) 7))]
                           [[:mossy-carpet] (fn [c _t p st _b _a] (moss/carpet-supported? c p st))]
                           [[:hanging-moss] (fn [c _t p st _b _a] (moss/hanging-supported? c p st))]
                           [[:pointed-dripstone :sulfur-spike] (fn [c _t p st _b _a] (dripstone/supported? c p st))]
                           [[:big-dripleaf :big-dripleaf-stem :small-dripleaf] (fn [c _t p st _b _a] (dripleaf/supported? c p st))]
                           [[:hanging-roots] (fn [_c _t _p _st _b above] (and (not (neg? above)) (block/face-sturdy? above :down)))]
                           [[:farmland] (fn [_c _t _p _st _b above] (farmland-supported? above))]
                           [[:dirt-path] (fn [_c _t _p _st _b above] (dirt-path-supported? above))]
                           [[:crop :carrot :potato :beetroot :torchflower-crop]
                            (fn [c t p st below _a] (crop-supported? c t p st below))]
                           [[:pitcher-crop] (fn [c t p st below _a] (pitcher-supported? c t p st below))]
                           [[:rail :powered-rail :detector-rail]
                            (fn [_c _t _p _st below _a] (or (neg? below) (block/face-holds-rigid? below :up)))]
                           [[:pressure-plate :weighted-pressure-plate]
                            (fn [_c _t _p _st below _a] (or (neg? below) (block/face-holds-rigid? below :up) (block/face-holds-center? below :up)))]
                           [[:redstone-wire]
                            (fn [_c _t _p _st below _a] (or (neg? below) (block/face-sturdy? below :up) (= :hopper (block/block-of below))))]]
              k classes]
          [k f])))

(defn- default-supported? [t st below]
  (if (block/needs-support? st) (vegetation-supported? t st below) true))

(defn supported? [chunks template pos st]
  (let [st (long st) t (block/type-of st)
        below (state-at chunks template (mapv + pos [0 -1 0]))
        above (state-at chunks template (mapv + pos [0 1 0]))]
    (if-let [f (supports t)]
      (f chunks template pos st below above)
      (default-supported? t st below))))

(defn- pick [chunks template pos states]
  (first (filter #(supported? chunks template pos %) states)))

(defn- lantern-fitted [chunks template pos st pitch]
  (let [self (block/block-of st) props (block/props-of st)
        standing (block/state self (assoc props :hanging :false))
        hanging (block/state self (assoc props :hanging :true))]
    (pick chunks template pos (if (pos? (double pitch)) [standing hanging] [hanging standing]))))

(defn- bell-fitted [chunks template pos st face yaw]
  (let [self (block/block-of st) props (block/props-of st)
        face (long face)
        with (fn [attachment facing] (block/state self (assoc props :attachment attachment :facing facing)))]
    (if (<= face 1)
      (pick chunks template pos [(with (if (= 0 face) :ceiling :floor) (dir/player-direction yaw))])
      (let [facing (dir/opposite (dir/face-facing face))
            axis (if (#{:north :south} facing) [:north :south] [:west :east])
            double? (every? #(attached-to? chunks template pos %) axis)
            wall (with (if double? :double_wall :single_wall) facing)
            below (state-at chunks template (mapv + pos [0 -1 0]))
            fallback (with (if (and (not (neg? below)) (block/face-sturdy? below :up)) :floor :ceiling) facing)]
        (pick chunks template pos [wall fallback])))))

(defn gone-state ^long [^long st]
  (if (#{:farmland :dirt-path} (block/type-of st)) (block/state :dirt) (block/emptied st)))

(defn look-order [yaw pitch]
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

(defn attachable? [chunks template pos dir]
  (let [n (state-at chunks template (mapv + pos (dir/offset dir)))]
    (and (not (neg? n)) (block/face-sturdy? n (dir/opposite dir)))))

(defn- vine-face-held? [chunks template pos st dir]
  (and (not= :down dir)
       (or (attachable? chunks template pos dir)
           (and (contains? dir/horizontal-offset dir)
                (let [above (state-at chunks template (mapv + pos [0 1 0]))]
                  (and (= (block/block-of above) (block/block-of st))
                       (= :true (get (block/props-of above) dir))))))))

(defn vine-updated ^long [chunks template pos ^long st]
  (let [props (block/props-of st)
        props' (reduce (fn [m dir]
                         (if (= :true (get m dir))
                           (assoc m dir (if (vine-face-held? chunks template pos st dir) :true :false))
                           m))
                       props [:up :north :south :west :east])
        st' (block/state (block/block-of st) props')]
    (if (seq (block/faces-of st')) st' 0)))

(defn multiface-updated ^long [chunks template pos ^long st]
  (let [props (block/props-of st)
        props' (reduce (fn [m dir]
                         (if (and (= :true (get m dir)) (not (attachable? chunks template pos dir)))
                           (assoc m dir :false)
                           m))
                       props block/face-props)
        st' (block/state (block/block-of st) props')]
    (if (seq (block/faces-of st')) st' (block/emptied st))))

(defn scaffold-distance ^long [chunks template [x y z :as pos]]
  (let [below (state-at chunks template [x (dec (long y)) z])
        scaffold? (fn [st] (= :scaffolding (block/type-of (max 0 st))))
        dist (fn [st] (block/prop-long st :distance))]
    (if (and (not (neg? below)) (not (scaffold? below)) (block/face-sturdy? below :up))
      0
      (reduce (fn [d dir]
                (let [n (state-at chunks template (mapv + pos (dir/horizontal-offset dir)))]
                  (if (scaffold? n) (min (long d) (inc (dist n))) d)))
              (if (scaffold? below) (dist below) 7)
              dir/horizontal))))

(defn scaffold-state ^long [chunks template pos ^long st]
  (let [d (scaffold-distance chunks template pos)
        below (state-at chunks template (mapv + pos [0 -1 0]))]
    (block/state (block/block-of st)
                 (assoc (block/props-of st) :distance (keyword (str d))
                                            :bottom (if (and (pos? d) (not= :scaffolding (block/type-of (max 0 below)))) :true :false)))))

(defn- bamboo-fitted [chunks template pos]
  (let [below (state-at chunks template (mapv + pos [0 -1 0]))
        above (state-at chunks template (mapv + pos [0 1 0]))
        age (fn [^long n] (:age (block/props-of n)))]
    (when (and (let [cur (max 0 (state-at chunks template pos))]
                 (and (nil? (liquid/liquid-class cur)) (not (block/waterlogged? cur))))
               (block/tagged? (max 0 below) "supports_bamboo"))
      (case (block/block-of (max 0 below))
        :bamboo-sapling (block/state :bamboo {:age :0})
        :bamboo (block/state :bamboo {:age (if (= :0 (age below)) :0 :1)})
        (if (= :bamboo (block/block-of (max 0 above)))
          (block/state :bamboo {:age (age above)})
          (block/state :bamboo-sapling))))))

(defn- cocoa-fitted [chunks template pos st yaw]
  (pick chunks template pos
        (map #(block/state (block/block-of st) (assoc (block/props-of st) :facing %)) (horizontal-look-order yaw))))

(defn- vine-fitted [chunks template pos st yaw pitch]
  (let [cur (state-at chunks template pos)
        base (if (= (block/block-of cur) (block/block-of st)) cur st)
        free (fn [dir] (and (not= :down dir) (= :false (get (block/props-of base) dir))
                            (vine-face-held? chunks template pos base dir)))]
    (if-let [dir (first (filter free (look-order yaw pitch)))]
      (block/state (block/block-of base) (assoc (block/props-of base) dir :true))
      (when (= base cur) cur))))

(defn- multiface-fitted [chunks template pos st yaw pitch]
  (let [cur (state-at chunks template pos)
        same? (= (block/block-of cur) (block/block-of st))
        base (cond same? cur
                   (water-source? (max 0 cur)) (block/with-water st)
                   :else st)
        free (fn [dir] (and (= :false (get (block/props-of base) dir)) (attachable? chunks template pos dir)))]
    (when-let [dir (first (filter free (look-order yaw pitch)))]
      (block/state (block/block-of base) (assoc (block/props-of base) dir :true)))))

(defn- rod-fitted [chunks template pos st face]
  (let [f (block/facing-of st)
        clicked (state-at chunks template (mapv - pos (dir/face-offset face)))]
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

(defn- standing-or-wall-fitted [chunks template pos st {:keys [yaw pitch face replacing?]}]
  (let [order (placement-order yaw pitch face replacing?)
        on-wall (fn [dir] (block/state (block/wall-block (block/block-of st)) {:facing (dir/opposite dir)}))
        wall-state (first (for [dir order :when (contains? dir/horizontal-offset dir)
                                :when (supported? chunks template pos (on-wall dir))]
                            (on-wall dir)))]
    (standing-or-wall order (when (supported? chunks template pos st) st) wall-state)))

(defn- skull-fitted [chunks template pos st {:keys [yaw pitch face replacing?]}]
  (let [order (placement-order yaw pitch face replacing?)
        wall (block/wall-block (block/block-of st))
        wall-state (first (for [dir order :when (contains? dir/horizontal-offset dir)
                                :when (not (block/can-be-replaced? (max 0 (state-at chunks template (mapv + pos (dir/horizontal-offset dir))))))]
                            (block/state wall {:facing (dir/opposite dir)})))]
    (standing-or-wall order st wall-state)))

(defn- growing-plant-fitted [chunks template pos st tick]
  (let [{:keys [head body dir]} (block/growing-plant (block/type-of st))
        n (max 0 (state-at chunks template (mapv + pos (dir/offset dir))))
        st' (if (contains? #{head body} (block/block-of n))
              (block/state body)
              (block/state head {:age (plant-age tick pos)}))]
    (when (supported? chunks template pos st') st')))

(defn- ceiling-sign [chunks template pos st yaw sneaking?]
  (let [above (state-at chunks template (mapv + pos [0 1 0]))
        dir (dir/player-direction yaw)
        ns? (contains? #{:north :south} dir)
        above-axis (case (block/type-of (max 0 above))
                     :wall-hanging-sign (contains? #{:north :south} (block/facing-of above))
                     :ceiling-hanging-sign (let [r (block/prop-long above :rotation)]
                                             (when (zero? (mod r 4)) (contains? #{0 8} r)))
                     nil)
        middle? (and (not (and (some? above-axis) (= above-axis ns?) (not sneaking?)))
                     (or (not (and (not (neg? above)) (block/face-sturdy? above :down))) sneaking?))
        rotation (if middle?
                   (:rotation (block/props-of st))
                   (keyword (str ({:south 0 :west 4 :north 8 :east 12} (dir/opposite dir)))))]
    (block/state (block/block-of st) (assoc (block/props-of st) :attached (if middle? :true :false) :rotation rotation))))

(defn- hanging-sign-fitted [chunks template pos st yaw pitch sneaking?]
  (let [wall (block/wall-block (block/block-of st))
        on-wall (fn [dir] (block/state wall {:facing (dir/opposite dir)}))
        wall-state (first (for [dir (look-order yaw pitch) :when (contains? dir/horizontal-offset dir)
                                :when (hanging-sign-held? chunks template pos (on-wall dir))]
                            (on-wall dir)))
        ceiling (ceiling-sign chunks template pos st yaw sneaking?)]
    (first (for [dir (look-order yaw pitch) :when (not= :down dir)
                 :let [cand (if (= :up dir) (when (supported? chunks template pos ceiling) ceiling) wall-state)]
                 :when cand]
             cand))))

(defn place-order
  [face yaw pitch replacing?]
  (let [order (look-order yaw pitch)]
    (if replacing?
      order
      (let [first-dir (dir/opposite (nth dir/six (long face)))]
        (into [first-dir] (remove #(= % first-dir)) order)))))

(defn- face-attached-fitted [chunks template pos st face yaw pitch replacing?]
  (let [self (block/block-of st) props (block/props-of st)
        horizontal (dir/player-direction yaw)
        candidate (fn [dir]
                    (if (contains? #{:up :down} dir)
                      (block/state self (assoc props :face (if (= :up dir) :ceiling :floor)
                                                     :facing horizontal))
                      (block/state self (assoc props :face :wall :facing (dir/opposite dir)))))]
    (first (for [dir (place-order face yaw pitch replacing?)
                 :let [cand (candidate dir)]
                 :when (supported? chunks template pos cand)]
             cand))))

(defn- carpet-fitted [chunks pos ^long st]
  (let [st' (moss/carpet-updated chunks pos st true)]
    (when (moss/carpet-supported? chunks pos st') st')))

(def ^:private fits
  (into {}
        (for [[classes f] [[[:button :lever :grindstone]
                            (fn [c t p st {:keys [face yaw pitch replacing?]}] (face-attached-fitted c t p st face yaw pitch replacing?))]
                           [[:standing-sign :torch :redstone-torch :banner] standing-or-wall-fitted]
                           [[:skull :wither-skull :player-head] skull-fitted]
                           [[:ceiling-hanging-sign]
                            (fn [c t p st {:keys [yaw pitch sneaking?]}] (hanging-sign-fitted c t p st yaw pitch sneaking?))]
                           [[:lantern :weathering-lantern] (fn [c t p st {:keys [pitch]}] (lantern-fitted c t p st pitch))]
                           [[:bell] (fn [c t p st {:keys [face yaw]}] (bell-fitted c t p st face yaw))]
                           [[:cocoa] (fn [c t p st {:keys [yaw]}] (cocoa-fitted c t p st yaw))]
                           [[:bamboo-stalk] (fn [c t p _st _o] (bamboo-fitted c t p))]
                           [[:weeping-vines :weeping-vines-plant :twisting-vines :twisting-vines-plant :cave-vines :cave-vines-plant]
                            (fn [c t p st {:keys [tick]}] (growing-plant-fitted c t p st tick))]
                           [[:end-rod] (fn [c t p st {:keys [face]}] (rod-fitted c t p st face))]
                           [[:mossy-carpet] (fn [c _t p st _o] (carpet-fitted c p st))]
                           [[:pointed-dripstone :sulfur-spike]
                            (fn [c _t p st {:keys [pitch sneaking?]}] (dripstone/placed c p st pitch sneaking?))]
                           [[:big-dripleaf] (fn [c _t p st _o] (dripleaf/leaf-placed c p st))]
                           [[:vine] (fn [c t p st {:keys [yaw pitch]}] (vine-fitted c t p st yaw pitch))]
                           [[:glow-lichen :multiface :sculk-vein] (fn [c t p st {:keys [yaw pitch]}] (multiface-fitted c t p st yaw pitch))]
                           [[:scaffolding] (fn [c t p st _o] (when (< (scaffold-distance c t p) 7) (scaffold-state c t p st)))]
                           [[:farmland :dirt-path] (fn [c t p st _o] (if (supported? c t p st) st (gone-state st)))]]
              k classes]
          [k f])))

(defn fitted [chunks template pos st face yaw pitch sneaking? tick replacing?]
  (if-let [f (fits (block/type-of st))]
    (f chunks template pos st {:face      face :yaw yaw :pitch pitch
                               :sneaking? sneaking? :tick tick :replacing? replacing?})
    (when (or (not (block/attached? st)) (supported? chunks template pos st)) st)))

(def rule
  {:name   :support
   :match? (fn [_chunks st _p] (block/attached? st))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p _rules]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
               (when-not (supported? chunks gen/flat-chunk p st)
                 [[p (gone-state st)]])))})

(defn free-below? [chunks template [x y z]]
  (let [y' (dec (long y))]
    (and (chunk/in-range? y')
         (block/free? (chunk/chunks-get-block chunks template [x y' z])))))

(def falling-rule
  {:name   :falling
   :match? (fn [_chunks st _p] (and (block/falls? st) (not= :scaffolding (block/type-of st))))
   :wake   (fn [_chunks tick _p _old _self?] (+ (long tick) 2))
   :due    (fn [chunks p _ctx]
             (when (free-below? chunks gen/flat-chunk p)
               [[p (block/emptied (chunk/chunks-get-block chunks gen/flat-chunk p))]]))})

(def scaffold-rule
  {:name   :scaffold
   :match? (fn [_chunks st _p] (= :scaffolding (block/type-of st)))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p _ctx]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)
                   st' (scaffold-state chunks gen/flat-chunk p st)]
               (cond
                 (= :7 (:distance (block/props-of st'))) [[p (block/emptied st)]]
                 (not= st' st) [[p st']])))})
