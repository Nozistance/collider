(ns collider.world.support
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chorus :as chorus]
            [collider.world.chunk :as chunk]
            [collider.world.dripleaf :as dripleaf]
            [collider.world.dripstone :as dripstone]
            [collider.world.gen :as gen]
            [collider.world.light :as light]
            [collider.world.moss :as moss]
            [collider.world.liquid :as liquid]))

(set! *warn-on-reflection* true)

(defn needs-support? [st] (block/needs-support? (long st)))
(defn replaceable? [st] (block/replaceable? (long st)))
(defn- state-at ^long [chunks template [_ y _ :as pos]]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (chunk/chunks-get-block chunks template pos)
      -1)))

(def ^:private dirs {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})
(def ^:private around6 [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])
(def ^:private six {:down [0 -1 0] :up [0 1 0] :north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})
(def ^:private opposite {:north :south :south :north :west :east :east :west :up :down :down :up})
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
    (contains? #{:soul-sand :soul-soil} (block/block-of (max 0 below)))
    (boolean
     (or (and (not (neg? below)) (block/face-sturdy? below :up))
         (some (fn [d]
                 (let [n (state-at chunks template (mapv + pos d))]
                   (and (pos? n) (block/burnable? n))))
               around6)))))

(defn- lower-half-of? [below st]
  (and (= (block/block-of below) (block/block-of st))
       (= :lower (:half (block/props-of below)))))

(defn- mushroom-supported? [chunks template [x y z] below]
  (or (block/tagged? below "overrides_mushroom_light_requirement")
      (and (< (long (light/light-at chunks template x y z)) 13)
           (block/solid-render? below))))

(defn- sugar-cane-supported? [chunks template [x y z :as pos] st below]
  (or (= (block/block-of below) (block/block-of st))
      (and (block/tagged? below "supports_sugar_cane")
           (boolean (some (fn [[dx _ dz]]
                            (let [n (state-at chunks template [(+ (long x) (long dx)) (dec (long y)) (+ (long z) (long dz))])]
                              (and (pos? n) (or (water? n) (block/tagged? n "supports_sugar_cane_adjacently")))))
                          (vals dirs))))))

(defn- cactus-supported? [chunks template pos st below]
  (and (not (some (fn [d]
                    (let [n (state-at chunks template (mapv + pos d))]
                      (and (pos? n) (or (block/blocks-motion? n) (= :lava (liquid/liquid-class n))))))
                  (vals dirs)))
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
  (let [n (state-at chunks template (mapv + pos (dirs dir)))]
    (and (not (neg? n)) (block/face-sturdy? n (opposite dir)))))

(defn- bell-supported? [chunks template pos st below above]
  (let [props (block/props-of st)]
    (case (:attachment props)
      :floor (and (not (neg? below)) (block/face-sturdy? below :up))
      :ceiling (holds-center-above? above)
      (attached-to? chunks template pos (:facing props)))))

(defn- vegetation-supported? [t st below]
  (case t
    (:dry-vegetation :short-dry-grass :tall-dry-grass) (block/tagged? below "supports_dry_vegetation")
    (:crop :carrot :potato :beetroot :torchflower-crop) (block/tagged? below "supports_crops")
    (:stem :attached-stem) (block/tagged? below "supports_stem_crops")
    :pitcher-crop (if (= :upper (:half (block/props-of st)))
                    (lower-half-of? below st)
                    (block/tagged? below "supports_crops"))
    (:double-plant :tall-flower) (if (= :upper (:half (block/props-of st)))
                                   (lower-half-of? below st)
                                   (block/tagged? below "supports_vegetation"))
    :seagrass (seagrass-supported? below)
    :cactus-flower (or (block/tagged? below "support_override_cactus_flower")
                       (holds-center-below? below))
    (:bamboo-stalk :bamboo-sapling) (block/tagged? below "supports_bamboo")
    :nether-wart (block/tagged? below "supports_nether_wart")
    :azalea (block/tagged? below "supports_azalea")
    :wither-rose (block/tagged? below "supports_wither_rose")
    :nether-sprouts (block/tagged? below "supports_nether_sprouts")
    (block/tagged? below "supports_vegetation")))

(defn plant-age [tick pos]
  (keyword (str (long (Math/floor (* 25.0 (random/of-key [tick pos :plant-age])))))))

(defn- growing-plant-supported? [chunks template pos st]
  (let [{:keys [head body dir]} (block/growing-plant (block/type-of st))
        n (state-at chunks template (mapv - pos (six dir)))]
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

(def ^:private clockwise {:north :east :east :south :south :west :west :north})
(def ^:private counter-clockwise {:north :west :west :south :south :east :east :north})
(defn- hanging-sign-attaches? [chunks template st attach-pos attach-face]
  (let [n (state-at chunks template attach-pos)]
    (and (not (neg? n))
         (if (= :wall-hanging-sign (block/type-of n))
           (= (#{:north :south} (block/facing-of n)) (#{:north :south} (block/facing-of st)))
           (block/face-sturdy? n attach-face)))))

(defn- hanging-sign-held? [chunks template pos st]
  (let [f (block/facing-of st) cw (clockwise f) ccw (counter-clockwise f)]
    (or (hanging-sign-attaches? chunks template st (mapv + pos (dirs cw)) ccw)
        (hanging-sign-attaches? chunks template st (mapv + pos (dirs ccw)) cw))))

(defn supported? [chunks template pos st]
  (let [st (long st) t (block/type-of st)
        below (state-at chunks template (mapv + pos [0 -1 0]))
        above (state-at chunks template (mapv + pos [0 1 0]))]
    (case t
      (:torch :redstone-torch) (holds-center-below? below)
      (:wall-torch :redstone-wall-torch :ladder)
      (attached-to? chunks template pos (opposite (block/facing-of st)))
      :standing-sign (block/blocks-motion? (max 0 below))
      :banner (block/blocks-motion? (max 0 below))
      :wall-banner (block/blocks-motion? (max 0 (state-at chunks template (mapv + pos (dirs (opposite (block/facing-of st)))))))
      :wall-sign (block/blocks-motion? (max 0 (state-at chunks template (mapv + pos (dirs (opposite (block/facing-of st)))))))
      :ceiling-hanging-sign (holds-center-above? above)
      :wall-hanging-sign (hanging-sign-held? chunks template pos st)
      (:kelp :kelp-plant) (kelp-supported? below)
      :tall-seagrass (if (= :upper (:half (block/props-of st)))
                       (lower-half-of? below st)
                       (seagrass-supported? below))
      (:azalea :wither-rose :nether-sprouts :nether-fungus :nether-roots)
      (vegetation-supported? t st below)
      :mangrove-propagule (if (= :true (:hanging (block/props-of st)))
                            (block/tagged? (max 0 above) "supports_hanging_mangrove_propagule")
                            (block/tagged? (max 0 below) "supports_mangrove_propagule"))
      (:chorus-flower :chorus-plant) (chorus/supported? chunks pos st)
      (:fire :soul-fire) (fire-supported? chunks template pos st below)
      :mushroom (mushroom-supported? chunks template pos below)
      :sugar-cane (sugar-cane-supported? chunks template pos st below)
      :cactus (cactus-supported? chunks template pos st below)
      :lily-pad (lily-pad-supported? chunks template pos below)
      :snow-layer (snow-supported? below)
      (:wool-carpet :carpet) (not (zero? below))
      :leaf-litter (and (not (neg? below)) (block/face-sturdy? below :up))
      (:lantern :weathering-lantern) (if (= :true (:hanging (block/props-of st)))
                                       (holds-center-above? above)
                                       (holds-center-below? below))
      :bell (bell-supported? chunks template pos st below above)
      :candle (holds-center-below? below)
      (:cake :candle-cake) (block/legacy-solid? (max 0 below))
      :grindstone true
      (:button :lever) (attachable? chunks template pos (opposite (connected-direction st)))
      (:weeping-vines :weeping-vines-plant :twisting-vines :twisting-vines-plant :cave-vines :cave-vines-plant)
      (growing-plant-supported? chunks template pos st)
      :amethyst-cluster (let [f (block/facing-of st)
                              n (state-at chunks template (mapv + pos (six (opposite f))))]
                          (and (not (neg? n)) (block/face-sturdy? n f)))
      :sea-pickle (or (neg? below) (block/face-sturdy? below :up) (block/full-cube? below))
      :cocoa (block/tagged? (max 0 (state-at chunks template (mapv + pos (dirs (block/facing-of st))))) "supports_cocoa")
      :spore-blossom (and (holds-center-above? above) (not (water? (state-at chunks template pos))))
      (:coral-plant :coral-fan :base-coral-plant :base-coral-fan) (and (not (neg? below)) (block/face-sturdy? below :up))
      (:coral-wall-fan :base-coral-wall-fan) (attached-to? chunks template pos (opposite (block/facing-of st)))
      :vine (pos? (vine-updated chunks template pos st))
      (:glow-lichen :multiface :sculk-vein) (pos? (multiface-updated chunks template pos st))
      :scaffolding (< (scaffold-distance chunks template pos) 7)
      :mossy-carpet (moss/carpet-supported? chunks pos st)
      :hanging-moss (moss/hanging-supported? chunks pos st)
      (:pointed-dripstone :sulfur-spike) (dripstone/supported? chunks pos st)
      (:big-dripleaf :big-dripleaf-stem :small-dripleaf) (dripleaf/supported? chunks pos st)
      :hanging-roots (and (not (neg? above)) (block/face-sturdy? above :down))
      :farmland (or (not (block/blocks-motion? (max 0 above))) (block/tagged? (max 0 above) "maintains_farmland"))
      :dirt-path (or (not (block/blocks-motion? (max 0 above))) (= :fence-gate (block/type-of (max 0 above))))
      (:crop :carrot :potato :beetroot :torchflower-crop)
      (and (crop-lit? chunks template pos) (vegetation-supported? t st below))
      :pitcher-crop
      (and (or (= :upper (:half (block/props-of st))) (crop-lit? chunks template pos))
           (vegetation-supported? t st below))
      (:rail :powered-rail :detector-rail)
      (or (neg? below) (block/face-holds-rigid? below :up))
      (:pressure-plate :weighted-pressure-plate)
      (or (neg? below) (block/face-holds-rigid? below :up) (block/face-holds-center? below :up))
      :redstone-wire
      (or (neg? below) (block/face-sturdy? below :up) (= :hopper (block/block-of below)))
      (if (block/needs-support? st)
        (vegetation-supported? t st below)
        true))))

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
      (pick chunks template pos [(with (if (= 0 face) :ceiling :floor) (block/player-direction yaw))])
      (let [facing (opposite (get {2 :north 3 :south 4 :west 5 :east} face))
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
    [a b c (opposite c) (opposite b) (opposite a)]))

(defn- horizontal-look-order [yaw]
  (filterv #(contains? dirs %) (look-order yaw 0.0)))

(defn attachable? [chunks template pos dir]
  (let [n (state-at chunks template (mapv + pos (six dir)))]
    (and (not (neg? n)) (block/face-sturdy? n (opposite dir)))))

(defn- vine-face-held? [chunks template pos st dir]
  (and (not= :down dir)
       (or (attachable? chunks template pos dir)
           (and (contains? dirs dir)
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
        dist (fn [st] (Long/parseLong (name (:distance (block/props-of st)))))]
    (if (and (not (neg? below)) (not (scaffold? below)) (block/face-sturdy? below :up))
      0
      (reduce (fn [d dir]
                (let [n (state-at chunks template (mapv + pos (dirs dir)))]
                  (if (scaffold? n) (min (long d) (inc (dist n))) d)))
              (if (scaffold? below) (dist below) 7)
              [:north :south :west :east]))))

(defn scaffold-state ^long [chunks template pos ^long st]
  (let [d (scaffold-distance chunks template pos)
        below (state-at chunks template (mapv + pos [0 -1 0]))]
    (block/state (block/block-of st)
                 (assoc (block/props-of st) :distance (keyword (str d))
                        :bottom (if (and (pos? d) (not= :scaffolding (block/type-of (max 0 below)))) :true :false)))))

(defn- bamboo-fitted [chunks template pos ^long st]
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
        clicked (state-at chunks template (mapv - pos (block/face-offsets face)))]
    (if (and (= (block/block-of clicked) (block/block-of st)) (= f (block/facing-of clicked)))
      (block/state (block/block-of st) (assoc (block/props-of st) :facing (opposite f)))
      st)))

(defn- sign-fitted [chunks template pos st yaw pitch]
  (let [self (block/block-of st) wall (block/wall-block self)
        on-wall (fn [dir] (block/state wall {:facing (opposite dir)}))
        wall-state (first (for [dir (look-order yaw pitch) :when (contains? dirs dir)
                                :when (supported? chunks template pos (on-wall dir))]
                            (on-wall dir)))]
    (first (for [dir (look-order yaw pitch) :when (not= :up dir)
                 :let [cand (if (= :down dir) (when (supported? chunks template pos st) st) wall-state)]
                 :when cand]
             cand))))

(defn- skull-wall-state [chunks template pos st yaw pitch]
  (let [wall (block/wall-block (block/block-of st))]
    (first (for [dir (look-order yaw pitch) :when (contains? dirs dir)
                 :when (not (block/can-be-replaced? (max 0 (state-at chunks template (mapv + pos (dirs dir))))))]
             (block/state wall {:facing (opposite dir)})))))

(defn- skull-fitted [chunks template pos st yaw pitch]
  (let [wall-state (skull-wall-state chunks template pos st yaw pitch)]
    (first (for [dir (look-order yaw pitch) :when (not= :up dir)
                 :let [cand (if (= :down dir) st wall-state)]
                 :when cand]
             cand))))

(defn- growing-plant-fitted [chunks template pos st tick]
  (let [{:keys [head body dir]} (block/growing-plant (block/type-of st))
        n (max 0 (state-at chunks template (mapv + pos (six dir))))
        st' (if (contains? #{head body} (block/block-of n))
              (block/state body)
              (block/state head {:age (plant-age tick pos)}))]
    (when (supported? chunks template pos st') st')))

(defn- ceiling-sign [chunks template pos st yaw sneaking?]
  (let [above (state-at chunks template (mapv + pos [0 1 0]))
        dir (block/player-direction yaw)
        ns? (contains? #{:north :south} dir)
        above-axis (case (block/type-of (max 0 above))
                     :wall-hanging-sign (contains? #{:north :south} (block/facing-of above))
                     :ceiling-hanging-sign (let [r (Long/parseLong (name (:rotation (block/props-of above))))]
                                             (when (zero? (mod r 4)) (contains? #{0 8} r)))
                     nil)
        middle? (and (not (and (some? above-axis) (= above-axis ns?) (not sneaking?)))
                     (or (not (and (not (neg? above)) (block/face-sturdy? above :down))) sneaking?))
        rotation (if middle?
                   (:rotation (block/props-of st))
                   (keyword (str ({:south 0 :west 4 :north 8 :east 12} (opposite dir)))))]
    (block/state (block/block-of st) (assoc (block/props-of st) :attached (if middle? :true :false) :rotation rotation))))

(defn- hanging-sign-fitted [chunks template pos st yaw pitch sneaking?]
  (let [wall (block/wall-block (block/block-of st))
        on-wall (fn [dir] (block/state wall {:facing (opposite dir)}))
        wall-state (first (for [dir (look-order yaw pitch) :when (contains? dirs dir)
                                :when (hanging-sign-held? chunks template pos (on-wall dir))]
                            (on-wall dir)))
        ceiling (ceiling-sign chunks template pos st yaw sneaking?)]
    (first (for [dir (look-order yaw pitch) :when (not= :down dir)
                 :let [cand (if (= :up dir) (when (supported? chunks template pos ceiling) ceiling) wall-state)]
                 :when cand]
             cand))))

(def ^:private face-dirs [:down :up :north :south :west :east])

(defn place-order
  [face yaw pitch replacing?]
  (let [order (look-order yaw pitch)]
    (if replacing?
      order
      (let [first-dir (opposite (nth face-dirs (long face)))]
        (into [first-dir] (remove #(= % first-dir)) order)))))

(defn- face-attached-fitted [chunks template pos st face yaw pitch replacing?]
  (let [self (block/block-of st) props (block/props-of st)
        horizontal (block/player-direction yaw)
        candidate (fn [dir]
                    (if (contains? #{:up :down} dir)
                      (block/state self (assoc props :face (if (= :up dir) :ceiling :floor)
                                               :facing horizontal))
                      (block/state self (assoc props :face :wall :facing (opposite dir)))))]
    (first (for [dir (place-order face yaw pitch replacing?)
                 :let [cand (candidate dir)]
                 :when (supported? chunks template pos cand)]
             cand))))

(defn- carpet-fitted [chunks pos ^long st]
  (let [st' (moss/carpet-updated chunks pos st true)]
    (when (moss/carpet-supported? chunks pos st') st')))

(defn fitted [chunks template pos st face yaw pitch sneaking? tick replacing?]
  (case (block/type-of st)
    (:button :lever :grindstone) (face-attached-fitted chunks template pos st face yaw pitch replacing?)
    :standing-sign (sign-fitted chunks template pos st yaw pitch)
    (:skull :wither-skull :player-head) (skull-fitted chunks template pos st yaw pitch)
    :ceiling-hanging-sign (hanging-sign-fitted chunks template pos st yaw pitch sneaking?)
    (:lantern :weathering-lantern) (lantern-fitted chunks template pos st pitch)
    :bell (bell-fitted chunks template pos st face yaw)
    :cocoa (cocoa-fitted chunks template pos st yaw)
    :bamboo-stalk (bamboo-fitted chunks template pos st)
    (:weeping-vines :weeping-vines-plant :twisting-vines :twisting-vines-plant :cave-vines :cave-vines-plant)
    (growing-plant-fitted chunks template pos st tick)
    :end-rod (rod-fitted chunks template pos st face)
    :mossy-carpet (carpet-fitted chunks pos st)
    (:pointed-dripstone :sulfur-spike) (dripstone/placed chunks pos st pitch sneaking?)
    :big-dripleaf (dripleaf/leaf-placed chunks pos st)
    :vine (vine-fitted chunks template pos st yaw pitch)
    (:glow-lichen :multiface :sculk-vein) (multiface-fitted chunks template pos st yaw pitch)
    :scaffolding (when (< (scaffold-distance chunks template pos) 7) (scaffold-state chunks template pos st))
    (:farmland :dirt-path) (if (supported? chunks template pos st) st (gone-state st))
    (when (or (not (block/attached? st)) (supported? chunks template pos st)) st)))

(def rule
  {:name   :support
   :match? (fn [_chunks st _p] (block/attached? st))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p _rules]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
               (when-not (supported? chunks gen/flat-chunk p st)
                 [[p (gone-state st)]])))})
