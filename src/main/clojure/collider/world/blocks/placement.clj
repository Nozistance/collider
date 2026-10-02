(ns collider.world.blocks.placement
  "The state a block takes when a player places it."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.campfire :as campfire]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.multiface :as multiface]
            [collider.world.blocks.mushroom :as mushroom]
            [collider.world.blocks.scaffold :as scaffold]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn- pick [chunks pos states]
  (first (filter #(support/supported? chunks pos %) states)))

(defn- placement-order [yaw pitch face replacing?]
  (let [order (dir/look-order yaw pitch)
        first-dir (dir/opposite (dir/from-index face))]
    (if replacing?
      order
      (into [first-dir] (remove #{first-dir}) order))))

(defn- first-free [order free?]
  (first (filter free? order)))

(defn- lantern-fitted [chunks pos st {:keys [pitch]}]
  (let [standing (block/with st :hanging :false)
        hanging (block/with st :hanging :true)
        order (if (pos? (double pitch))
                [standing hanging]
                [hanging standing])]
    (pick chunks pos order)))

(defn- bell-at [st attachment facing]
  (block/with st :attachment attachment :facing facing))

(defn- bell-on-wall [chunks pos st facing]
  (let [axis (if (#{:north :south} facing)
               [:north :south]
               [:west :east])
        double? (every? #(support/attached-to? chunks pos %) axis)
        below (chunk/at chunks (dir/down pos))
        floor? (block/face-sturdy? below :up)]
    [(bell-at st (if double? :double_wall :single_wall) facing)
     (bell-at st (if floor? :floor :ceiling) facing)]))

(def ^:private bell-ends {:down :ceiling :up :floor})

(defn- bell-fitted [chunks pos st {:keys [face yaw]}]
  (if-let [on (bell-ends (dir/from-index face))]
    (pick chunks pos [(bell-at st on (dir/player-direction yaw))])
    (->> (dir/opposite (dir/horizontal-face face))
         (bell-on-wall chunks pos st)
         (pick chunks pos))))

(defn- bamboo-age [^long n] (:age (block/props-of n)))

(defn- bamboo-on [^long below ^long above]
  (case (block/block-of below)
    :bamboo-sapling (block/state :bamboo {:age :0})
    :bamboo (let [young? (= :0 (bamboo-age below))]
              (block/state :bamboo {:age (if young? :0 :1)}))
    (if (= :bamboo (block/block-of above))
      (block/state :bamboo {:age (bamboo-age above)})
      (block/state :bamboo-sapling))))

(defn- bamboo-fitted [chunks pos _st _opts]
  (let [below (chunk/at chunks (dir/down pos))]
    (when (and (nil? (block/liquid-class (chunk/at chunks pos)))
               (block/tagged? below "supports_bamboo"))
      (bamboo-on below (chunk/at chunks (dir/up pos))))))

(defn- cocoa-fitted [chunks pos st {:keys [yaw]}]
  (->> (dir/look-order yaw 0.0)
       (filter dir/horizontal-offset)
       (map #(block/with st :facing %))
       (pick chunks pos)))

(defn- with-face ^long [^long st side]
  (block/with st side :true))

(defn- vine-fitted [chunks pos st opts]
  (let [{:keys [yaw pitch face replacing?]} opts
        order (placement-order yaw pitch face replacing?)
        cur (chunk/at chunks pos)
        base (if (= (block/block-of cur) (block/block-of st)) cur st)
        free? (fn [side]
                (and (not= :down side)
                     (= :false (get (block/props-of base) side))
                     (support/vine-face-held? chunks pos base side)))]
    (if-let [side (first-free order free?)]
      (with-face base side)
      (when (= base cur) cur))))

(defn- multiface-base [^long cur ^long st]
  (cond
    (= (block/block-of cur) (block/block-of st)) cur
    (block/holds-water-source? cur) (block/with-water st)
    :else st))

(defn- multiface-fitted [chunks pos st opts]
  (let [{:keys [yaw pitch face replacing?]} opts
        order (placement-order yaw pitch face replacing?)
        base (multiface-base (chunk/at chunks pos) st)
        free? (fn [side]
                (and (= :false (get (block/props-of base) side))
                     (multiface/attaches? chunks pos side)))]
    (when-let [side (first-free order free?)]
      (with-face base side))))

(defn- hook-fitted [chunks pos st opts]
  (let [{:keys [yaw pitch face replacing?]} opts]
    (->> (placement-order yaw pitch face replacing?)
         (filter dir/horizontal-offset)
         (map #(block/with st :facing (dir/opposite %)))
         (pick chunks pos))))

(defn- planted [b tick pos]
  (block/state b {:age (support/plant-age tick pos)}))

(defn- kelp-fitted [chunks pos _st {:keys [tick]}]
  (let [above (chunk/at chunks (dir/up pos))
        st' (if (contains? support/kelp-types (block/type-of above))
              (block/state :kelp-plant)
              (planted :kelp tick pos))]
    (when (support/supported? chunks pos st') st')))

(defn- rod-fitted [chunks pos st {:keys [face]}]
  (let [f (block/facing-of st)
        clicked (chunk/at chunks (mapv - pos (dir/face-offset face)))]
    (if (and (= (block/block-of clicked) (block/block-of st))
             (= f (block/facing-of clicked)))
      (block/with st :facing (dir/opposite f))
      st)))

(defn- first-candidate [order skipped own own-state wall-state]
  (first (for [side order :when (not= skipped side)
               :let [cand (if (= own side) own-state wall-state)]
               :when cand]
           cand)))

(defn- first-wall [order fit? on-wall]
  (first (for [side order
               :when (contains? dir/horizontal-offset side)
               :when (fit? side)]
           (on-wall side))))

(defn- wall-state-of [st side]
  (let [wall (block/wall-block (block/block-of st))
        logged (select-keys (block/props-of st) [:waterlogged])]
    (block/state wall (assoc logged :facing (dir/opposite side)))))

(defn- held-wall [chunks pos st order]
  (let [on-wall #(wall-state-of st %)
        held? #(support/supported? chunks pos (on-wall %))]
    (first-wall order held? on-wall)))

(defn- standing-or-wall-fitted [chunks pos st opts]
  (let [{:keys [yaw pitch face replacing?]} opts
        order (placement-order yaw pitch face replacing?)
        standing (when (support/supported? chunks pos st) st)]
    (first-candidate order :up :down standing
                     (held-wall chunks pos st order))))

(defn- skull-wall-backed? [chunks pos side]
  (let [n (chunk/at chunks (dir/toward pos side))]
    (not (block/can-be-replaced? n))))

(defn- skull-fitted [chunks pos st opts]
  (let [{:keys [yaw pitch face replacing?]} opts
        order (placement-order yaw pitch face replacing?)
        backed? #(skull-wall-backed? chunks pos %)
        wall (first-wall order backed? #(wall-state-of st %))]
    (first-candidate order :up :down st wall)))

(defn- growing-plant-fitted [chunks pos st {:keys [tick]}]
  (let [{:keys [head body dir]}
        (block/growing-plant (block/type-of st))
        n (chunk/at chunks (dir/toward pos dir))
        st' (if (contains? #{head body} (block/block-of n))
              (block/state body)
              (planted head tick pos))]
    (when (support/supported? chunks pos st') st')))

(defn- ceiling-sign-axis [^long above]
  (let [r (block/prop-long above :rotation)]
    (when (zero? (mod r 4)) (if (contains? #{0 8} r) :z :x))))

(defn- above-sign-axis [^long above]
  (case (block/type-of above)
    :wall-hanging-sign (dir/axis (block/facing-of above))
    :ceiling-hanging-sign (ceiling-sign-axis above)
    nil))

(defn- sign-middle? [^long above axis player-axis sneaking?]
  (and (not (and (some? axis) (= axis player-axis) (not sneaking?)))
       (or sneaking? (not (block/face-sturdy? above :down)))))

(def ^:private sign-turns {:south 0 :west 4 :north 8 :east 12})

(defn- ceiling-sign [chunks pos st yaw sneaking?]
  (let [above (chunk/at chunks (dir/up pos))
        facing (dir/player-direction yaw)
        axis (above-sign-axis above)
        middle? (sign-middle? above axis (dir/axis facing) sneaking?)
        rotation (if middle?
                   (:rotation (block/props-of st))
                   (sign-turns (dir/opposite facing)))]
    (block/with st
                :attached (block/flag middle?)
                :rotation rotation)))

(defn- hanging-sign-fitted [chunks pos st opts]
  (let [{:keys [yaw pitch sneaking?]} opts
        order (dir/look-order yaw pitch)
        ceiling (ceiling-sign chunks pos st yaw sneaking?)
        up (when (support/supported? chunks pos ceiling) ceiling)]
    (first-candidate order :down :up up
                     (held-wall chunks pos st order))))

(defn- face-attached-state [st side horizontal]
  (let [wall? (not (contains? #{:up :down} side))
        face (cond wall? :wall (= :up side) :ceiling :else :floor)
        facing (if wall? (dir/opposite side) horizontal)]
    (block/with st :face face :facing facing)))

(defn- face-attached-fitted [chunks pos st opts]
  (let [{:keys [face yaw pitch replacing?]} opts
        horizontal (dir/player-direction yaw)]
    (->> (placement-order yaw pitch face replacing?)
         (map #(face-attached-state st % horizontal))
         (pick chunks pos))))

(defn- carpet-fitted [chunks pos ^long st _opts]
  (let [st' (moss/carpet-updated chunks pos st true)]
    (when (moss/carpet-supported? chunks pos st') st')))

(defn- scaffold-fitted [chunks pos st _opts]
  (when (< (scaffold/distance chunks pos) 7)
    (scaffold/shaped chunks pos st)))

(defn- dripstone-fitted [chunks pos st {:keys [pitch sneaking?]}]
  (dripstone/placed chunks pos st pitch sneaking?))

(defn- dripleaf-fitted [chunks pos st _opts]
  (dripleaf/leaf-placed chunks pos st))

(defn- campfire-fitted [chunks pos st {:keys [yaw]}]
  (campfire/placed chunks pos st yaw))

(defn- mushroom-fitted [chunks pos st _opts]
  (mushroom/placed chunks pos st))

(defn- dirt-fitted [chunks pos st _opts]
  (if (support/supported? chunks pos st) st (support/gone-state st)))

(def ^:private fit-rules
  [[[:button :lever :grindstone] face-attached-fitted]
   [[:standing-sign :torch :redstone-torch :banner :coral-fan
     :base-coral-fan]
    standing-or-wall-fitted]
   [[:trip-wire-hook] hook-fitted]
   [[:kelp] kelp-fitted]
   [[:skull :wither-skull :player-head] skull-fitted]
   [[:ceiling-hanging-sign] hanging-sign-fitted]
   [[:lantern :weathering-lantern] lantern-fitted]
   [[:bell] bell-fitted]
   [[:cocoa] cocoa-fitted]
   [[:bamboo-stalk] bamboo-fitted]
   [block/growing-plant-types growing-plant-fitted]
   [[:end-rod] rod-fitted]
   [[:mossy-carpet] carpet-fitted]
   [[:pointed-dripstone :sulfur-spike] dripstone-fitted]
   [[:big-dripleaf] dripleaf-fitted]
   [[:vine] vine-fitted]
   [block/multiface-types multiface-fitted]
   [[:scaffolding] scaffold-fitted]
   [[:campfire] campfire-fitted]
   [[:huge-mushroom] mushroom-fitted]
   [[:farmland :dirt-path] dirt-fitted]])

(def ^:private fits
  (into {} (for [[classes f] fit-rules, k classes] [k f])))

(defn fitted
  "Returns the state st takes when a player places it at pos, or nil
  when it cannot stand there."
  [chunks pos st face yaw pitch sneaking? tick replacing?]
  (if-let [f (fits (block/type-of st))]
    (f chunks pos st {:face face :yaw yaw :pitch pitch
                      :sneaking? sneaking? :tick tick
                      :replacing? replacing?})
    (when (or (not (block/attached? st))
              (support/supported? chunks pos st))
      st)))

(defn- hinge-balance ^long [at left right]
  (let [full (fn [off] (if (block/full-cube? (at off)) 1 0))]
    (+ (- (full left)) (- (full (dir/up left)))
       (full right) (full (dir/up right)))))

(defn- lower-door? [st]
  (and (contains? block/door-types (block/type-of st))
       (= :lower (:half (block/props-of st)))))

(defn- cursor-hinge [facing ^double cx ^double cz]
  (let [[sx _ sz] (dir/horizontal-offset facing)]
    (if (and (or (>= (long sx) 0) (not (< cz 0.5)))
             (or (<= (long sx) 0) (not (> cz 0.5)))
             (or (>= (long sz) 0) (not (> cx 0.5)))
             (or (<= (long sz) 0) (not (< cx 0.5))))
      :left
      :right)))

(defn- forced-hinge [l r ^long balance]
  (cond
    (not (and (or (not l) r) (<= balance 0))) :right
    (not (and (or (not r) l) (>= balance 0))) :left))

(defn door-hinge
  "Returns :left or :right for a door placed at pos with that facing.
  The cursor coordinates are within the clicked face, in sixteenths."
  [chunks pos facing cursor-x cursor-z]
  (let [at (fn [d] (chunk/at chunks (mapv + pos d)))
        left (dir/horizontal-offset (dir/counter-clockwise facing))
        right (dir/horizontal-offset (dir/clockwise facing))
        [l r] (map (comp lower-door? at) [left right])]
    (or (forced-hinge l r (hinge-balance at left right))
        (cursor-hinge facing (/ (double cursor-x) 16.0)
                      (/ (double cursor-z) 16.0)))))
