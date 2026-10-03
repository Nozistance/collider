(ns collider.world.blocks.chest
  "Chests and barrels, their placement and the pairing of two halves."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def types
  "The block types of chests that pair into double chests."
  #{:chest :trapped-chest :copper-chest :weathering-copper-chest})

(def copper-types
  "The block types of copper chests."
  #{:copper-chest :weathering-copper-chest})

(def ^:private ^:table copper-chests
  (delay (set (get-in (data/tags) ["block" "copper_chests"]))))

(defn state-at
  ^long [chunks pos]
  (chunk/at chunks pos))

(defn connected-direction
  "Returns the direction from the half of a double chest st to its
  other half."
  [^long st]
  (let [{:keys [type facing]} (block/props-of st)]
    (if (= :left type)
      (dir/clockwise facing)
      (dir/counter-clockwise facing))))

(defn- connects? [^long self ^long other]
  (if (contains? copper-types (block/type-of self))
    (contains? @copper-chests (block/block-of other))
    (and (pos? other)
         (= (block/block-of self) (block/block-of other)))))

(defn- partner-pos [pos ^long st]
  (dir/toward pos (connected-direction st)))

(defn paired?
  "Returns true when chest other is the other half of the double
  chest st."
  [^long st ^long other]
  (let [a (block/props-of st) b (block/props-of other)]
    (and (connects? st other)
         (not= :single (:type b))
         (not= (:type a) (:type b))
         (= (:facing a) (:facing b)))))

(defn partner
  "Returns the cell of the other half of the double chest at pos.
  Returns nil when no chest pairs with it."
  [chunks pos ^long st]
  (when (and (contains? types (block/type-of st))
             (not= :single (:type (block/props-of st))))
    (let [p2 (partner-pos pos st)]
      (when (paired? st (state-at chunks p2)) p2))))

(def ^:private oxidation
  {:copper-chest 0
   :exposed-copper-chest 1
   :weathered-copper-chest 2
   :oxidized-copper-chest 3
   :waxed-copper-chest 0
   :waxed-exposed-copper-chest 1
   :waxed-weathered-copper-chest 2
   :waxed-oxidized-copper-chest 3})

(def ^:private unwaxed
  {:waxed-copper-chest :copper-chest
   :waxed-exposed-copper-chest :exposed-copper-chest
   :waxed-weathered-copper-chest :weathered-copper-chest
   :waxed-oxidized-copper-chest :oxidized-copper-chest})

(defn- waxed? [b] (contains? unwaxed b))

(defn- least-oxidized [a b]
  (let [[a b] (if (= (waxed? a) (waxed? b))
                [a b]
                [(get unwaxed a a) (get unwaxed b b)])]
    (if (<= (long (oxidation a 0)) (long (oxidation b 0))) a b)))

(defn- copper-merged [^long st ^long other]
  (let [b (block/block-of st) o (block/block-of other)]
    (if (and (contains? copper-types (block/type-of st))
             (contains? @copper-chests o))
      (block/state (least-oxidized b o) (block/props-of st))
      st)))

(defn- candidate-facing [chunks pos ^long st dir]
  (let [o (state-at chunks (dir/toward pos dir))
        props (block/props-of o)]
    (when (and (connects? st o) (= :single (:type props)))
      (:facing props))))

(defn- chest-type [chunks pos ^long st facing]
  (let [toward #(candidate-facing chunks pos st %)]
    (cond
      (= facing (toward (dir/clockwise facing))) :left
      (= facing (toward (dir/counter-clockwise facing))) :right
      :else :single)))

(defn- sneak-pair [chunks pos st clicked]
  (let [nf (when (contains? #{:x :z} (dir/axis clicked))
             (candidate-facing chunks pos st (dir/opposite clicked)))]
    (when (and nf (not= (dir/axis nf) (dir/axis clicked)))
      [nf (if (= (dir/counter-clockwise nf) (dir/opposite clicked))
            :right
            :left)])))

(defn- facing-and-type [chunks pos st face sneaking?]
  (let [clicked (dir/from-index (long face))
        pair (when sneaking? (sneak-pair chunks pos st clicked))
        facing (:facing (block/props-of st))]
    (cond
      pair pair
      sneaking? [facing :single]
      :else [facing (chest-type chunks pos st facing)])))

(defn placed
  "Returns the state of the chest st placed at pos against face. A
  sneaking player pairs it only along that face."
  [chunks pos st face sneaking?]
  (let [[facing type] (facing-and-type chunks pos st face sneaking?)
        st' (block/with st :facing facing :type type)]
    (if (= :single type)
      st'
      (copper-merged st' (state-at chunks (partner-pos pos st'))))))

(defn- joined-toward [chunks pos ^long st dir]
  (let [o (state-at chunks (dir/toward pos dir))
        props (block/props-of o)]
    (when (and (connects? st o)
               (not= :single (:type props))
               (= (:facing (block/props-of st)) (:facing props))
               (= (connected-direction o) (dir/opposite dir)))
      (if (= :left (:type props)) :right :left))))

(defn- joined-type [chunks pos ^long st]
  (some #(joined-toward chunks pos st %) [:north :south :west :east]))

(defn updated
  "Returns the chest st at pos paired with or parted from the chests
  next to it."
  [chunks pos ^long st]
  (if (= :single (:type (block/props-of st)))
    (if-let [t (joined-type chunks pos st)]
      (block/with st :type t)
      st)
    (let [o (state-at chunks (partner-pos pos st))]
      (if (connects? st o)
        (copper-merged st o)
        (block/with st :type :single)))))

(defn- nearest-looking [yaw pitch]
  (let [y (Math/toRadians (double yaw))
        p (Math/toRadians (double pitch))
        dx (- (* (Math/sin y) (Math/cos p)))
        dy (- (Math/sin p))
        dz (* (Math/cos y) (Math/cos p))
        ax (Math/abs dx) ay (Math/abs dy) az (Math/abs dz)]
    (cond
      (and (>= ay ax) (>= ay az)) (if (pos? dy) :up :down)
      (>= ax az) (if (pos? dx) :east :west)
      :else (if (pos? dz) :south :north))))

(defn barrel-placed
  "Returns the barrel st placed by a player who looks along yaw and
  pitch. It faces the player."
  [^long st yaw pitch]
  (block/with st :facing (dir/opposite (nearest-looking yaw pitch))))
