(ns collider.world.blocks.chest
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def types #{:chest :trapped-chest :copper-chest :weathering-copper-chest})
(def copper-types #{:copper-chest :weathering-copper-chest})
(def ^:private copper-chests (delay (set (get-in @data/tags ["block" "copper_chests"]))))


(defn state-at ^long [chunks pos]
  (chunk/chunks-get-block chunks gen/flat-chunk pos))

(defn connected-direction [^long st]
  (let [{:keys [type facing]} (block/props-of st)]
    (if (= :left type) (dir/clockwise facing) (dir/counter-clockwise facing))))

(defn connects? [^long self ^long other]
  (if (contains? copper-types (block/type-of self))
    (contains? @copper-chests (block/block-of other))
    (and (pos? other) (= (block/block-of self) (block/block-of other)))))

(defn partner-pos [pos ^long st]
  (mapv + pos (dir/offset (connected-direction st))))

(defn paired? [^long st ^long other]
  (let [a (block/props-of st) b (block/props-of other)]
    (and (connects? st other)
         (not= :single (:type b))
         (not= (:type a) (:type b))
         (= (:facing a) (:facing b)))))

(defn partner [chunks pos ^long st]
  (when (and (contains? types (block/type-of st))
             (not= :single (:type (block/props-of st))))
    (let [p2 (partner-pos pos st)]
      (when (paired? st (state-at chunks p2)) p2))))

(def ^:private oxidation
  {:copper-chest                 0 :exposed-copper-chest 1 :weathered-copper-chest 2 :oxidized-copper-chest 3
   :waxed-copper-chest           0 :waxed-exposed-copper-chest 1
   :waxed-weathered-copper-chest 2 :waxed-oxidized-copper-chest 3})

(def ^:private unwaxed
  {:waxed-copper-chest           :copper-chest :waxed-exposed-copper-chest :exposed-copper-chest
   :waxed-weathered-copper-chest :weathered-copper-chest
   :waxed-oxidized-copper-chest  :oxidized-copper-chest})

(defn- waxed? [b] (contains? unwaxed b))

(defn- least-oxidized [a b]
  (let [[a b] (if (= (waxed? a) (waxed? b)) [a b] [(get unwaxed a a) (get unwaxed b b)])]
    (if (<= (long (oxidation a 0)) (long (oxidation b 0))) a b)))

(defn- copper-merged [^long st ^long other]
  (if (and (contains? copper-types (block/type-of st))
           (contains? @copper-chests (block/block-of other)))
    (block/state (least-oxidized (block/block-of st) (block/block-of other)) (block/props-of st))
    st))

(defn- candidate-facing [chunks pos ^long st dir]
  (let [o (state-at chunks (mapv + pos (dir/offset dir)))]
    (when (and (connects? st o) (= :single (:type (block/props-of o))))
      (:facing (block/props-of o)))))

(defn- chest-type [chunks pos ^long st facing]
  (cond
    (= facing (candidate-facing chunks pos st (dir/clockwise facing))) :left
    (= facing (candidate-facing chunks pos st (dir/counter-clockwise facing))) :right
    :else :single))

(defn placed [chunks pos st face sneaking?]
  (let [props (block/props-of st)
        clicked (dir/from-index (long face))
        nf (when (and sneaking? (contains? #{:x :z} (dir/axis clicked)))
             (candidate-facing chunks pos st (dir/opposite clicked)))
        [facing type] (if (and nf (not= (dir/axis nf) (dir/axis clicked)))
                        [nf (if (= (dir/counter-clockwise nf) (dir/opposite clicked)) :right :left)]
                        [(:facing props) :single])
        type (if (and (= :single type) (not sneaking?))
               (chest-type chunks pos st facing)
               type)
        st' (block/state (block/block-of st) (assoc props :facing facing :type type))]
    (if (= :single type)
      st'
      (copper-merged st' (state-at chunks (partner-pos pos st'))))))

(defn- joined-type [chunks pos ^long st]
  (some (fn [dir]
          (let [o (state-at chunks (mapv + pos (dir/offset dir)))]
            (when (and (connects? st o)
                       (not= :single (:type (block/props-of o)))
                       (= (:facing (block/props-of st)) (:facing (block/props-of o)))
                       (= (connected-direction o) (dir/opposite dir)))
              (if (= :left (:type (block/props-of o))) :right :left))))
        [:north :south :west :east]))

(defn updated [chunks pos ^long st]
  (let [props (block/props-of st)]
    (if (= :single (:type props))
      (if-let [t (joined-type chunks pos st)]
        (block/state (block/block-of st) (assoc props :type t))
        st)
      (let [o (state-at chunks (partner-pos pos st))]
        (if (connects? st o)
          (copper-merged st o)
          (block/state (block/block-of st) (assoc props :type :single)))))))

(defn nearest-looking [yaw pitch]
  (let [y (Math/toRadians (double yaw)) p (Math/toRadians (double pitch))
        dx (- (* (Math/sin y) (Math/cos p)))
        dy (- (Math/sin p))
        dz (* (Math/cos y) (Math/cos p))]
    (cond
      (and (>= (Math/abs dy) (Math/abs dx)) (>= (Math/abs dy) (Math/abs dz))) (if (pos? dy) :up :down)
      (>= (Math/abs dx) (Math/abs dz)) (if (pos? dx) :east :west)
      :else (if (pos? dz) :south :north))))

(defn barrel-placed [^long st yaw pitch]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :facing (dir/opposite (nearest-looking yaw pitch)))))
