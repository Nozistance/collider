(ns collider.world.blocks.dripleaf
  "Big and small dripleaf with their support, tilt and growth."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn leaf?
  "Returns true when st is the leaf of a big dripleaf."
  [^long st]
  (= :big-dripleaf (block/type-of st)))

(defn- stem? [^long st] (= :big-dripleaf-stem (block/type-of st)))

(defn- small? [^long st] (= :small-dripleaf (block/type-of st)))

(defn- dripleaf? [^long st] (or (leaf? st) (stem? st) (small? st)))

(defn- half-of [^long st] (:half (block/props-of st)))

(defn tilt-of
  [^long st]
  (:tilt (block/props-of st)))

(defn- big-base? [^long st]
  (block/tagged? st "supports_big_dripleaf"))

(defn- leaf-supported? [chunks p]
  (let [b (chunk/at-void chunks (dir/down p))]
    (or (leaf? b) (stem? b) (big-base? b))))

(defn- stem-supported? [chunks p]
  (let [b (chunk/at-void chunks (dir/down p))
        a (chunk/at-void chunks (dir/up p))]
    (and (or (stem? b) (big-base? b))
         (or (stem? a) (leaf? a)))))

(defn- may-place-small-on? [chunks p ^long below]
  (or (block/tagged? below "supports_small_dripleaf")
      (and (block/holds-water-source? (chunk/at-void chunks p))
           (block/tagged? below "supports_vegetation"))))

(defn- small-supported? [chunks p ^long st]
  (let [b (chunk/at-void chunks (dir/down p))]
    (if (= :upper (half-of st))
      (and (small? b) (= :lower (half-of b)))
      (may-place-small-on? chunks p b))))

(defn supported?
  [chunks p ^long st]
  (cond
    (leaf? st) (leaf-supported? chunks p)
    (stem? st) (stem-supported? chunks p)
    :else (small-supported? chunks p st)))

(defn leaf-topped
  "Returns the leaf st as a stem when another leaf sits on it."
  ^long [chunks p ^long st]
  (if (leaf? (chunk/at-void chunks (dir/up p)))
    (->> (select-keys (block/props-of st) [:facing :waterlogged])
         (block/state :big-dripleaf-stem))
    st))

(defn- small-gone? [chunks p ^long st side]
  (and (some? side) (not (small-supported? chunks p st))))

(defn- leaf-gone? [chunks p side]
  (and (= :down side) (not (leaf-supported? chunks p))))

(defn leaf-placed
  "Returns the placed leaf turned the way of the dripleaf under it,
  or nil when nothing holds it."
  [chunks p ^long st]
  (let [b (chunk/at-void chunks (dir/down p))]
    (when (leaf-supported? chunks p)
      (if (or (leaf? b) (stem? b))
        (block/with st :facing (block/facing-of b))
        st))))

(def ^:private next-tilt
  {:unstable :partial :partial :full :full :none})

(def ^:private tilt-delay {:unstable 10 :partial 10 :full 100})

(defn tilted
  ^long [^long st tilt]
  (block/with st :tilt tilt))

(defn- tilt-sound [^long st]
  (case (tilt-of st)
    :unstable nil
    :none :big-dripleaf/tilt-up
    :big-dripleaf/tilt-down))

(defn rests-on?
  "Returns true when a body on the ground at height py stands on the
  leaf at pos."
  [[_ y _] py on-ground?]
  (and (boolean on-ground?) (> (double py) (+ (double y) 0.6875))))

(defn- can-replace? [^long st]
  (or (zero? st) (= :water (block/block-of st)) (small? st)))

(defn- can-place-at? [chunks p]
  (let [st (chunk/at-void chunks p)]
    (and (not (neg? st)) (can-replace? st))))

(defn- watered [self chunks p props]
  (let [wet? (block/holds-water-source? (chunk/at-void chunks p))]
    (block/state self (assoc props :waterlogged (block/flag wet?)))))

(defn- leaf-state ^long [chunks p facing]
  (watered :big-dripleaf chunks p {:facing facing :tilt :none}))

(defn- stem-state ^long [chunks p facing]
  (watered :big-dripleaf-stem chunks p {:facing facing}))

(defn- free-height ^long [chunks [x y z] ^long desired]
  (loop [n 0]
    (if (and (< n desired)
             (can-place-at? chunks [x (+ (long y) n) z]))
      (recur (inc n))
      n)))

(defn- column-changes [chunks [x y z :as p] facing ^long desired]
  (let [y (long y)
        top (max y (+ y (free-height chunks p desired) -1))
        stem (fn [q] [[x q z] (stem-state chunks [x q z] facing)])]
    (conj (mapv stem (range y top))
          [[x top z] (leaf-state chunks [x top z] facing)])))

(defn- raised [chunks h ^long st]
  (let [f (block/facing-of st)
        a (dir/up h)]
    (when (can-place-at? chunks a)
      {:changes [[h (stem-state chunks h f)]
                 [a (leaf-state chunks a f)]]})))

(defn- head-pos [chunks p]
  (loop [q (dir/up p)]
    (let [st (chunk/at-void chunks q)]
      (cond
        (stem? st) (recur (dir/up q))
        (leaf? st) q))))

(defn- stem-meal [chunks p ^long st]
  (when-let [h (head-pos chunks p)]
    (raised chunks h st)))

(defn- small-meal [chunks p ^long st roll]
  (let [lower (if (= :upper (half-of st)) (dir/down p) p)
        base (chunk/at-void chunks lower)
        a (dir/up lower)
        wet? (block/waterlogged? (chunk/at-void chunks a))
        cleared (if wet? (block/state :water) 0)]
    (when (small? base)
      (let [chunks' (chunk/chunks-set-block chunks a cleared)
            h (random/below (roll :height) 4)
            f (block/facing-of base)
            col (column-changes chunks' lower f (+ 2 h))]
        {:changes (into [[a cleared nil 18]] col)}))))

(defn meal
  "Returns the changes bone meal makes on the dripleaf st at p, or
  nil when it does nothing."
  [chunks p ^long st roll]
  (cond
    (leaf? st) (raised chunks p st)
    (stem? st) (stem-meal chunks p st)
    (small? st) (small-meal chunks p st roll)))

(defn- stem-unsupported? [chunks p side]
  (and (#{:up :down} side) (not (stem-supported? chunks p))))

(defn- tilt-wake [^long st tick side]
  (when-let [wait (and (nil? side) (leaf? st)
                       (tilt-delay (tilt-of st)))]
    (+ (long tick) (long wait))))

(defn- wake [chunks _dim tick p _old side]
  (let [st (chunk/at-void chunks p)]
    (cond
      (stem? st) (when (stem-unsupported? chunks p side)
                   (inc (long tick)))
      (and (small? st) (small-gone? chunks p st side)) :neighbor
      (and (leaf? st) (leaf-gone? chunks p side)) :neighbor
      :else (tilt-wake st tick side))))

(defn- tilt-change [p ^long st tilt ctx]
  (let [st' (tilted st tilt)
        pitch (random/pitch (:tick ctx) p :tilt)]
    [p st' [[:sound (tilt-sound st') 1.0 pitch]]]))

(defn- due [chunks p ctx]
  (let [st (chunk/at-void chunks p)
        tilt (next-tilt (tilt-of st))]
    (cond
      (and (stem? st) (not (stem-supported? chunks p)))
      [(block/destroyed p st)]
      (and (leaf? st) tilt) [(tilt-change p st tilt ctx)])))

(defn- gone [chunks p _ctx]
  (let [st (chunk/at-void chunks p)]
    (when (if (small? st)
            (not (small-supported? chunks p st))
            (and (leaf? st) (not (leaf-supported? chunks p))))
      [(block/destroyed p st)])))

(def rule
  "The block rule that breaks dripleaves without support and tilts a
  big leaf back step by step."
  {:name    :dripleaf
   :match?  (fn [_chunks st _p] (dripleaf? st))
   :wake    wake
   :reshape gone
   :due     due})
