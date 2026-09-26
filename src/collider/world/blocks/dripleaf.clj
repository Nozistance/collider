(ns collider.world.blocks.dripleaf
  "Big and small dripleaf: support, tilting and growth."
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn leaf? [^long st] (= :big-dripleaf (block/type-of st)))

(defn stem? [^long st] (= :big-dripleaf-stem (block/type-of st)))

(defn small? [^long st] (= :small-dripleaf (block/type-of st)))

(defn dripleaf? [^long st] (or (leaf? st) (stem? st) (small? st)))

(defn- half-of [^long st] (:half (block/props-of st)))

(defn tilt-of [^long st] (:tilt (block/props-of st)))

(defn- water-source? [^long st]
  (and (pos? st)
       (or (block/waterlogged? st)
           (block/water-source? st))))

(defn- big-base? [^long st]
  (block/tagged? st "supports_big_dripleaf"))

(defn leaf-supported? [chunks p]
  (let [b (chunk/at-void chunks (dir/down p))]
    (or (leaf? b) (stem? b) (big-base? b))))

(defn stem-supported? [chunks p]
  (let [b (chunk/at-void chunks (dir/down p))
        a (chunk/at-void chunks (dir/up p))]
    (and (or (stem? b) (big-base? b))
         (or (stem? a) (leaf? a)))))

(defn- may-place-small-on? [chunks p ^long below]
  (or (block/tagged? below "supports_small_dripleaf")
      (and (water-source? (chunk/at-void chunks p))
           (block/tagged? below "supports_vegetation"))))

(defn small-supported? [chunks p ^long st]
  (let [b (chunk/at-void chunks (dir/down p))]
    (if (= :upper (half-of st))
      (and (small? b) (= :lower (half-of b)))
      (may-place-small-on? chunks p b))))

(defn supported? [chunks p ^long st]
  (cond
    (leaf? st) (leaf-supported? chunks p)
    (stem? st) (stem-supported? chunks p)
    :else (small-supported? chunks p st)))

(defn leaf-updated ^long [chunks p ^long st]
  (cond
    (not (leaf-supported? chunks p)) 0
    (leaf? (chunk/at-void chunks (dir/up p)))
    (->> (select-keys (block/props-of st) [:facing :waterlogged])
         (block/state :big-dripleaf-stem))
    :else st))

(defn small-updated ^long [chunks p ^long st]
  (let [upper? (= :upper (half-of st))
        q (if upper? (dir/down p) (dir/up p))
        partner (chunk/at-void chunks q)]
    (if (and (small? partner)
             (not= upper? (= :upper (half-of partner)))
             (small-supported? chunks p st))
      st
      0)))

(defn- facing ^long [^long st f]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :facing f)))

(defn leaf-placed
  "Returns the placed leaf turned the way of the dripleaf under it,
  or nil when nothing holds it."
  [chunks p ^long st]
  (let [b (chunk/at-void chunks (dir/down p))]
    (when (leaf-supported? chunks p)
      (if (or (leaf? b) (stem? b))
        (facing st (block/facing-of b))
        st))))

(def ^:private next-tilt
  {:unstable :partial :partial :full :full :none})

(def ^:private tilt-delay {:unstable 10 :partial 10 :full 100})

(defn tilted ^long [^long st tilt]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :tilt tilt)))

(defn tilt-sound [^long st]
  (case (tilt-of st)
    :unstable nil
    :none :big-dripleaf/tilt-up
    :big-dripleaf/tilt-down))

(defn can-tilt?
  "Returns true when something at height py rests on the leaf.
  The block of the leaf starts at y."
  [[_ y _] py on-ground?]
  (and (boolean on-ground?) (> (double py) (+ (double y) 0.6875))))

(defn- can-replace? [^long st]
  (or (zero? st) (= :water (block/block-of st)) (small? st)))

(defn- can-place-at? [chunks p]
  (let [st (chunk/at-void chunks p)]
    (and (not (neg? st)) (can-replace? st))))

(defn- watered [self chunks p props]
  (let [wet? (water-source? (chunk/at-void chunks p))]
    (block/state self
                 (assoc props :waterlogged (if wet? :true :false)))))

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

(defn column-changes
  "Returns the changes that grow a big dripleaf column from p up to
  the desired height, as far as free space allows: stems and a leaf
  on top."
  [chunks [x y z :as p] facing ^long desired]
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

(defn leaf-meal [chunks p ^long st]
  (raised chunks p st))

(defn- head-pos [chunks p]
  (loop [q (dir/up p)]
    (let [st (chunk/at-void chunks q)]
      (cond
        (stem? st) (recur (dir/up q))
        (leaf? st) q))))

(defn stem-meal [chunks p ^long st]
  (when-let [h (head-pos chunks p)]
    (raised chunks h st)))

(defn small-meal [chunks p ^long st roll]
  (let [lower (if (= :upper (half-of st)) (dir/down p) p)
        base (chunk/at-void chunks lower)
        a (dir/up lower)
        wet? (block/waterlogged? (chunk/at-void chunks a))
        cleared (if wet? (block/state :water) 0)]
    (when (small? base)
      (let [chunks' (chunk/chunks-set-blocks chunks [[a cleared]])
            h (Math/floor (* 4.0 (double (roll :height))))
            f (block/facing-of base)
            col (column-changes chunks' lower f (+ 2 (long h)))]
        {:changes (into [[a cleared]] col)}))))

(defn meal [chunks p ^long st roll]
  (cond
    (leaf? st) (leaf-meal chunks p st)
    (stem? st) (stem-meal chunks p st)
    (small? st) (small-meal chunks p st roll)))

(defn- wake [chunks _dim tick p _old self?]
  (let [st (chunk/at-void chunks p)
        delay (tilt-delay (tilt-of st))]
    (cond
      (stem? st) (inc (long tick))
      (and self? (leaf? st) delay) (+ (long tick) (long delay)))))

(defn- due [chunks p _ctx]
  (let [st (chunk/at-void chunks p)
        tilt (next-tilt (tilt-of st))]
    (cond
      (and (stem? st) (not (stem-supported? chunks p)))
      [[p (block/emptied st)]]
      (and (leaf? st) tilt) [[p (tilted st tilt)]])))

(def rule
  {:name   :dripleaf
   :match? (fn [_chunks st _p] (dripleaf? st))
   :wake   wake
   :due    due})
