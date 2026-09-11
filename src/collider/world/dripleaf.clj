(ns collider.world.dripleaf
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]))

(set! *warn-on-reflection* true)

(defn- at ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks gen/flat-chunk p) -1))

(defn- up [p] (mapv + p [0 1 0]))
(defn- down [p] (mapv + p [0 -1 0]))
(defn leaf? [^long st] (= :big-dripleaf (block/type-of st)))
(defn stem? [^long st] (= :big-dripleaf-stem (block/type-of st)))
(defn small? [^long st] (= :small-dripleaf (block/type-of st)))
(defn dripleaf? [^long st] (or (leaf? st) (stem? st) (small? st)))
(defn- half-of [^long st] (:half (block/props-of st)))
(defn tilt-of [^long st] (:tilt (block/props-of st)))

(defn- water-source? [^long st]
  (and (pos? st)
       (or (block/waterlogged? st)
           (and (= :water (liquid/liquid-class st)) (liquid/source-state? st)))))

(defn leaf-supported? [chunks p]
  (let [b (at chunks (down p))]
    (or (leaf? b) (stem? b) (block/tagged? b "supports_big_dripleaf"))))

(defn stem-supported? [chunks p]
  (let [b (at chunks (down p)) a (at chunks (up p))]
    (and (or (stem? b) (block/tagged? b "supports_big_dripleaf"))
         (or (stem? a) (leaf? a)))))

(defn- may-place-small-on? [chunks p ^long below]
  (or (block/tagged? below "supports_small_dripleaf")
      (and (water-source? (at chunks p)) (block/tagged? below "supports_vegetation"))))

(defn small-supported? [chunks p ^long st]
  (let [b (at chunks (down p))]
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
    (leaf? (at chunks (up p)))
    (block/state :big-dripleaf-stem (select-keys (block/props-of st) [:facing :waterlogged]))
    :else st))

(defn small-updated ^long [chunks p ^long st]
  (let [upper? (= :upper (half-of st))
        partner (at chunks (if upper? (down p) (up p)))]
    (if (and (small? partner)
             (not= upper? (= :upper (half-of partner)))
             (small-supported? chunks p st))
      st
      0)))

(defn leaf-placed [chunks p ^long st]
  (let [b (at chunks (down p))
        st' (if (or (leaf? b) (stem? b))
              (block/state (block/block-of st) (assoc (block/props-of st) :facing (block/facing-of b)))
              st)]
    (when (leaf-supported? chunks p) st')))

(def ^:private next-tilt {:unstable :partial :partial :full :full :none})
(def ^:private tilt-delay {:unstable 10 :partial 10 :full 100})

(defn tilted ^long [^long st tilt]
  (block/state (block/block-of st) (assoc (block/props-of st) :tilt tilt)))

(defn tilt-sound [^long st]
  (case (tilt-of st)
    :unstable nil
    :none :big-dripleaf/tilt-up
    :big-dripleaf/tilt-down))

(defn can-tilt? [[_ y _] py on-ground?]
  (and (boolean on-ground?) (> (double py) (+ (double y) 0.6875))))

(defn- can-replace? [^long st]
  (or (zero? st) (= :water (block/block-of st)) (small? st)))

(defn- can-place-at? [chunks p]
  (let [st (at chunks p)]
    (and (not (neg? st)) (can-replace? st))))

(defn- watered [self chunks p props]
  (block/state self (assoc props :waterlogged (if (water-source? (at chunks p)) :true :false))))

(defn- leaf-state ^long [chunks p facing]
  (watered :big-dripleaf chunks p {:facing facing :tilt :none}))

(defn- stem-state ^long [chunks p facing]
  (watered :big-dripleaf-stem chunks p {:facing facing}))

(defn column-changes [chunks [x y z] facing ^long desired]
  (let [h (loop [n 0]
            (if (and (< n desired) (can-place-at? chunks [x (+ (long y) n) z])) (recur (inc n)) n))
        top (max (long y) (+ (long y) h -1))]
    (conj (mapv (fn [q] [[x q z] (stem-state chunks [x q z] facing)]) (range (long y) top))
          [[x top z] (leaf-state chunks [x top z] facing)])))

(defn leaf-meal [chunks p ^long st]
  (let [a (up p)]
    (when (can-place-at? chunks a)
      (let [f (block/facing-of st)]
        {:changes [[p (stem-state chunks p f)] [a (leaf-state chunks a f)]]}))))

(defn- head-pos [chunks p]
  (loop [q (up p)]
    (let [st (at chunks q)]
      (cond
        (stem? st) (recur (up q))
        (leaf? st) q))))

(defn stem-meal [chunks p ^long st]
  (when-let [h (head-pos chunks p)]
    (when (can-place-at? chunks (up h))
      (let [f (block/facing-of st) a (up h)]
        {:changes [[h (stem-state chunks h f)] [a (leaf-state chunks a f)]]}))))

(defn small-meal [chunks p ^long st roll]
  (let [lower (if (= :upper (half-of st)) (down p) p)
        base (at chunks lower)
        a (up lower)
        cleared (if (block/waterlogged? (at chunks a)) (block/state :water) 0)]
    (when (small? base)
      (let [chunks' (chunk/chunks-set-blocks chunks gen/flat-chunk [[a cleared]])
            desired (+ 2 (long (Math/floor (* 4.0 (double (roll :height))))))]
        {:changes (into [[a cleared]]
                        (column-changes chunks' lower (block/facing-of base) desired))}))))

(defn meal [chunks p ^long st roll]
  (cond
    (leaf? st) (leaf-meal chunks p st)
    (stem? st) (stem-meal chunks p st)
    (small? st) (small-meal chunks p st roll)))

(def rule
  {:name   :dripleaf
   :match? (fn [_chunks st _p] (dripleaf? st))
   :wake   (fn [chunks tick p _old self?]
             (let [st (at chunks p) delay (tilt-delay (tilt-of st))]
               (cond
                 (stem? st) (inc (long tick))
                 (and self? (leaf? st) delay) (+ (long tick) (long delay)))))
   :due    (fn [chunks p _ctx]
             (let [st (at chunks p)]
               (cond
                 (and (stem? st) (not (stem-supported? chunks p))) [[p (block/emptied st)]]
                 (and (leaf? st) (next-tilt (tilt-of st))) [[p (tilted st (next-tilt (tilt-of st)))]])))})
