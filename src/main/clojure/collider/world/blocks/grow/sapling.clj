(ns collider.world.blocks.grow.sapling
  "Saplings, mangrove propagules and azaleas growing into trees."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.weather :as weather]
            [collider.world.feature :as feature]
            [collider.world.feature.worldgen :as worldgen]
            [collider.world.feature.tree :as tree]
            [collider.world.feature.trunk :refer [off]]
            [collider.world.update :as update]
            [collider.world.blocks.grow.common
             :refer [age chance? older]]))

(set! *warn-on-reflection* true)

(defn- grower-of [st]
  (get (data/growers)
       (if (= :azalea (block/type-of st))
         :azalea
         (:grower (get (data/blocks) (block/block-of st))))))

(defn- min-height ^long [grower]
  (long (or (some-> (:tree grower) feature/configured-feature
                    :config :trunk-placer :base-height)
            0)))

(defn- flowers? [level p]
  (boolean (some #(block/tagged? (worldgen/at level %) "flowers")
                 (for [x (range -2 3) y (range -1 2) z (range -2 3)]
                   (off p x y z)))))

(defn- mega-key [level grower]
  (if (and (:secondary-mega grower)
           (worldgen/chance? level (:secondary-chance grower)))
    (:secondary-mega grower)
    (:mega grower)))

(defn- tree-key [level grower flowers?]
  (let [second? (worldgen/chance? level (:secondary-chance grower))]
    (or (when second?
          (or (when flowers? (:secondary-flowers grower))
              (:secondary-tree grower)))
        (if (and flowers? (:flowers grower))
          (:flowers grower)
          (:tree grower)))))

(defn- square [p [dx dz]]
  (for [[x z] [[0 0] [1 0] [0 1] [1 1]]]
    (off p (+ (long dx) x) 0 (+ (long dz) z))))

(defn- fill [level cells st]
  (reduce #(worldgen/put %1 %2 st update/quiet) level cells))

(defn- placed [level feature-key p]
  (let [config (:config (feature/configured-feature feature-key))]
    (tree/place level config p)))

(def ^:private corners [[0 0] [0 -1] [-1 0] [-1 -1]])

(defn- mega-corner [level p st]
  (let [self (block/block-of st)
        same? #(= self (block/block-of (worldgen/at level %)))]
    (first (filter #(every? same? (square p %)) corners))))

(defn- mega [level p st feature-key]
  (when-let [corner (mega-corner level p st)]
    (let [cells (square p corner)
          cleared (fill level cells 0)
          level (placed cleared feature-key (first cells))]
      (if (:placed? level) level (fill level cells st)))))

(defn- empty-of ^long [st]
  (if (block/waterlogged? st) (block/state :water) 0))

(defn- resent [level p]
  (update level :changes conj [:send p]))

(defn- single [level p st feature-key]
  (let [empty (empty-of st)
        level (placed (worldgen/put level p empty update/quiet)
                      feature-key p)]
    (if (:placed? level)
      (cond-> level (= empty (worldgen/at level p)) (resent p))
      (worldgen/put level p st update/quiet))))

(defn- grow
  "Returns the changes of a tree grown from the sapling st at p."
  [chunks p st roll]
  (let [grower (grower-of st)
        level (worldgen/start chunks roll :tree)
        big (some->> (mega-key level grower) (mega level p st))]
    (:changes
      (or big
          (let [k (tree-key level grower (flowers? level p))]
            (if k (single level p st k) level))))))

(defn- advance
  "Returns the changes of one growth stage of the sapling st at p."
  [chunks p st roll]
  (if (zero? (block/prop-long st :stage))
    [[p (block/with st :stage 1) nil update/quiet]]
    (grow chunks p st roll)))

(defn- meal-roll? [roll]
  (< (double (float (roll :success))) 0.45))

(defn- meal-result [roll f]
  {:changes (if (meal-roll? roll) (f) [])})

(defn tick
  "Returns the changes of a random tick of the sapling st at p."
  [chunks p st roll time world]
  (let [[x y z] p up-y (inc (long y))
        light (weather/brightness world chunks x up-y z time)]
    (when (and (>= light 9) (chance? roll :gate 7))
      (advance chunks p st roll))))

(defn meal
  "Returns the bone meal result for the sapling st at p."
  [chunks p st roll]
  (when (chunk/in-range? (+ (long (p 1)) (min-height (grower-of st))))
    (meal-result roll #(advance chunks p st roll))))

(defn- hanging? [st] (= :true (:hanging (block/props-of st))))

(defn propagule-tick
  "Returns the changes of a random tick of the propagule st at p."
  [chunks p st roll _time _world]
  (cond
    (not (hanging? st))
    (when (chance? roll :gate 7) (advance chunks p st roll))
    (< (age st) 4) [[p (older st) nil update/clients]]))

(defn propagule-meal
  "Returns the bone meal result for the propagule st at p."
  [chunks p st roll]
  (cond
    (not (hanging? st)) (meal-result roll #(advance chunks p st roll))
    (< (age st) 4) {:changes [[p (older st) nil update/clients]]}))

(defn azalea-meal
  "Returns the bone meal result for the azalea st at p."
  [chunks p st roll]
  (let [up (dir/up p)
        h (+ (long (p 1)) (min-height (grower-of st)) 2)]
    (when (and (chunk/in-range? h)
               (nil? (block/liquid-class (chunk/at chunks up))))
      (meal-result roll #(grow chunks p st roll)))))
