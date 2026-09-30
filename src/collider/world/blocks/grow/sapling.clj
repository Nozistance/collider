(ns collider.world.blocks.grow.sapling
  "Saplings, mangrove propagules and azaleas growing into trees."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.weather :as weather]
            [collider.world.feature :as feature]
            [collider.world.feature.worldgen :as lv]
            [collider.world.feature.tree :as tree]
            [collider.world.feature.trunk :refer [off]]
            [collider.world.blocks.grow.common
             :refer [age aged chance? with]]))

(set! *warn-on-reflection* true)

(defn- grower [st]
  (get (data/growers)
       (if (= :azalea (block/type-of st))
         :azalea
         (:grower (get (data/blocks) (block/block-of st))))))

(defn- min-height ^long [g]
  (long (or (some-> (:tree g) feature/configured-feature
                    :config :trunk-placer :base-height)
            0)))

(defn- flowers? [l p]
  (boolean (some #(block/tagged? (lv/at l %) "flowers")
                 (for [x (range -2 3) y (range -1 2) z (range -2 3)]
                   (off p x y z)))))

(defn- mega-key [l g]
  (if (and (:secondary-mega g) (lv/chance? l (:secondary-chance g)))
    (:secondary-mega g)
    (:mega g)))

(defn- tree-key [l g flowers?]
  (let [second? (lv/chance? l (:secondary-chance g))]
    (or (when second?
          (or (when flowers? (:secondary-flowers g)) (:secondary-tree g)))
        (if (and flowers? (:flowers g)) (:flowers g) (:tree g)))))

(defn- square [p [dx dz]]
  (for [[x z] [[0 0] [1 0] [0 1] [1 1]]]
    (off p (+ (long dx) x) 0 (+ (long dz) z))))

(defn- fill [l cells st]
  (reduce #(lv/put %1 %2 st 260) l cells))

(defn- placed [l k p]
  (tree/place l (:config (feature/configured-feature k)) p))

(defn- mega [l p st k]
  (let [same? #(= (block/block-of st) (block/block-of (lv/at l %)))
        corner (first (filter #(every? same? (square p %))
                              [[0 0] [0 -1] [-1 0] [-1 -1]]))]
    (when corner
      (let [cells (square p corner)
            l (placed (fill l cells 0) k (first cells))]
        (if (:placed? l) l (fill l cells st))))))

(defn- empty-of ^long [st]
  (if (block/waterlogged? st) (block/state :water) 0))

(defn- single [l p st k]
  (let [empty (empty-of st)
        l (placed (lv/put l p empty 260) k p)]
    (if (:placed? l)
      (cond-> l (= empty (lv/at l p)) (update :changes conj [:send p]))
      (lv/put l p st 260))))

(defn- grow
  "TreeGrower.growTree: the changes of a tree grown from st at p."
  [chunks p st roll]
  (let [g (grower st)
        l (lv/start chunks roll :tree)
        big (some->> (mega-key l g) (mega l p st))]
    (:changes
      (or big
          (let [k (tree-key l g (flowers? l p))]
            (if k (single l p st k) l))))))

(defn- advance
  "SaplingBlock.advanceTree."
  [chunks p st roll]
  (if (zero? (block/prop-long st :stage))
    [[p (with st :stage 1) nil 260]]
    (grow chunks p st roll)))

(defn- meal-roll? [roll]
  (< (double (float (roll :success))) 0.45))

(defn tick
  "Returns the changes of a random tick of the sapling st at p."
  [chunks p st roll time ctx]
  (when (and (>= (weather/brightness ctx chunks (p 0) (inc (long (p 1)))
                                     (p 2) time)
                 9)
             (chance? roll :gate 7))
    (advance chunks p st roll)))

(defn meal
  "Returns the bone meal result for the sapling st at p."
  [chunks p st roll]
  (when (chunk/in-range? (+ (long (p 1)) (min-height (grower st))))
    {:changes (if (meal-roll? roll) (advance chunks p st roll) [])}))

(defn- hanging? [st] (= :true (:hanging (block/props-of st))))

(defn propagule-tick
  "Returns the changes of a random tick of the propagule st at p."
  [chunks p st roll _time _ctx]
  (cond
    (not (hanging? st))
    (when (chance? roll :gate 7) (advance chunks p st roll))
    (< (age st) 4) [[p (aged st (inc (age st))) nil 2]]))

(defn propagule-meal
  "Returns the bone meal result for the propagule st at p."
  [chunks p st roll]
  (cond
    (not (hanging? st))
    {:changes (if (meal-roll? roll) (advance chunks p st roll) [])}
    (< (age st) 4) {:changes [[p (aged st (inc (age st))) nil 2]]}))

(defn azalea-meal
  "Returns the bone meal result for the azalea st at p."
  [chunks p st roll]
  (let [up (dir/up p)
        h (+ (long (p 1)) (min-height (grower st)) 2)]
    (when (and (chunk/in-range? h)
               (nil? (block/liquid-class (chunk/at chunks up))))
      {:changes (if (meal-roll? roll) (grow chunks p st roll) [])})))
