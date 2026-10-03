(ns collider.world.blocks.grow.underwater
  "Seagrass and coral that bone meal grows in water over a floor."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.update :as update]
            [collider.world.blocks.grow.common :refer [meal-walk]]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(def ^:private ^:const tries 128)

(def ^:private plane-order [:north :east :south :west])

(def ^:private ^:table seagrass (delay (block/state :seagrass)))

(def ^:private ^:table meals
  (delay (vec (data/tag-values "block" "underwater_bonemeals"))))

(def ^:private ^:table wall-corals
  (delay (vec (data/tag-values "block" "wall_corals"))))

(defn- nth-by [xs ^double r] (nth xs (long (* (count xs) r))))

(defn- wall-coral? [st]
  (and (some #{(block/block-of st)} @wall-corals)
       (contains? (block/props-of st) :facing)))

(def ^:private step-parts {:x :x :y :y :n :h :z :z})

(defn- walked [at pos roll ^long j]
  (meal-walk roll (fn [i part] [j i (step-parts part)]) pos j
             #(not (block/full-cube? (max 0 (long (at %)))))))

(defn- coral-state [roll ^long j face]
  (cond
    (and (zero? j) (contains? dir/horizontal-offset face))
    (block/with (block/state (nth-by @wall-corals (roll [j :wall])))
                :facing face)
    (zero? (random/below (roll [j :odds]) 4))
    (block/state (nth-by @meals (roll [j :meal])))
    :else @seagrass))

(defn- settled [chunks p st roll j]
  (if (wall-coral? st)
    (loop [d 0 st st]
      (if (and (< d 4) (not (support/supported? chunks p st)))
        (let [side (nth-by plane-order (roll [j :turn d]))]
          (recur (inc d) (block/with st :facing side)))
        st))
    st))

(defn- tall [p]
  (let [half #(block/state :tall-seagrass {:half %})]
    [[p (half :lower) nil update/clients]
     [(dir/up p) (half :upper) nil update/clients]]))

(defn seagrass-meal
  "Returns the tall seagrass bone meal grows from the seagrass at p."
  [chunks p _st _roll]
  (when (block/water? (chunk/at chunks (dir/up p)))
    {:changes (tall p)}))

(defn- grown [at p st roll j]
  (let [cur (long (at p))]
    (cond
      (block/full-water? cur) [[p st]]
      (and (= :seagrass (block/block-of cur))
           (block/water? (at (dir/up p)))
           (zero? (random/below (roll [j :tall]) 10)))
      (tall p))))

(defn- one-try [chunks pos face roll corals? [at changes] j]
  (if-let [p (walked at pos roll j)]
    (let [st (if (corals? p) (coral-state roll j face) @seagrass)
          st (settled chunks p st roll j)
          cs (when (support/supported? chunks p st)
               (grown at p st roll j))
          placed (into {} (map (fn [[q s]] [q s])) cs)]
      [#(get placed % (at %)) (into changes cs)])
    [at changes]))

(defn meal
  "Returns the seagrass and coral that bone meal on the floor face
  grows around the water at pos, or nil unless water fills pos."
  [chunks pos face roll corals?]
  (when (block/full-water? (chunk/at chunks pos))
    (let [at0 #(chunk/at chunks %)
          step #(one-try chunks pos face roll corals? %1 %2)]
      (second (reduce step [at0 []] (range tries))))))
