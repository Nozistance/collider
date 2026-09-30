(ns collider.world.blocks.grow.underwater
  "Bone meal in water over a floor: seagrass, and coral where the
  biome grows it."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const tries 128)

(def ^:private turns [:north :east :south :west])

(def ^:private ^:table seagrass (delay (block/state :seagrass)))

(def ^:private ^:table meals
  (delay (vec (data/tag-values "block" "underwater_bonemeals"))))

(def ^:private ^:table wall-corals
  (delay (vec (data/tag-values "block" "wall_corals"))))

(defn full-water?
  "Returns true when st is water that fills its block."
  [st]
  (and (block/water? st)
       (let [l (block/liquid-level st)] (or (zero? l) (>= l 8)))))

(defn- roll-int [roll n & ks]
  (long (* (long n) (double (roll (vec ks))))))

(defn- pick [xs ^double r] (nth xs (long (* (count xs) r))))

(defn- faced [st face]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :facing face)))

(defn- wall-coral? [st]
  (and (some #{(block/block-of st)} @wall-corals)
       (contains? (block/props-of st) :facing)))

(defn- stride [roll j i]
  (let [r #(roll-int roll 3 j i %)]
    [(dec (r :x)) (quot (* (dec (r :y)) (r :h)) 2) (dec (r :z))]))

(defn- walked [at pos roll ^long j]
  (loop [i 0 p pos]
    (if (< i (quot j 16))
      (let [p' (mapv + p (stride roll j i))]
        (when-not (block/full-cube? (max 0 (long (at p'))))
          (recur (inc i) p')))
      p)))

(defn- coral-state [roll ^long j face]
  (cond
    (and (zero? j) (#{:north :south :west :east} face))
    (faced (block/state (pick @wall-corals (roll [j :wall]))) face)
    (zero? (roll-int roll 4 j :odds))
    (block/state (pick @meals (roll [j :meal])))
    :else @seagrass))

(defn- settled [chunks p st roll j]
  (if (wall-coral? st)
    (loop [d 0 st st]
      (if (and (< d 4) (not (support/supported? chunks p st)))
        (recur (inc d) (faced st (pick turns (roll [j :turn d]))))
        st))
    st))

(defn- tall [p]
  (let [half #(block/state :tall-seagrass {:half %})]
    [[p (half :lower) nil 2]
     [(mapv + p [0 1 0]) (half :upper) nil 2]]))

(defn- grown [at p st roll j]
  (let [cur (long (at p))]
    (cond
      (full-water? cur) [[p st]]
      (and (= :seagrass (block/block-of cur))
           (block/water? (at (mapv + p [0 1 0])))
           (zero? (roll-int roll 10 j :tall)))
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
  "Returns the changes of bone meal on the water at pos, clicked on
  face of the floor next to it. Returns nil unless pos holds water
  that fills it. Roll gives a number from 0 to 1 for a key, and
  corals? tells whether the biome at a position grows coral."
  [chunks pos face roll corals?]
  (when (full-water? (chunk/at chunks pos))
    (let [at0 #(chunk/at chunks %)
          step #(one-try chunks pos face roll corals? %1 %2)]
      (second (reduce step [at0 []] (range tries))))))
