(ns collider.world.blocks.grow.turf
  "Grass, mycelium and farmland, and what bone meal grows on grass."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.weather :as weather]
            [collider.world.feature :as feature]
            [collider.world.update :as update]
            [collider.world.blocks.grass :as grass]
            [collider.world.blocks.grow.common
             :refer [air-at? flagged meal-walk]]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn- seen ^long [chunks changes q]
  (or (some (fn [[c st]] (when (= c q) st)) changes)
      (chunk/at chunks q)))

(defn- spread-target? [chunks changes self q]
  (and (= :dirt (block/block-of (seen chunks changes q)))
       (let [up (seen chunks changes (dir/up q))]
         (and (grass/can-stay-alive? (block/state self) up)
              (not (block/water? up))))))

(def ^:private spread-salts
  (mapv (fn [i] [[:x i] [:y i] [:z i]]) (range 4)))

(defn- spread-offset [p roll i]
  (let [[sx sy sz] (spread-salts i)]
    [(+ (long (p 0)) (dec (random/below (roll sx) 3)))
     (+ (long (p 1)) (- (random/below (roll sy) 5) 3))
     (+ (long (p 2)) (dec (random/below (roll sz) 3)))]))

(defn- spread-try [chunks p self roll changes i]
  (let [q (spread-offset p roll i)]
    (if (spread-target? chunks changes self q)
      (let [up (seen chunks changes (dir/up q))
            snowy (block/flag (block/tagged? up "snow"))]
        (conj changes [q (block/state self {:snowy snowy})]))
      changes)))

(defn- spread-cells [chunks p st roll]
  (let [self (block/block-of st)]
    (loop [i 0 changes []]
      (if (= i (count spread-salts))
        (not-empty changes)
        (recur (inc i) (spread-try chunks p self roll changes i))))))

(defn spread-tick
  "Returns the changes of a random tick of grass or mycelium at p."
  [chunks p st roll time world]
  (let [[x y z] p up-y (inc (long y))]
    (if-not (grass/can-stay-alive? st (chunk/at chunks (dir/up p)))
      [[p (grass/dirt-state)]]
      (when (>= (weather/brightness world chunks x up-y z time) 9)
        (spread-cells chunks p st roll)))))

(def ^:private water-offsets
  (for [dx (range -4 5) dy [0 1] dz (range -4 5)] [dx dy dz]))

(defn- near-water? [chunks p]
  (boolean (some #(block/water? (chunk/at chunks (mapv + p %)))
                 water-offsets)))

(defn farmland-tick
  "Returns the changes of a random tick of farmland at p."
  [chunks p st _roll _time _world]
  (let [m (block/prop-long st :moisture)
        above (chunk/at chunks (dir/up p))]
    (cond
      (near-water? chunks p)
      (when (< m 7)
        [[p (block/with st :moisture 7) nil update/clients]])
      (pos? m)
      [[p (block/with st :moisture (dec m)) nil update/clients]]
      (not (block/tagged? above "maintains_farmland"))
      [[p (grass/dirt-state)]])))

(defn- grown-tall [acc q ^long st]
  (let [[tall upper] (grass/tall-of st)
        up (dir/up q)]
    (when (and (support/supported? (feature/chunks acc) q tall)
               (air-at? (feature/chunks acc) up))
      (-> acc
          (feature/set-state q tall)
          (feature/set-state up upper)))))

(defn- turf-short [acc q st j roll]
  (if (and (= :short-grass (block/block-of (long st)))
           (zero? (random/below (roll [:tall j]) 10)))
    (or (grown-tall acc q st) acc)
    acc))

(defn- turf-flower [acc q j roll salt biome]
  (let [fs (feature/bone-meal-features biome)]
    (if (empty? fs)
      acc
      (let [f (nth fs (random/below (roll [:which j]) (count fs)))]
        (feature/configured acc f q roll salt)))))

(defn- turf-plant [acc q st j roll biome]
  (let [salt [:grow j]]
    (cond
      (not (and (zero? (long st)) (chunk/in-range? (q 1)))) acc
      (pos? (random/below (roll [:kind j]) 8))
      (feature/placed acc :grass-bonemeal q roll salt)
      :else (turf-flower acc q j roll salt biome))))

(defn- walkable? [acc self q]
  (and (= self (block/block-of (feature/state-at acc (dir/down q))))
       (not (block/full-cube? (feature/state-at acc q)))))

(defn- turf-try [acc self p j roll biome]
  (let [salt (fn [i part] [:walk j i part])
        pass? #(walkable? acc self %)]
    (if-let [q (meal-walk roll salt (dir/up p) j pass?)]
      (let [st (feature/state-at acc q)]
        (-> (turf-short acc q st j roll)
            (turf-plant q st j roll biome)))
      acc)))

(def ^:private ^:const tries 128)

(defn turf-meal
  "Returns the bone meal result for a grass block at p in biome."
  [chunks p st roll biome]
  (when (air-at? chunks (dir/up p))
    (let [self (block/block-of st)
          n (:name biome)
          acc (reduce (fn [acc j] (turf-try acc self p j roll n))
                      (feature/start chunks) (range tries))]
      {:changes (flagged update/clients (feature/cells acc))})))

(defn placer-meal
  "Returns the patch bone meal grows on a block that places one."
  [chunks p st roll]
  (when (air-at? chunks (dir/up p))
    (let [f (feature/placer-feature (block/block-of st))
          start (feature/start chunks)
          acc (feature/configured start f (dir/up p) roll [:patch])]
      {:changes (flagged update/clients (feature/cells acc))})))
