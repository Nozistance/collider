(ns collider.world.blocks.precipitation
  "Rain and snow at the top of a column."
  (:require [collider.world.block :as block]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.biome :as biome]
            [collider.world.env.weather :as weather]
            [collider.world.light :as light]
            [collider.world.space.column :as column]))

(set! *warn-on-reflection* true)

(def ^:private ^:const rain-fill-chance 0.05)

(def ^:private ^:const powder-snow-fill-chance 0.1)

(defn- state-at ^long [chunks p]
  (if (chunk/in-range? (long (nth p 1)))
    (long (chunk/at chunks p))
    0))

(defn- block-light ^long [chunks p]
  (long (light/block-light-at chunks (nth p 0) (nth p 1) (nth p 2))))

(defn- water? [chunks p]
  (block/water? (state-at chunks p)))

(defn- water-sides? [chunks p]
  (every? #(water? chunks (dir/toward p %)) dir/horizontal))

(defn- should-freeze? [chunks biome p]
  (let [st (state-at chunks p)]
    (and (not (biome/warm-enough-to-rain? biome p))
         (chunk/in-range? (long (nth p 1)))
         (< (block-light chunks p) 10)
         (block/liquid? st)
         (block/water? st)
         (not (water-sides? chunks p)))))

(defn- should-snow? [chunks biome p]
  (let [st (state-at chunks p)]
    (and (= :snow (biome/precipitation-at biome p))
         (chunk/in-range? (long (nth p 1)))
         (< (block-light chunks p) 10)
         (or (zero? st) (= :snow (block/block-of st)))
         (support/supported? chunks p (block/state :snow)))))

(defn- more-snow [st p ^long max-height]
  (let [layers (block/prop-long st :layers)]
    (when (< layers (min max-height 8))
      [[p (block/with-long st :layers (inc layers))]])))

(defn- snow-change [chunks biome p ^long max-height]
  (when (and (pos? max-height) (should-snow? chunks biome p))
    (let [st (state-at chunks p)]
      (if (= :snow (block/block-of st))
        (more-snow st p max-height)
        [[p (block/state :snow)]]))))

(defn- fill-chance ^double [precipitation]
  (case precipitation
    :rain rain-fill-chance
    :snow powder-snow-fill-chance
    0.0))

(defn- empty-fill [precipitation]
  (case precipitation
    :rain (block/state :water-cauldron)
    :snow (block/state :powder-snow-cauldron)
    nil))

(def ^:private filled-by
  {:water-cauldron :rain :powder-snow-cauldron :snow})

(defn- layered-fill [^long st precipitation]
  (let [level (block/prop-long st :level)]
    (when (and (not= 3 level)
               (= (filled-by (block/block-of st)) precipitation))
      (block/with-long st :level (inc level)))))

(defn- cauldron-fill [^long st precipitation]
  (case (block/type-of st)
    :cauldron (empty-fill precipitation)
    :layered-cauldron (layered-fill st precipitation)
    nil))

(defn- cauldron-change [chunks p precipitation ^double roll]
  (when (< roll (fill-chance precipitation))
    (when-let [st (cauldron-fill (state-at chunks p) precipitation)]
      [[p st]])))

(defn- rain-changes [chunks biome top below max-height roll]
  (concat
    (snow-change chunks biome top (long max-height))
    (let [kind (biome/precipitation-at biome below)]
      (when (not= :none kind)
        (cauldron-change chunks below kind (double roll))))))

(defn- mild? [ctx x z]
  (let [p [(long x) (long chunk/max-y) (long z)]]
    (biome/warm-enough-to-rain? (biome/at (:dim ctx) p) p)))

(defn- column-changes [ctx chunks x z max-height roll]
  (let [top [(long x) (column/motion-blocking-height chunks x z)
             (long z)]
        below [(long x) (dec (long (nth top 1))) (long z)]
        biome (biome/at (:dim ctx) top)]
    (concat
      (when (should-freeze? chunks biome below)
        [[below (block/state :ice)]])
      (when (weather/raining? ctx)
        (rain-changes chunks biome top below max-height roll)))))

(defn tick-precipitation
  "Returns the changes the weather makes at the top of column x z.
  max-height is the most snow layers allowed there. roll decides
  whether a cauldron fills."
  [ctx chunks [x _ z] max-height roll]
  (when (or (weather/raining? ctx) (not (mild? ctx x z)))
    (column-changes ctx chunks x z max-height roll)))
