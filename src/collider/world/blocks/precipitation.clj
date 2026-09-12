(ns collider.world.blocks.precipitation
  (:require [collider.world.env.biome :as biome]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.light :as light]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.space.spawn :as spawn]
            [collider.world.blocks.support :as support]
            [collider.world.env.weather :as weather]))

(set! *warn-on-reflection* true)

(def ^:const rain-fill-chance 0.05)
(def ^:const powder-snow-fill-chance 0.1)

(defn- state-at ^long [chunks p]
  (if (chunk/in-range? (long (nth p 1)))
    (long (chunk/chunks-get-block chunks gen/flat-chunk p))
    0))

(defn- block-light ^long [chunks p]
  (long (light/block-light-at chunks gen/flat-chunk (nth p 0) (nth p 1) (nth p 2))))

(defn- water? [chunks p]
  (= :water (liquid/liquid-class (state-at chunks p))))

(def ^:private sides [[-1 0 0] [1 0 0] [0 0 -1] [0 0 1]])

(defn- should-freeze? [chunks biome p]
  (let [st (state-at chunks p)]
    (and (not (biome/warm-enough-to-rain? biome p))
         (chunk/in-range? (long (nth p 1)))
         (< (block-light chunks p) 10)
         (liquid/liquid-state? st)
         (= :water (liquid/liquid-class st))
         (not (every? (fn [d] (water? chunks (mapv + p d))) sides)))))

(defn- should-snow? [chunks biome p]
  (let [st (state-at chunks p)]
    (and (= :snow (biome/precipitation-at biome p))
         (chunk/in-range? (long (nth p 1)))
         (< (block-light chunks p) 10)
         (or (zero? st) (= :snow (block/block-of st)))
         (support/supported? chunks gen/flat-chunk p (block/state :snow)))))

(defn- snow-change [chunks biome p ^long max-height]
  (when (and (pos? max-height) (should-snow? chunks biome p))
    (let [st (state-at chunks p)]
      (if (= :snow (block/block-of st))
        (let [layers (long (Long/parseLong (name (:layers (block/props-of st)))))]
          (when (< layers (min max-height 8))
            [[p (block/state :snow {:layers (keyword (str (inc layers)))})]]))
        [[p (block/state :snow)]]))))

(defn- fill-chance ^double [precipitation]
  (case precipitation :rain rain-fill-chance :snow powder-snow-fill-chance 0.0))

(defn- cauldron-fill [^long st precipitation]
  (case (block/type-of st)
    :cauldron (case precipitation
                :rain (block/state :water-cauldron)
                :snow (block/state :powder-snow-cauldron)
                nil)
    :layered-cauldron
    (let [level (long (Long/parseLong (name (:level (block/props-of st)))))
          kind  (case (block/block-of st)
                  :water-cauldron :rain
                  :powder-snow-cauldron :snow
                  nil)]
      (when (and (not= 3 level) (= kind precipitation))
        (block/state (block/block-of st) {:level (keyword (str (inc level)))})))
    nil))

(defn- cauldron-change [chunks p precipitation ^double roll]
  (when (< roll (fill-chance precipitation))
    (when-let [st (cauldron-fill (state-at chunks p) precipitation)]
      [[p st]])))

(defn tick-precipitation [ctx chunks [x _ z] max-height roll]
  (let [top    [(long x) (spawn/motion-blocking-height chunks gen/flat-chunk x z) (long z)]
        below  [(long x) (dec (long (nth top 1))) (long z)]
        biome  (biome/at chunks top)]
    (concat
     (when (should-freeze? chunks biome below) [[below (block/state :ice)]])
     (when (weather/raining? ctx)
       (concat
        (snow-change chunks biome top (long max-height))
        (let [precipitation (biome/precipitation-at biome below)]
          (when (not= :none precipitation)
            (cauldron-change chunks below precipitation (double roll)))))))))
