(ns collider.world.biome)

(set! *warn-on-reflection* true)

(def ^:const sea-level -63)
(def ^:const snow-level (+ sea-level 17))

(def plains
  {:name                    :plains
   :has-precipitation?      true
   :temperature             0.8
   :downfall                0.4
   :increased-fire-burnout? false})

(defn at [_chunks _p] plains)

(defn- temperature-noise ^double [^long _x ^long _z] 0.0)

(defn- height-adjusted-temperature ^double [biome p]
  (let [y    (long (nth p 1))
        base (float (:temperature biome))]
    (if (> y (long snow-level))
      (let [v (float (* (temperature-noise (long (nth p 0)) (long (nth p 2))) 8.0))
            d (float (+ v (float (- (float y) (float snow-level)))))]
        (double (float (- base (float (/ (float (* d (float 0.05))) (float 40.0)))))))
      (double base))))

(defn temperature ^double [biome p]
  (height-adjusted-temperature biome p))

(defn warm-enough-to-rain? [biome p]
  (>= (temperature biome p) (double (float 0.15))))

(defn cold-enough-to-snow? [biome p]
  (not (warm-enough-to-rain? biome p)))

(defn precipitation-at [biome p]
  (cond
    (not (:has-precipitation? biome)) :none
    (cold-enough-to-snow? biome p)    :snow
    :else                             :rain))

(defn increased-fire-burnout? [biome]
  (boolean (:increased-fire-burnout? biome)))
