(ns collider.world.env.biome
  "Biomes, and the temperature and precipitation they give a
  position.")

(set! *warn-on-reflection* true)

(def ^:const sea-level -63)

(def ^:const snow-level (+ sea-level 17))

(defn sea-level-of
  "Returns the sea level of the generator of level dim.
  The overworld is flat; the others keep their noise settings."
  ^long [dim]
  (case dim :the-nether 32 :the-end 0 sea-level))

(def plains
  {:name                    :plains
  :has-precipitation?      true
  :temperature             0.8
  :downfall                0.4
  :increased-fire-burnout? false})

(defn at [_chunks _p] plains)

(defn- temperature-noise ^double [^long _x ^long _z] 0.0)

(defn- chill ^double [^double base ^long y p]
  (let [n (temperature-noise (long (nth p 0)) (long (nth p 2)))
        v (float (* n 8.0))
        d (float (+ v (float (- (float y) (float snow-level)))))
        drop (float (/ (float (* d (float 0.05))) (float 40.0)))]
    (double (float (- (float base) drop)))))

(defn- height-adjusted-temperature ^double [biome p]
  (let [y (long (nth p 1))
        base (float (:temperature biome))]
    (if (> y (long snow-level))
      (chill base y p)
      (double base))))

(defn temperature
  "Returns the temperature of biome at p. It falls above the
  snow level."
  ^double [biome p]
  (height-adjusted-temperature biome p))

(defn warm-enough-to-rain? [biome p]
  (>= (temperature biome p) (double (float 0.15))))

(defn cold-enough-to-snow? [biome p]
  (not (warm-enough-to-rain? biome p)))

(defn precipitation-at
  "Returns what falls from the sky of biome at p: :rain, :snow or
  :none."
  [biome p]
  (cond
    (not (:has-precipitation? biome)) :none
    (cold-enough-to-snow? biome p) :snow
    :else :rain))

(defn increased-fire-burnout? [biome]
  (boolean (:increased-fire-burnout? biome)))
