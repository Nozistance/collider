(ns collider.world.env.biome
  "Biomes and the temperature and precipitation they give a place."
  (:require [collider.data :as data]
            [collider.world.env.dimension :as dimension]))

(set! *warn-on-reflection* true)

(def ^:const sea-level
  "The sea level of the flat overworld."
  -63)

(def ^:const snow-level
  "The height above which the overworld snows in every biome."
  (+ sea-level 17))

(defn sea-level-of
  "Returns the sea level of the generator of level dim."
  ^long [dim]
  (case dim :the-nether 32 :the-end 0 sea-level))

(defn- spawner [m]
  {:type (data/kebab (get m "type")) :weight (get m "weight")
   :min (get m "minCount") :max (get m "maxCount")})

(defn- spawners [json]
  (into {} (map (fn [[k v]] [(data/kebab k) (mapv spawner v)]))
        (get json "spawners")))

(defn- biome [json]
  (let [modifier (get json "temperature_modifier")]
    (cond-> {:attributes (dimension/attributes json)
             :spawners (spawners json)
             :downfall (get json "downfall")
             :has-precipitation (get json "has_precipitation")
             :temperature (get json "temperature")}
      modifier (assoc :temperature-modifier (data/kebab modifier)))))

(def ^:private ^:table biome-table
  (delay (into {}
               (map (fn [[id json]] [(data/kebab id) (biome json)]))
               (data/pack "worldgen/biome"))))

(defn biomes
  "Returns the climate and attributes of every biome, by name."
  []
  @biome-table)

(def ^:private flat-biomes
  {:overworld :plains :the-nether :nether-wastes :the-end :the-end})

(defn- biome-in [dim n]
  (assoc (get (biomes) n) :name n :dimension dim))

(def ^:private ^:table flat
  (delay (into {} (for [[dim n] flat-biomes]
                    [dim (biome-in dim n)]))))

(defn at
  "Returns the biome of level dim at p.
  Every column of a flat level has the biome of its generator."
  [dim _p]
  (get @flat (or dim :overworld)))

(defn id
  "Returns the network id of the biome of level dim."
  ^long [dim]
  (data/datapack-id "worldgen/biome" (:name (at dim nil))))

(defn- temperature-noise ^double [^long _x ^long _z] 0.0)

(defn- chill ^double [^double base ^long y p]
  (let [n (temperature-noise (long (nth p 0)) (long (nth p 2)))
        v (float (* n 8.0))
        d (float (+ v (float (- (float y) (float snow-level)))))
        drop (float (/ (float (* d (float 0.05))) (float 40.0)))]
    (double (float (- (float base) drop)))))

(defn temperature
  "Returns the temperature of biome at p. It falls above the
  snow level."
  ^double [biome p]
  (let [y (long (nth p 1))
        base (float (:temperature biome))]
    (if (> y (long snow-level))
      (chill base y p)
      (double base))))

(defn warm-enough-to-rain?
  [biome p]
  (>= (temperature biome p) (double (float 0.15))))

(defn cold-enough-to-snow?
  [biome p]
  (not (warm-enough-to-rain? biome p)))

(defn precipitation-at
  "Returns :rain, :snow or :none for what falls from the sky of biome
  at p."
  [biome p]
  (cond
    (not (:has-precipitation biome)) :none
    (cold-enough-to-snow? biome p) :snow
    :else :rain))

(defn attribute
  "Returns environment attribute k of biome, else of its dimension."
  [biome k]
  (let [dim (dimension/type-of (:dimension biome))]
    (get (:attributes biome) k (get (:attributes dim) k))))

(defn increased-fire-burnout?
  [biome]
  (boolean (attribute biome :gameplay/increased-fire-burnout)))
