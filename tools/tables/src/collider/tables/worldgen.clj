(ns collider.tables.worldgen
  "Reading features, dimension types and biomes."
  (:require [clojure.string :as str]
            [collider.tables.files :refer [jsons read-json]]
            [collider.tables.value :refer [kw plain json-name]]))

(set! *warn-on-reflection* true)

(defn- feature-props [props]
  (into (sorted-map) (map (fn [[k v]] [(kw k) (keyword v)])) props))

(defn- state-value [m]
  (let [props (get m "Properties")]
    (cond-> (sorted-map :block (kw (get m "Name")))
      props (assoc :props (feature-props props)))))

(defn- feature-value [v]
  (cond
    (and (map? v) (contains? v "Name")) (state-value v)
    (map? v) (into (sorted-map)
                   (map (fn [[k x]] [(kw k) (feature-value x)]))
                   v)
    (vector? v) (mapv feature-value v)
    (not (string? v)) v
    (str/starts-with? v "#") {:tag (plain (subs v 1))}
    :else (kw v)))

(def ^:private selector-types
  #{"minecraft:random_selector" "minecraft:weighted_random_selector"
    "minecraft:simple_random_selector"
    "minecraft:random_boolean_selector"})

(def ^:private placed-fields
  #{"feature" "default_feature" "vegetation_feature"})

(defn- placed-refs [v]
  (let [ref (fn [[k x]]
              (if (and (string? x) (placed-fields k))
                [(plain x)]
                (placed-refs x)))]
    (cond
      (and (map? v) (contains? v "placement")) [v]
      (map? v) (mapcat ref v)
      (vector? v) (mapcat placed-refs v))))

(declare conf-features)

(defn- placed-of [reg p] (if (map? p) p (get (:placed reg) p)))

(defn- placed-features [reg p]
  (conf-features reg (plain (get (placed-of reg p) "feature"))))

(defn- conf-features [reg nm]
  (let [j (get (:configured reg) nm)]
    (into [nm]
          (when (selector-types (get j "type"))
            (mapcat #(placed-features reg %)
                    (placed-refs (get j "config")))))))

(defn- biome-features [reg tagged j]
  (let [xf (comp cat
                 (mapcat #(placed-features reg (plain %)))
                 (filter tagged))]
    (into [] xf (get j "features"))))

(defn- bone-meal-biomes [reg tagged]
  (into (sorted-map)
        (keep (fn [[nm j]]
                (let [fs (biome-features reg tagged j)]
                  (when (seq fs) [(kw nm) (mapv kw fs)]))))
        (:biomes reg)))

(defn- conf-placed-refs [reg nm]
  (placed-refs (get (get (:configured reg) nm) "config")))

(defn- closure [reg cs ps]
  (let [rs (mapcat #(conf-placed-refs reg %) cs)
        cs' (into cs (map #(plain (get (placed-of reg %) "feature")))
                  (concat ps rs))
        ps' (into ps (filter string?) rs)]
    (if (and (= cs cs') (= ps ps')) [cs' ps'] (recur reg cs' ps'))))

(defn- feature-set [reg kind names]
  (into (sorted-map)
        (map (fn [n] [(kw n) (feature-value (get (kind reg) n))]))
        names))

(def ^:private bone-meal-tag
  (str "data/minecraft/tags/worldgen/configured_feature"
       "/can_spawn_from_bone_meal.json"))

(defn- feature-registries [zf]
  (let [dir-of (fn [dir] (str "data/minecraft/worldgen/" dir "/"))]
    (into {}
          (map (fn [[k dir]] [k (jsons zf (dir-of dir))]))
          {:placed "placed_feature"
           :configured "configured_feature"
           :biomes "biome"})))

(defn features [zf placers]
  (let [reg (feature-registries zf)
        bone-meal (read-json zf bone-meal-tag)
        tagged (into #{} (map plain) (get bone-meal "values"))
        roots (into tagged (map (comp json-name second)) placers)
        [cs ps] (closure reg roots #{"grass_bonemeal"})]
    {:configured (feature-set reg :configured cs)
     :placed     (feature-set reg :placed ps)
     :bone-meal  (bone-meal-biomes reg tagged)
     :placers    placers}))

(defn- dimension-attr-value [v]
  (cond
    (map? v)
    (into (sorted-map)
          (map (fn [[k x]] [(kw k) (dimension-attr-value x)]))
          v)
    (vector? v) (mapv dimension-attr-value v)
    :else v))

(defn- dimension-attrs [json]
  (into (sorted-map)
        (map (fn [[k v]] [(kw k) (dimension-attr-value v)]))
        (get json "attributes")))

(defn- monster-spawn-light [v]
  (if (map? v)
    {:min (get v "min_inclusive")
     :max (get v "max_inclusive")}
    v))

(defn- dimension-shape [json]
  (sorted-map
   :min-y (get json "min_y")
   :height (get json "height")
   :logical-height (get json "logical_height")
   :coordinate-scale (get json "coordinate_scale")
   :has-skylight (get json "has_skylight")
   :has-ceiling (get json "has_ceiling")
   :has-ender-dragon-fight (get json "has_ender_dragon_fight" false)
   :has-fixed-time (get json "has_fixed_time" false)))

(defn- dimension-look [json]
  (sorted-map
   :ambient-light (get json "ambient_light")
   :infiniburn (kw (subs (get json "infiniburn") 1))
   :monster-spawn-light-level
   (monster-spawn-light (get json "monster_spawn_light_level"))
   :monster-spawn-block-light-limit
   (get json "monster_spawn_block_light_limit")
   :skybox (kw (get json "skybox" "overworld"))
   :cardinal-light (kw (get json "cardinal_light" "default"))
   :attributes (dimension-attrs json)))

(defn- dimension-type [json]
  (cond-> (merge (dimension-shape json) (dimension-look json))
    (contains? json "default_clock")
    (assoc :default-clock (kw (get json "default_clock")))))

(defn dimension-types [zf]
  (into (sorted-map)
        (map (fn [[name json]]
               [(kw name) (dimension-type json)]))
        (jsons zf "data/minecraft/dimension_type/")))

(defn- biome [json]
  (cond-> (sorted-map
           :has-precipitation (get json "has_precipitation")
           :temperature (get json "temperature")
           :downfall (get json "downfall")
           :attributes (dimension-attrs json))
    (contains? json "temperature_modifier")
    (assoc :temperature-modifier
           (kw (get json "temperature_modifier")))))

(defn biomes [zf]
  (into (sorted-map)
        (map (fn [[name json]] [(kw name) (biome json)]))
        (jsons zf "data/minecraft/worldgen/biome/")))
