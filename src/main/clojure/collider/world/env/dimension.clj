(ns collider.world.env.dimension
  "Dimension types with the shape, light and attributes of a level."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(defn- attribute-value [v]
  (letfn [(entry [[k x]] [(data/kebab k) (attribute-value x)])]
    (cond
      (map? v) (into {} (map entry) v)
      (vector? v) (mapv attribute-value v)
      :else v)))

(defn attributes
  "Returns the environment attributes of json of a dimension type
  or a biome, by keyword."
  [json]
  (attribute-value (get json "attributes" {})))

(defn- spawn-light [v]
  (if (map? v)
    {:min (get v "min_inclusive") :max (get v "max_inclusive")}
    v))

(defn- shape [json]
  (hash-map
   :min-y (get json "min_y")
   :height (get json "height")
   :logical-height (get json "logical_height")
   :coordinate-scale (get json "coordinate_scale")
   :has-skylight (get json "has_skylight")
   :has-ceiling (get json "has_ceiling")
   :has-ender-dragon-fight (get json "has_ender_dragon_fight" false)
   :has-fixed-time (get json "has_fixed_time" false)))

(defn- environment [json]
  (hash-map
   :ambient-light (get json "ambient_light")
   :infiniburn (data/kebab (data/tag-name (get json "infiniburn")))
   :monster-spawn-light-level
   (spawn-light (get json "monster_spawn_light_level"))
   :monster-spawn-block-light-limit
   (get json "monster_spawn_block_light_limit")
   :skybox (data/kebab (get json "skybox" "overworld"))
   :cardinal-light (data/kebab (get json "cardinal_light" "default"))
   :attributes (attributes json)))

(defn- dimension-type [json]
  (let [clock (get json "default_clock")]
    (cond-> (merge (shape json) (environment json))
      clock (assoc :default-clock (data/kebab clock)))))

(def ^:private ^:table type-table
  (delay (into {}
               (map (fn [[id json]]
                      [(data/kebab id) (dimension-type json)]))
               (data/pack "dimension_type"))))

(defn types
  "Returns the fields of every dimension type, by name."
  []
  @type-table)

(defn type-of
  [dim]
  (get (types) dim))
