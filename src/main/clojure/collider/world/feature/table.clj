(ns collider.world.feature.table
  "The features that bone meal reaches, read from the datapack."
  (:require [clojure.string :as str]
            [collider.data :as data]))

(set! *warn-on-reflection* true)

(defn- plain [s] (str/replace (str s) #"^minecraft:" ""))

(defn- kw [s] (keyword (str/replace (plain s) "_" "-")))

(defn- by-key [kvs] (into {} (sort-by first kvs)))

(defn- by-name [path] (update-keys (data/pack path) plain))

(defn- state-value [m]
  (let [props (get m "Properties")
        prop (fn [[k v]] [(kw k) (keyword v)])]
    (cond-> {:block (kw (get m "Name"))}
      props (assoc :props (by-key (map prop props))))))

(defn- feature-value [v]
  (cond
    (and (map? v) (contains? v "Name")) (state-value v)
    (map? v) (by-key (map (fn [[k x]] [(kw k) (feature-value x)]) v))
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

(defn- placed-of [reg p] (if (map? p) p (get (:placed reg) p)))

(defn- placed-feature [reg p]
  (plain (get (placed-of reg p) "feature")))

(defn- conf-placed-refs [reg nm]
  (placed-refs (get (get (:configured reg) nm) "config")))

(defn- conf-features [reg nm]
  (let [j (get (:configured reg) nm)
        nested #(conf-features reg (placed-feature reg %))]
    (into [nm]
          (when (selector-types (get j "type"))
            (mapcat nested (conf-placed-refs reg nm))))))

(defn- biome-features [reg tagged j]
  (let [grown #(conf-features reg (placed-feature reg (plain %)))
        xf (comp cat (mapcat grown) (filter tagged))]
    (into [] xf (get j "features"))))

(defn- bone-meal-biomes [reg tagged]
  (by-key (keep (fn [[nm j]]
                  (let [fs (biome-features reg tagged j)]
                    (when (seq fs) [(kw nm) (mapv kw fs)])))
                (:biomes reg))))

(defn- closure [reg cs ps]
  (let [rs (mapcat #(conf-placed-refs reg %) cs)
        cs' (into cs (map #(placed-feature reg %)) (concat ps rs))
        ps' (into ps (filter string?) rs)]
    (if (and (= cs cs') (= ps ps')) [cs' ps'] (recur reg cs' ps'))))

(defn- feature-set [reg kind names]
  (by-key (map (fn [n] [(kw n) (feature-value (get (kind reg) n))])
               names)))

(defn- bone-meal-tagged []
  (let [r "worldgen/configured_feature"]
    (into #{} (map data/snake)
          (data/tag-values r "can_spawn_from_bone_meal"))))

(defn- placers []
  (by-key (keep (fn [[b m]] (when-let [f (:feature m)] [b f]))
                (data/blocks))))

(defn- grown-trees []
  (into #{} (comp (mapcat #(vals (dissoc % :secondary-chance)))
                  (map data/snake))
        (vals (data/growers))))

(defn- feature-registries []
  {:placed (by-name "worldgen/placed_feature")
   :configured (by-name "worldgen/configured_feature")
   :biomes (by-name "worldgen/biome")})

(defn- feature-table []
  (let [reg (feature-registries)
        tagged (bone-meal-tagged)
        placers (placers)
        roots (-> tagged
                  (into (map (comp data/snake val)) placers)
                  (into (grown-trees)))
        [cs ps] (closure reg roots #{"grass_bonemeal"})]
    {:configured (feature-set reg :configured cs)
     :placed (feature-set reg :placed ps)
     :bone-meal (bone-meal-biomes reg tagged)
     :placers placers}))

(def ^:private ^:table feature-tables (delay (feature-table)))

(defn features
  "Returns the features that bone meal reaches, the features that
  each biome grows from it and the feature of each placer block."
  []
  @feature-tables)

(defn bone-meal-features
  "Returns the features that bone meal grows in biome."
  [biome]
  (get-in (features) [:bone-meal biome]))

(defn configured-feature
  "Returns the configured feature named k that bone meal reaches."
  [k]
  (get-in (features) [:configured k]))

(defn placer-feature
  "Returns the feature that the placer block grows, or nil."
  [block]
  (get-in (features) [:placers block]))
