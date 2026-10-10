(ns collider.game.mob.variant
  "Spawn variants by the spawn conditions of their registry."
  (:require [collider.data :as data]
            [collider.game.clock :as clock]
            [collider.random :as random]
            [collider.world.env.biome :as biome]))

(set! *warn-on-reflection* true)

(def ^:private registries
  ["cat_variant" "chicken_variant" "cow_variant" "frog_variant"
   "pig_variant" "wolf_variant" "zombie_nautilus_variant"])

(def ^:private moon-brightness
  {"full_moon" 1.0 "waning_gibbous" 0.75 "third_quarter" 0.5
   "waning_crescent" 0.25 "new_moon" 0.0 "waxing_crescent" 0.25
   "first_quarter" 0.5 "waxing_gibbous" 0.75})

(defn moon-phase
  "Returns the moon phase of level dim in world at biome."
  [world dim biome]
  (or (clock/held world dim "minecraft:visual/moon_phase")
      (biome/attribute biome :visual/moon-phase)
      "full_moon"))

(defn place
  "Returns what spawn conditions see at pos in level dim of world.
  No structure stands anywhere yet."
  [world dim pos]
  (let [b (biome/at dim pos)]
    {:biome b :moon-phase (moon-phase world dim b) :structures #{}}))

(defn- in-range? [r ^double x]
  (if (map? r)
    (<= (double (get r "min" x)) x (double (get r "max" x)))
    (== (double r) x)))

(defn- members [registry v] (set (data/holder-set registry v)))

(defn- check [c]
  (case (get c "type")
    "minecraft:biome"
    (let [s (members "worldgen/biome" (get c "biomes"))]
      #(contains? s (:name (:biome %))))
    "minecraft:moon_brightness"
    (let [r (get c "range")]
      #(in-range? r (moon-brightness (:moon-phase %))))
    "minecraft:structure"
    (let [s (members "worldgen/structure" (get c "structures"))]
      #(boolean (some s (:structures %))))))

(defn- selectors [registry]
  (let [entries (data/pack registry)]
    (vec (for [k (get (data/datapack) registry)
               s (get-in entries [(data/wire k) "spawn_conditions"])
               :let [c (get s "condition")]]
           [k (long (get s "priority"))
            (if c (check c) (constantly true))]))))

(def ^:private ^:table selector-table
  (delay (into {} (map (juxt identity selectors)) registries)))

(defn- chosen [xs place]
  (let [hits (filterv (fn [[_ _ ok?]] (ok? place)) xs)
        top (reduce max Long/MIN_VALUE (map second hits))]
    (into [] (keep (fn [[k p]] (when (== top (long p)) k))) hits)))

(defn pick
  "Returns the entry of registry that a mob spawned at place takes,
  or nil when no condition holds. The entries of the highest
  priority that holds have an even chance. The keys ks decide."
  [registry place ks]
  (let [xs (chosen (get @selector-table registry) place)
        r (random/of-key (conj ks :variant))]
    (when (seq xs) (nth xs (random/below r (count xs))))))

(def dye-colors
  "The dye colours by their id, as the loot tables name them."
  [:white :orange :magenta :light-blue :yellow :lime :pink :gray
   :light-gray :cyan :purple :blue :brown :green :red :black])

(def ^:private color-ids
  (into {} (map-indexed (fn [i c] [c i])) dye-colors))

(defn color-id
  "Returns the id of dye colour c, or nil when c is no dye colour."
  [c] (color-ids c))

(def ^:private spawn-configs
  {:temperate
   {:rare [[5 :black] [5 :gray] [5 :light-gray] [3 :brown]]
    :common :white}
   :warm
   {:rare [[5 :gray] [5 :light-gray] [5 :white] [3 :black]]
    :common :brown}
   :cold
   {:rare [[5 :light-gray] [5 :gray] [5 :white] [3 :brown]]
    :common :black}})

(defn- biome-tag [tag]
  (delay (set (data/tag-values "worldgen/biome" tag))))

(def ^:private ^:table warm-biomes
  (biome-tag "spawns_warm_variant_farm_animals"))

(def ^:private ^:table cold-biomes
  (biome-tag "spawns_cold_variant_farm_animals"))

(defn- biome-kind [biome]
  (let [n (:name biome)]
    (cond (@warm-biomes n) :warm
          (@cold-biomes n) :cold
          :else :temperate)))

(defn- spawn-config [biome] (spawn-configs (biome-kind biome)))

(def ^:private ^:const rare-total 100.0)

(def ^:private ^:const common-total 500.0)

(def ^:private ^:const common-weight 499)

(defn- weighted [^long r entries]
  (loop [lo 0 [[w c] & more] entries]
    (when w
      (if (< r (+ lo (long w))) c (recur (+ lo (long w)) more)))))

(defn- common-color [ks common]
  (if (< (long (* common-total (random/of-key (conj ks :pink))))
         common-weight)
    common
    :pink))

(defn sheep-color
  "Returns the wool colour a sheep takes at place by the keys ks."
  [ks {:keys [biome]}]
  (let [{:keys [rare common]} (spawn-config biome)
        r (long (* rare-total (random/of-key ks)))]
    (color-id (or (weighted r rare) (common-color ks common)))))

(def ^:private ^:table white-rabbit-biomes
  (biome-tag "spawns_white_rabbits"))

(def ^:private ^:table gold-rabbit-biomes
  (biome-tag "spawns_gold_rabbits"))

(def rabbit-variants
  {0 :brown 1 :white 2 :black 3 :white-splotched 4 :gold 5 :salt
   99 :evil})

(defn- mixed-rabbit ^long [^long r]
  (cond (< r 50) 0 (< r 90) 5 :else 2))

(defn rabbit-variant
  "Returns the variant a rabbit takes at place by the keys ks."
  [ks {:keys [biome]}]
  (let [n (:name biome)
        r (long (* 100.0 (random/of-key (conj ks :rabbit))))]
    (cond (@white-rabbit-biomes n) (if (< r 80) 1 3)
          (@gold-rabbit-biomes n) 4
          :else (mixed-rabbit r))))
