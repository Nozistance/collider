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
