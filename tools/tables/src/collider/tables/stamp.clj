(ns collider.tables.stamp
  "The game version and the layout of the tables this tool makes."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def game "26.2")

(def layout 17)

(def pack
  ["banner_pattern" "cat_sound_variant" "cat_variant" "chat_type"
   "chicken_sound_variant" "chicken_variant" "cow_sound_variant"
   "cow_variant" "damage_type" "dialog" "dimension_type"
   "enchantment" "enchantment_provider" "frog_variant"
   "instrument" "jukebox_song" "painting_variant"
   "pig_sound_variant" "pig_variant" "sulfur_cube_archetype"
   "test_environment" "test_instance" "timeline" "trade_set"
   "trial_spawner" "trim_material" "trim_pattern"
   "villager_trade" "wolf_sound_variant" "wolf_variant"
   "world_clock" "worldgen/biome" "worldgen/configured_carver"
   "worldgen/configured_feature" "worldgen/density_function"
   "worldgen/flat_level_generator_preset"
   "worldgen/multi_noise_biome_source_parameter_list"
   "worldgen/noise" "worldgen/noise_settings"
   "worldgen/placed_feature" "worldgen/processor_list"
   "worldgen/structure" "worldgen/structure_set"
   "worldgen/template_pool" "worldgen/world_preset"
   "zombie_nautilus_variant"])

(def reloadable
  ["item_modifier" "loot_table" "predicate" "recipe"])

(def tags
  ["banner_pattern" "block" "damage_type" "dialog" "enchantment"
   "entity_type" "fluid" "game_event" "instrument" "item"
   "painting_variant" "point_of_interest_type" "potion" "timeline"
   "villager_trade" "worldgen/biome" "worldgen/configured_feature"
   "worldgen/flat_level_generator_preset" "worldgen/structure"
   "worldgen/world_preset"])

(def files
  (-> ["packets" "registries" "blocks" "synced" "items"
       "light" "fire" "fuel" "brewing" "dyes" "sounds"
       "potions" "effects" "shapes" "outlines" "sturdy" "flags"]
      (into (map #(str "pack/" %)) pack)
      (into (map #(str "pack/" %)) reloadable)
      (into (map #(str "pack/tags/" %)) tags)))

(defn stamp
  "Returns the stamp that a set of tables of this tool carries."
  []
  {:game game :layout layout})

(defn stamp-of
  "Returns the stamp of the tables in d, or nil when it has none."
  [d]
  (try (edn/read-string (slurp (io/file d "stamp.edn")))
       (catch Exception _ nil)))

(defn missing
  "Returns the files of a full set that d does not hold."
  [d]
  (into []
        (comp (map #(str % ".edn"))
              (remove #(.isFile (io/file d %))))
        (conj files "stamp")))
