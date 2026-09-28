(ns collider.tables.stamp
  "The game version and the layout of the tables this tool makes."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def game "26.2")

(def layout 13)

(def files
  ["packets" "registries" "blocks" "datapack" "tags" "items"
   "light" "fire" "drops" "entity-drops" "recipes" "sounds"
   "features" "potions" "effects" "enchantments"
   "dimension-types" "biomes" "shapes"
   "outlines" "sturdy" "flags"])

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
