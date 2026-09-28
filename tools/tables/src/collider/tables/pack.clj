(ns collider.tables.pack
  "The registries, the tags and the reloadable data of the vanilla
  pack as their own codecs write them."
  (:require [clojure.data.json :as json]
            [collider.tables.reflect
             :refer [any-class call call-static class-field
                     registry static-field]]
            [collider.tables.value :refer [sorted-json unknown]])
  (:import (java.io Reader)
           (java.util Optional)))

(set! *warn-on-reflection* true)

(defn- encoded [ops codec v]
  (let [e (call (call codec "encodeStart" ops v) "getOrThrow")]
    (sorted-json (json/read-str (str e)))))

(defn- entries [lookup]
  (iterator-seq (call (call lookup "listElements") "iterator")))

(defn- lookup [provider rd]
  (Optional/.orElse (call provider "lookup" (call rd "key")) nil))

(defn- entry [ops codec e]
  [(str (call (call e "key") "identifier"))
   (encoded ops codec (call e "value"))])

(defn- registry-table [provider ops rd]
  (when-let [l (lookup provider rd)]
    (let [codec (call rd "elementCodec")]
      (into (sorted-map) (map #(entry ops codec %)) (entries l)))))

(def ^:private json-ops "com.mojang.serialization.JsonOps")

(defn- registry-path [rd]
  (-> (call rd "key") (call "identifier") (call "getPath")))

(defn- json-context [provider]
  (let [json (class-field (any-class json-ops) "INSTANCE")]
    (call provider "createSerializationContext" json)))

(defn registries
  "Returns every worldgen registry that access holds by path, each
  entry as json by its identifier."
  [access]
  (let [ops (json-context access)]
    (into (sorted-map)
          (keep #(when-let [t (registry-table access ops %)]
                   [(registry-path %) t]))
          (static-field "resources.RegistryDataLoader"
                        "WORLDGEN_REGISTRIES"))))

(defn- key-path [k] (call (call k "identifier") "getPath"))

(defn- loot-table [provider ops t]
  (let [l (call provider "lookupOrThrow" (call t "registryKey"))
        codec (call t "codec")]
    [(key-path (call t "registryKey"))
     (into (sorted-map) (map #(entry ops codec %)) (entries l))]))

(defn- recipe-table [provider ops managers]
  (let [codec (static-field "world.item.crafting.Recipe" "CODEC")
        rm (call managers "getRecipeManager")
        one (fn [h]
              [(str (call (call h "id") "identifier"))
               (encoded ops codec (call h "value"))])]
    ["recipe" (into (sorted-map) (map one) (call rm "getRecipes"))]))

(defn reloadable
  "Returns the loot tables, predicates, item modifiers and recipes the
  server loaded, by registry path, each entry as json by its id."
  [managers]
  (let [provider (call (call managers "fullRegistries") "lookup")
        ops (json-context provider)
        loot "world.level.storage.loot.LootDataType"
        types (-> (call-static loot "values") (call "iterator")
                  iterator-seq)]
    (into (sorted-map)
          (conj (mapv #(loot-table provider ops %) types)
                (recipe-table provider ops managers)))))

(defn- registry-keys []
  (let [loader "resources.RegistryDataLoader"
        built-in "core.registries.BuiltInRegistries"]
    (map #(call % "key")
         (concat (static-field built-in "REGISTRY")
                 (static-field loader "WORLDGEN_REGISTRIES")
                 (static-field loader "DIMENSION_REGISTRIES")))))

(defn- tag-file [tag-codec ops id stack]
  (when (not= 1 (count stack))
    (throw (unknown "a tag in many packs" {:tag (str id)})))
  (with-open [^Reader r (call (first stack) "openAsReader")]
    (let [json (call-static "util.StrictJsonParser" "parse" r)
          tag (call (call tag-codec "parse" ops json) "getOrThrow")]
      (encoded ops tag-codec tag))))

(defn- registry-tags [rm ops k]
  (let [dir (call-static "core.registries.Registries" "tagsDirPath" k)
        lister (call-static "resources.FileToIdConverter" "json" dir)
        codec (static-field "tags.TagFile" "CODEC")]
    (into (sorted-map)
          (map (fn [[file stack]]
                 (let [id (call lister "fileToId" file)]
                   [(str id) (tag-file codec ops id stack)])))
          (call lister "listMatchingResourceStacks" rm))))

(defn tags
  "Returns the tag files of the vanilla pack as TagFile.CODEC writes
  them, by id, for every registry that has any, by path. Reads them
  from the resource manager rm."
  [rm]
  (let [ops (class-field (any-class json-ops) "INSTANCE")]
    (into (sorted-map)
          (keep (fn [k]
                  (let [t (registry-tags rm ops k)]
                    (when (seq t)
                      [(call (call k "identifier") "getPath") t]))))
          (registry-keys))))

(defn- patch [components]
  (-> (call-static "core.component.DataComponentPatch" "builder")
      (call "set" components)
      (call "build")))

(defn components
  "Returns the default components of every item that has any, as
  DataComponentPatch.CODEC writes them, by id. The components come
  from the items as the loaded server bound them."
  [access]
  (let [ops (json-context access)
        patches "core.component.DataComponentPatch"
        codec (static-field patches "CODEC")
        one (fn [h]
              (let [cs (call h "components")]
                (when-not (call cs "isEmpty")
                  [(str (call (call h "key") "identifier"))
                   (encoded ops codec (patch cs))])))]
    (into (sorted-map) (keep one) (entries (registry "ITEM")))))
