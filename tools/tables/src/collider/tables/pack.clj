(ns collider.tables.pack
  "The registries of the vanilla pack as their own codecs write them."
  (:require [clojure.data.json :as json]
            [collider.tables.reflect
             :refer [any-class call call-static class-field cls
                     static-field]]
            [collider.tables.value :refer [sorted-json unknown]])
  (:import (clojure.lang Reflector)
           (java.io Reader)
           (java.util List Optional)))

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

(defn registries
  "Returns every registry of the vanilla pack by path, each entry as
  json by its identifier."
  []
  (let [vanilla "data.registries.VanillaRegistries"
        provider (call-static vanilla "createLookup")
        json (class-field (any-class json-ops) "INSTANCE")
        ops (call provider "createSerializationContext" json)]
    (into (sorted-map)
          (keep #(when-let [t (registry-table provider ops %)]
                   [(registry-path %) t]))
          (static-field "resources.RegistryDataLoader"
                        "WORLDGEN_REGISTRIES"))))

(defn- registry-keys []
  (let [loader "resources.RegistryDataLoader"
        built-in "core.registries.BuiltInRegistries"]
    (map #(call % "key")
         (concat (static-field built-in "REGISTRY")
                 (static-field loader "WORLDGEN_REGISTRIES")
                 (static-field loader "DIMENSION_REGISTRIES")))))

(defn- vanilla-resources []
  (let [source "server.packs.repository.ServerPacksSource"
        pack (call-static source "createVanillaPackSource")
        data (static-field "server.packs.PackType" "SERVER_DATA")
        c (cls "server.packs.resources.MultiPackResourceManager")
        args (object-array [data (List/of pack)])]
    (Reflector/invokeConstructor c args)))

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
  them, by id, for every registry that has any, by path."
  []
  (let [rm (vanilla-resources)
        ops (class-field (any-class json-ops) "INSTANCE")]
    (into (sorted-map)
          (keep (fn [k]
                  (let [t (registry-tags rm ops k)]
                    (when (seq t)
                      [(call (call k "identifier") "getPath") t]))))
          (registry-keys))))
