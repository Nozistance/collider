(ns collider.tables.pack
  "The registries of the vanilla pack as their own codecs write them."
  (:require [clojure.data.json :as json]
            [collider.tables.reflect
             :refer [any-class call call-static class-field
                     static-field]]
            [collider.tables.value :refer [sorted-json]])
  (:import (java.util Optional)))

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
