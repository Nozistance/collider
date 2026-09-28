(ns collider.tables.tags
  "Reading tags and the items they stand for."
  (:require [clojure.string :as str]
            [collider.tables.files :refer [jsons]]
            [collider.tables.value
             :refer [kw plain sorted-json sorted-vals unknown]]))

(set! *warn-on-reflection* true)

(defn ingredient [v]
  (cond
    (string? v) (if (str/starts-with? v "#")
                  {:tag (str/replace (subs v 1) #"^minecraft:" "")}
                  [(kw v)])
    (sequential? v) (mapv kw v)
    :else (throw (unknown "unknown ingredient" {:value v}))))

(defn item-set [tags v]
  (let [i (ingredient v)
        items (if (map? i)
                (or (get-in tags ["item" (:tag i)])
                    (throw (unknown "unknown item tag" {:tag i})))
                i)]
    (into (sorted-set) items)))

(defn- tag-values [json]
  (mapv #(if (map? %) (get % "id") %) (get json "values")))

(defn- resolve-tags [found vs seen]
  (let [tag (fn [t]
              (if (seen t)
                []
                (resolve-tags found (found t []) (conj seen t))))
        one (fn [v]
              (if (str/starts-with? v "#")
                (tag (plain (subs v 1)))
                [(kw v)]))]
    (into [] (mapcat one) vs)))

(defn tags-of [zf registries]
  (into (sorted-map)
        (keep (fn [reg]
                (let [prefix (str "data/minecraft/tags/" reg "/")
                      found (sorted-vals (jsons zf prefix) tag-values)
                      resolved #(resolve-tags found % #{})]
                  (when (seq found)
                    [reg (sorted-vals found resolved)]))))
        registries))

(defn- pack-tag [[name json]]
  [(str "minecraft:" name) (sorted-json json)])

(defn pack-tags
  "Returns the tags of registry path of the vanilla pack as the pack
  holds them, by id."
  [zf path]
  (let [prefix (str "data/minecraft/tags/" path "/")]
    (into (sorted-map) (map pack-tag) (jsons zf prefix))))
