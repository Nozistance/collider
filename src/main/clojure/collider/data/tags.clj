(ns collider.data.tags
  "Tags built from the tag files of a pack, as the vanilla loader
  builds them."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- entry
  "Returns entry e of a tag file as id, tag flag and requirement."
  [e]
  (let [[s required] (if (map? e)
                       [(get e "id") (get e "required" true)]
                       [e true])
        tag? (str/starts-with? s "#")]
    [(if tag? (subs s 1) s) tag? required]))

(defn- refs [file]
  (into [] (keep (fn [e] (let [[id tag?] (entry e)] (when tag? id))))
        (get file "values")))

(declare visit)

(defn- visit-refs [files acc id path]
  (let [path (conj path id)
        [o d] (reduce #(visit files %1 %2 path) acc
                      (refs (get files id)))]
    [(conj o id) (conj d id)]))

(defn- visit [files [_ done :as acc] id path]
  (cond
    (or (done id) (not (contains? files id))) acc
    (path id) (throw (ex-info "tag cycle" {:tag id :path path}))
    :else (visit-refs files acc id path)))

(defn- build-order
  "Returns the ids of files, each after the tags it refers to."
  [files]
  (first (reduce #(visit files %1 %2 #{}) [[] #{}] (keys files))))

(defn- part [exists? built e]
  (let [[id tag? required] (entry e)
        vs (if tag? (get built id) (when (exists? id) [id]))]
    (cond vs vs required nil :else [])))

(defn- build-tag [exists? built file]
  (let [parts (mapv #(part exists? built %) (get file "values"))]
    (when (every? some? parts)
      (into [] (comp cat (distinct)) parts))))

(defn build
  "Returns the tags that files build, by id, each a vector of element
  ids without repeats in the order the entries give them. Files map
  a tag id to its tag file; exists? tells the element ids of the
  registry. A tag with a required entry that is missing is left out,
  and so is every tag that requires it."
  [files exists?]
  (reduce (fn [built id]
            (if-let [vs (build-tag exists? built (get files id))]
              (assoc built id vs)
              built))
          {} (build-order files)))
