(ns collider.data.tags
  "Tags built from the tag files of a pack."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- entry
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
        step #(visit files %1 %2 path)
        [order done] (reduce step acc (refs (get files id)))]
    [(conj order id) (conj done id)]))

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
  "Returns the tags that files build, by id.
  Each tag lists element ids once, in entry order. Exists? tells if an
  element id is in the registry. A tag with a missing required entry
  is left out, and so is every tag that requires it."
  [files exists?]
  (reduce (fn [built id]
            (if-let [vs (build-tag exists? built (get files id))]
              (assoc built id vs)
              built))
          {} (build-order files)))
