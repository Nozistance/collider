(ns collider.tables.value
  "Turning vanilla names and numbers into table values."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn kw
  "Returns resource location s as a keyword without the minecraft
  namespace, dashes for underscores."
  [s]
  (-> (str s)
      (str/replace #"^minecraft:" "")
      (str/replace "_" "-")
      keyword))

(defn flt
  "Returns v as the double that prints like the float v."
  ^double [v]
  (Double/parseDouble (Float/toString (float v))))

(defn sorted-vals
  "Returns m sorted by key, with f applied to every value."
  [m f]
  (into (sorted-map) (map (fn [[k v]] [k (f v)])) m))

(defn sorted-json
  "Returns json v with every object sorted by key."
  [v]
  (cond
    (map? v) (sorted-vals v sorted-json)
    (vector? v) (mapv sorted-json v)
    :else v))

(defn unknown
  "Returns the error for vanilla data the tables cannot hold."
  [msg data]
  (let [why (str "The tables have no place for " msg ": "
                 (pr-str data))]
    (ex-info msg (assoc data :what "unknown vanilla data" :why why))))
