(ns collider.data.pack
  "Values read from the json of a pack, in the shape of the tables."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn kw
  "Returns id s as a keyword without the minecraft namespace, with
  dashes for underscores."
  [s]
  (-> (str s)
      (str/replace #"^minecraft:" "")
      (str/replace "_" "-")
      keyword))

(defn plain
  "Returns v with every sorted map and set made plain, in the same
  order, as reading it back from edn gives it."
  [v]
  (cond
    (map? v) (into {} (map (fn [[k x]] [k (plain x)])) v)
    (set? v) (into #{} (map plain) v)
    (vector? v) (mapv plain v)
    :else v))
