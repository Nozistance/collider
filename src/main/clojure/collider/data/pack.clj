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

(defn flt
  "Returns v as the double that prints like the float v."
  ^double [v]
  (Double/parseDouble (Float/toString (unchecked-float v))))

(defn unknown
  "Returns the error for pack data the tables have no words for."
  [msg data]
  (ex-info msg (assoc data :what "unknown vanilla data")))

(defn ingredient
  "Returns the items of holder set v, or the tag name under :tag."
  [v]
  (cond
    (string? v) (if (str/starts-with? v "#")
                  {:tag (str/replace (subs v 1) #"^minecraft:" "")}
                  [(kw v)])
    (sequential? v) (mapv kw v)
    :else (throw (unknown "unknown ingredient" {:value v}))))

(defn item-set
  "Returns the items of holder set v as a sorted set. Tags gives the
  items of an item tag by name, or nil for no such tag."
  [tags v]
  (let [i (ingredient v)
        items (if (map? i)
                (or (tags (:tag i))
                    (throw (unknown "unknown item tag" {:tag i})))
                i)]
    (into (sorted-set) items)))
