(ns collider.data.loot
  "The drops of blocks and mobs read from the loot tables of a pack."
  (:require [clojure.string :as str]
            [collider.data.pack :as pack]))

(set! *warn-on-reflection* true)

(defn- under
  [tables dir]
  (let [prefix (str "minecraft:" dir "/")]
    (into (sorted-map)
          (keep (fn [[id json]]
                  (when (str/starts-with? id prefix)
                    [(subs id (count prefix)) json])))
          tables)))

(defn- shear-name [n]
  (if-let [i (str/index-of n "/")]
    (str (subs n 0 i) "-shear" (subs n i))
    (str n "-shear")))

(defn- table-ref [v]
  (let [s (str/replace (str v) #"^minecraft:" "")]
    (cond
      (str/starts-with? s "entities/") (pack/kw (subs s 9))
      (str/starts-with? s "shearing/")
      (pack/kw (shear-name (subs s 9)))
      :else s)))

(defn- loot-scalar [v]
  (cond
    (and (string? v) (str/starts-with? v "#"))
    {:tag (str/replace (subs v 1) #"^minecraft:" "")}
    (string? v) (pack/kw v)
    (not (number? v)) v
    (== (double v) (Math/rint (double v))) (long v)
    :else v))

(declare loot-node)

(defn- loot-provider [m]
  (case (:type m)
    :uniform (select-keys m [:min :max])
    :binomial (select-keys m [:n :p])
    :constant (:value m)
    m))

(defn- loot-map [m]
  (let [r (loot-provider
           (into (sorted-map)
                 (map (fn [[k v]] [(pack/kw k) (loot-node v)]))
                 m))]
    (if (= :loot-table (:type r))
      (assoc r :value (table-ref (get m "value")))
      r)))

(defn- loot-node [v]
  (cond
    (map? v) (loot-map v)
    (sequential? v) (mapv loot-node v)
    :else (loot-scalar v)))

(defn- gift? [[_ json]] (= "minecraft:gift" (get json "type")))

(defn block-drops
  "Returns the loot tables of the blocks by name, from the loot
  tables of a pack by id."
  [tables]
  (pack/plain
   (into (sorted-map)
         (map (fn [[name json]] [(pack/kw name) (loot-node json)]))
         (under tables "blocks"))))

(defn entity-drops
  "Returns the loot tables of the mobs by name, from the loot tables
  of a pack by id. A shearing table has the name of the mob with the
  suffix -shear, a brushing table the suffix -brush, and a gift table
  its own name."
  [tables]
  (let [shear (under tables "shearing")
        brush (under tables "brush")]
    (pack/plain
     (into (sorted-map)
           (map (fn [[name json]] [(pack/kw name) (loot-node json)]))
           (concat (under tables "entities")
                   (map (fn [[n j]] [(shear-name n) j]) shear)
                   (map (fn [[n j]] [(str n "-brush") j]) brush)
                   (filter gift? (under tables "gameplay")))))))
