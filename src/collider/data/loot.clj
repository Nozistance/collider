(ns collider.data.loot
  "The drops of blocks and mobs read from the loot tables of a pack."
  (:require [clojure.string :as str]
            [collider.data.pack :as pack]))

(set! *warn-on-reflection* true)

(defn- loot-number [v]
  (cond
    (number? v) [(long v) (long v)]
    (map? v) [(long (get v "min" 1)) (long (get v "max" 1))]
    :else [1 1]))

(declare loot-condition)

(defn- state-props [c]
  {:props (into {}
                (map (fn [[k v]] [(pack/kw k) (keyword v)]))
                (get c "properties"))})

(defn- nested-condition [c]
  (case (get c "condition")
    "minecraft:inverted"
    (if (= :skip (loot-condition (get c "term"))) {} :unknown)
    "minecraft:any_of"
    (if (every? #(= :skip (loot-condition %)) (get c "terms"))
      :skip
      :unknown)
    :unknown))

(defn- loot-condition [c]
  (case (get c "condition")
    "minecraft:survives_explosion" {:survives-explosion true}
    "minecraft:random_chance" {:chance (double (get c "chance"))}
    "minecraft:table_bonus"
    {:chance (double (first (get c "chances")))}
    "minecraft:block_state_property" (state-props c)
    "minecraft:entity_properties" {:entity? true}
    "minecraft:match_tool" :skip
    (nested-condition c)))

(defn- loot-conditions [cs]
  (reduce (fn [acc c]
            (let [r (loot-condition c)]
              (if (keyword? r) (reduced r) (merge acc r))))
          {} cs))

(defn- collect [f xs]
  (reduce (fn [acc x]
            (let [r (f x)]
              (cond (= r :skip) acc
                    (keyword? r) (reduced r)
                    :else (into acc r))))
          [] xs))

(defn- set-count [e]
  (some #(when (= "minecraft:set_count" (get % "function"))
           (loot-number (get % "count")))
        (get e "functions")))

(defn- item-entry [e cs]
  (let [n (set-count e)]
    [(cond-> (assoc cs :item (pack/kw (get e "name")))
       n (assoc :count n))]))

(defn- loot-entry [e]
  (let [type (get e "type")
        cs (loot-conditions (get e "conditions"))]
    (cond
      (not (#{"minecraft:item" "minecraft:alternatives"} type))
      :unknown
      (keyword? cs) cs
      (= "minecraft:item" type) (item-entry e cs)
      :else
      (let [r (collect loot-entry (get e "children"))]
        (if (keyword? r) r (mapv #(merge cs %) r))))))

(defn- loot-pool [p]
  (let [cs (loot-conditions (get p "conditions"))
        rolls (loot-number (get p "rolls" 1))
        r (when-not (keyword? cs)
            (collect loot-entry (get p "entries")))]
    (cond
      (keyword? cs) cs
      (keyword? r) r
      :else (mapv #(cond-> (merge cs %)
                     (not= rolls [1 1]) (assoc :rolls rolls))
                  r))))

(defn- block-table [json]
  (let [rs (map loot-pool (get json "pools"))]
    (if (some #{:unknown} rs)
      :complex
      (into [] (mapcat #(if (= % :skip) [] %)) rs))))

(defn- under
  "Returns the name and the table of every table of tables whose id
  starts with dir, by name."
  [tables dir]
  (let [prefix (str "minecraft:" dir "/")]
    (into (sorted-map)
          (keep (fn [[id json]]
                  (when (str/starts-with? id prefix)
                    [(subs id (count prefix)) json])))
          tables)))

(defn block-drops
  "Returns what each block drops, by block, from the loot tables of a
  pack by id: the items with their counts and conditions, or :complex
  for a table these words cannot tell. Blocks that drop nothing are
  left out."
  [tables]
  (pack/plain
   (into (sorted-map)
         (keep (fn [[name json]]
                 (let [t (block-table json)]
                   (when (not= t []) [(pack/kw name) t]))))
         (under tables "blocks"))))

(defn- loot-id [s]
  (pack/kw (str/replace (str s) #"^#" "")))

(declare shear-name)

(defn- table-ref [v]
  (let [s (str/replace (str v) #"^minecraft:" "")]
    (cond
      (str/starts-with? s "entities/") (pack/kw (subs s 9))
      (str/starts-with? s "shearing/") (pack/kw (shear-name (subs s 9)))
      :else s)))

(defn- loot-scalar [v]
  (cond
    (string? v) (loot-id v)
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

(defn- shear-name [n]
  (if-let [i (str/index-of n "/")]
    (str (subs n 0 i) "-shear" (subs n i))
    (str n "-shear")))

(defn entity-drops
  "Returns the loot tables of the mobs by name, from the loot tables
  of a pack by id. A shearing table has the name of the mob with the
  suffix -shear."
  [tables]
  (let [shear (under tables "shearing")]
    (pack/plain
     (into (sorted-map)
           (map (fn [[name json]] [(pack/kw name) (loot-node json)]))
           (concat (under tables "entities")
                   (map (fn [[n j]] [(shear-name n) j]) shear))))))
