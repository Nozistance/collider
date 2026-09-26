(ns collider.data
  "The game data tables."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader)
           (java.util Arrays HashMap List)))

(set! *warn-on-reflection* true)

(def game "26.2")

(def layout 7)

(def ^:private files
  ["packets" "registries" "blocks" "datapack" "tags" "items"
   "light" "fire" "drops" "entity-drops" "recipes" "sounds"
   "features" "potions" "effects" "enchantments"
   "dimension-types" "biomes" "shapes"
   "outlines" "sturdy" "sturdy-center" "sturdy-rigid" "flags"])

(defn stamp
  "Returns the mark a set of tables carries: the game version and
  the layout they were made for."
  []
  {:game game :layout layout})

(defn- stamped? [d]
  (= (stamp) (try (edn/read-string (slurp (io/file d "stamp.edn")))
                  (catch Exception _ nil))))

(defn complete?
  "Returns true when d holds a full, stamped set of tables."
  [d]
  (and (every? #(.isFile (io/file d (str % ".edn"))) files)
       (stamped? d)))

(defn dir
  "Returns the first place that holds a full set of tables of this
  version, or nil when none does."
  []
  (->> [(System/getProperty "collider.data") "target/data" "data"]
       (remove nil?)
       (filter complete?)
       first))

(defn- tables-of [ns]
  (for [[_ v] (ns-interns ns) :when (:table (meta v))] @v))

(defn load!
  "Reads every table the loaded namespaces declare."
  []
  (doseq [ns (all-ns)
          :when (.startsWith (str (ns-name ns)) "collider.")
          t (tables-of ns)]
    @t))

(defn- no-tables []
  (let [why (str "No complete set of tables for " game
                 " in target/data or data; the release jar"
                 " generates it on its first start")
        how (str "From the source tree:"
                 " clojure -T:build data")]
    (ex-info "no tables"
             {:what    "no game data"
              :why     why
              :command how})))

(defn- read-edn [name]
  (let [d (or (dir) (throw (no-tables)))]
    (with-open [r (io/reader (io/file d name))]
      (edn/read (PushbackReader. r)))))

(def ^:private table-names
  [:packets :registries :blocks :datapack :tags :items :light
   :fire :drops :entity-drops :recipes :sounds :features
   :potions :effects :enchantments :dimension-types :biomes])

(def ^:private ^:table tables
  (delay (into {}
               (map (fn [k]
                      [k (read-edn (str (name k) ".edn"))]))
               table-names)))

(defn packets
  "Returns the packet ids by connection state, direction and name."
  [] (:packets @tables))

(defn registries
  "Returns the ids of the entries the client knows, by registry."
  [] (:registries @tables))

(defn blocks [] (:blocks @tables))

(defn datapack
  "Returns the entries the server sends to the client, by registry."
  [] (:datapack @tables))

(defn tags [] (:tags @tables))

(defn items [] (:items @tables))

(defn light
  "Returns how block states pass and emit light."
  [] (:light @tables))

(defn fire
  "Returns how blocks catch fire and burn."
  [] (:fire @tables))

(defn drops
  "Returns what blocks drop when broken."
  [] (:drops @tables))

(defn entity-drops
  "Returns the loot tables of the mobs, in their vanilla shape.
  A shearing table is named after the mob with -shear, so that
  shearing/sheep/black is :sheep-shear/black."
  [] (:entity-drops @tables))

(defn recipes
  "Returns the stonecutting recipes and their ingredients.
  The client is told of the ingredients."
  [] (:recipes @tables))

(defn cooking-recipes
  "Returns the cooking recipes in furnace search order.
  They are the smelting, blasting, smoking and campfire recipes."
  [] (:cooking (recipes)))

(defn fuel
  "Returns how many ticks each item burns for in a furnace."
  [] (:fuel (recipes)))

(defn brewing
  "Returns the containers, mixes and fuel of a brewing stand."
  [] (:brewing (recipes)))

(defn enchantments
  "Returns the cost, reach and rivals of every enchantment."
  [] (:enchantments @tables))

(defn enchantment [name] (get (enchantments) name))

(defn dimension-types
  "Returns the fields of every dimension type, by name."
  [] (:dimension-types @tables))

(defn dimension-type
  "Returns the fields of a dimension type."
  [dim] (get (dimension-types) dim))

(defn biomes
  "Returns the climate and attributes of every biome, by name."
  [] (:biomes @tables))

(defn smithing-recipes
  "Returns the transform and trim recipes of a smithing table."
  [] (:smithing (recipes)))

(defn sounds [] (:sounds @tables))

(defn features
  "Returns the features bone meal reaches.
  Also returns the features each biome grows."
  [] (:features @tables))

(defn potions
  "Returns the effect instances every potion gives."
  [] (:potions @tables))

(defn mob-effects
  "Returns the colour, category and immediacy of every effect."
  [] (:effects @tables))

(defn use-cooldown
  "Returns the cooldown group and the ticks item locks it for.
  Returns nil when the item has no cooldown."
  [item]
  (when-let [c (get-in (items) [item :use-cooldown])]
    [(get c :group item) (long (* 20.0 (double (:seconds c))))]))

(defn max-stack ^long [item]
  (long (get-in (items) [item :max-stack] 64)))

(defn jukebox-song [item]
  (get-in (items) [item :jukebox-song]))

(defn equip-slot [item]
  (get-in (items) [item :equip]))

(defn dye-color [item]
  (get-in (items) [item :dye]))

(defn pattern-tag [item]
  (get-in (items) [item :patterns]))

(defn compost
  "Returns the chance item raises a composter, else nil."
  [item]
  (get-in (items) [item :compost]))

(defn item-name
  "Returns the name an item shows when it has no custom one."
  [item]
  (get-in (items) [item :name]))

(defn item-title
  "Returns the text component an item is called by."
  [item]
  (get-in (items) [item :title]))

(defn rarity
  "Returns how rare an item is: :common, :uncommon, :rare or :epic."
  [item]
  (get-in (items) [item :rarity] :common))

(defn repairable
  "Returns the items that mend item on an anvil, else nil."
  [item]
  (get-in (items) [item :repairable]))

(defn trim-material
  "Returns the trim material item gives, else nil."
  [item]
  (get-in (items) [item :trim-material]))

(defn resists
  "Returns the tag of the damage item shrugs off, else nil."
  [item]
  (get-in (items) [item :resists]))

(defn tag-values [registry tag]
  (get-in (tags) [registry tag] []))

(defn snake
  "Returns the name of k with dashes as underscores."
  ^String [k]
  (.replace (name k) \- \_))

(defn wire
  "Returns k as a resource location, minecraft by default."
  ^String [k]
  (str (or (namespace k) "minecraft") ":" (snake k)))

(defn kebab
  "Returns resource location s as a keyword: the namespace kept
  unless it is minecraft, underscores as dashes."
  [^String s]
  (let [s (.toLowerCase s)
        i (.indexOf s ":")
        ns (if (neg? i) "minecraft" (subs s 0 i))
        nm (.replace (if (neg? i) s (subs s (inc i))) \_ \-)]
    (if (= ns "minecraft") (keyword nm) (keyword ns nm))))

(defn packet-id ^long [state dir name]
  (or (get-in (packets) [state dir name])
      (throw (ex-info "unknown packet"
                      {:state state :dir dir :name name}))))

(defn registry-id ^long [registry entry]
  (or (get-in (registries) [registry entry])
      (throw (ex-info "unknown registry entry"
                      {:registry registry :entry entry}))))

(defn- index-entries [entries]
  (into {} (map-indexed (fn [i e] [e (long i)])) entries))

(def ^:private ^:table datapack-index
  (delay
    (into {}
          (map (fn [[registry entries]]
                 [registry (index-entries entries)]))
          (datapack))))

(defn datapack-id
  "Returns the id of an entry the server sends to the client.
  The client does not know the entry already."
  ^long [registry entry]
  (or (get (get @datapack-index registry) entry)
      (throw (ex-info "unknown datapack entry"
                      {:registry registry :entry entry}))))

(defn entry-id
  "Returns the network id of entry in registry, built in or from
  the datapack."
  ^long [registry entry]
  (if (contains? (registries) registry)
    (registry-id registry entry)
    (datapack-id registry entry)))

(defn- invert-ids [entries]
  (into {} (map (fn [[k v]] [(long v) k])) entries))

(def ^:private ^:table by-id
  (delay
    (into {}
          (map (fn [[registry entries]]
                 [registry (invert-ids entries)]))
          (registries))))

(defn- unknown-id [registry ^long id]
  (ex-info "unknown registry id" {:registry registry :id id}))

(defn- unknown-registry [registry]
  (ex-info "unknown registry" {:registry registry}))

(defn entry-name
  "Returns the entry of registry with network id id."
  [registry ^long id]
  (let [m (get @by-id registry)
        v (get (datapack) registry)]
    (cond
      m (or (get m id) (throw (unknown-id registry id)))
      (nil? v) (throw (unknown-registry registry))
      (< -1 id (count v)) (nth v id)
      :else (throw (unknown-id registry id)))))

(defn- prop-order [b] (vec (keys (:props b))))

(defn- prop-sizes [b]
  (mapv #(count (get (:props b) %)) (prop-order b)))

(defn- state-count ^long [b]
  (reduce * 1 (map count (vals (:props b)))))

(defn- tail-size ^long [sizes ^long i]
  (long (reduce * 1 (subvec sizes (inc i)))))

(defn- decode-props [b ^long offset]
  (let [order (prop-order b)
        sizes (prop-sizes b)]
    (loop [i 0, left offset, acc {}]
      (if (= i (count order))
        acc
        (let [prop (nth order i)
              tail (tail-size sizes i)
              v (nth (get (:props b) prop) (quot left tail))]
          (recur (inc i) (rem left tail)
                 (assoc acc prop v)))))))

(defn- state-last ^long [b]
  (+ (long (:first b)) (state-count b)))

(def ^:private ^:table state-total
  (delay
    (long (reduce (fn [n [_ b]] (max n (state-last b)))
                  0 (blocks)))))

(defn block-state-count ^long []
  @state-total)

(def ^:private ^:table state-blocks
  (delay
    (let [a (object-array (block-state-count))]
      (doseq [[block b] (blocks)
              :let [from (long (:first b))]
              i (range (state-count b))]
        (aset a (+ from (long i)) block))
      a)))

(defn block-of-state ^objects []
  @state-blocks)

(defn- interned [^HashMap seen v]
  (if (vector? v)
    (let [v (mapv (fn [x] (interned seen x)) v)]
      (or (.get seen v) (do (.put seen v v) v)))
    v))

(defn- object-table [name]
  (let [a (object-array (block-state-count))
        seen (HashMap.)]
    (doseq [[k v] (read-edn name)
            :when (< -1 (long k) (block-state-count))]
      (aset a (int (long k)) (interned seen v)))
    a))

(defn- byte-table [name ^long default]
  (let [a (byte-array (block-state-count))]
    (Arrays/fill a (byte default))
    (doseq [[k v] (read-edn name)
            :when (< -1 (long k) (block-state-count))]
      (aset a (int (long k)) (byte (long v))))
    a))

(def ^:private ^:table shape-table
  (delay (object-table "shapes.edn")))

(def ^:private ^:table outline-table
  (delay (object-table "outlines.edn")))

(def ^:private ^:table sturdy-table
  (delay (byte-table "sturdy.edn" 63)))

(def ^:private ^:table sturdy-center-table
  (delay (byte-table "sturdy-center.edn" 63)))

(def ^:private ^:table sturdy-rigid-table
  (delay (byte-table "sturdy-rigid.edn" 63)))

(def ^:private ^:table flag-table
  (delay (byte-table "flags.edn" 0)))

(defn shapes
  "Returns the collision boxes of every state, by id.
  A state that is a full cube has nil instead."
  ^objects []
  @shape-table)

(defn outlines
  "Returns the outline boxes of every state, by id.
  A state that is a full cube has nil instead."
  ^objects []
  @outline-table)

(defn sturdy
  "Returns which faces of every state hold things, by id."
  ^bytes []
  @sturdy-table)

(defn sturdy-center
  "Returns which faces of every state hold things, by id.
  A thing is held at the center of the face."
  ^bytes []
  @sturdy-center-table)

(defn sturdy-rigid
  "Returns which faces of every state hold things rigidly, by id."
  ^bytes []
  @sturdy-rigid-table)

(defn flags ^bytes []
  @flag-table)

(defn- default-of [b]
  (decode-props b (- (long (:default b)) (long (:first b)))))

(def ^:private ^:table defaults
  (delay
    (into {}
          (map (fn [[block b]] [block (default-of b)]))
          (blocks))))

(defn default-props []
  @defaults)

(defn info
  "Returns the facts known about block. Throws for an unknown one."
  [block]
  (or (get (blocks) block)
      (throw (ex-info "unknown block" {:block block}))))

(defn place-sound [block]
  (get-in (sounds) [(:sound (info block)) :place]))

(defn open-sound [block open?]
  (get (info block) (if open? :open :close)))

(defn by-hand?
  "Returns true when block drops when broken without a tool."
  [block]
  (get (info block) :hand? true))

(defn- prop-index ^long [block-name prop vs v]
  (let [idx (.indexOf ^List vs v)]
    (when (neg? idx)
      (throw (ex-info "unknown property value"
                      {:block block-name :prop prop :value v})))
    idx))

(defn- state-offset ^long [block-name b wanted defaults]
  (let [order (prop-order b)
        sizes (prop-sizes b)]
    (loop [i 0 id (long (:first b))]
      (if (= i (count order))
        id
        (let [prop (nth order i)
              vs (get (:props b) prop)
              want (get wanted prop (get defaults prop))
              idx (prop-index block-name prop vs want)
              tail (tail-size sizes i)]
          (recur (inc i) (long (+ id (* idx tail)))))))))

(defn state-id
  "Returns the global state id of a block. Properties missing from
  wanted take their default values."
  (^long [block-name] (long (:default (info block-name))))
  (^long [block-name wanted]
   (let [b (info block-name)]
     (if (empty? wanted)
       (state-id block-name)
       (state-offset block-name b wanted
                     (get (default-props) block-name))))))

(defn state-block
  "Returns the block of block state id, or nil for no such state."
  [^long id]
  (when (< -1 id (block-state-count)) (aget (block-of-state) id)))

(defn state-props [^long id]
  (when-let [block-name (state-block id)]
    (let [b (get (blocks) block-name)]
      [block-name (decode-props b (- id (long (:first b))))])))
