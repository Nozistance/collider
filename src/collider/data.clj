(ns collider.data
  "Game data tables."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.data.items :as items]
            [collider.data.loot :as loot]
            [collider.data.recipes :as recipes]
            [collider.data.tags :as tags])
  (:import (clojure.lang PersistentArrayMap)
           (java.io File PushbackReader)
           (java.util Arrays List)
           (java.util.concurrent ExecutionException)))

(set! *warn-on-reflection* true)

(def game "26.2")

(def layout 19)

(defn- stamp-of [d]
  (try (edn/read-string (slurp (io/file d "stamp.edn")))
       (catch Exception _ nil)))

(defn complete?
  "Returns true when d holds a set of tables of this game and layout.
  The generator writes the stamp last, so a stamp means a full set."
  [d]
  (= {:game game :layout layout}
     (select-keys (stamp-of d) [:game :layout])))

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

(defn- all-tables []
  (for [ns (all-ns)
        :when (.startsWith (str (ns-name ns)) "collider.")
        t (tables-of ns)]
    t))

(defn- wait [f]
  (try @f
       (catch ExecutionException e (throw (ex-cause e)))))

(defn load!
  "Reads every table that the loaded namespaces declare.
  A table made from other tables waits for them."
  []
  (run! wait (mapv #(future @%) (all-tables))))

(defn no-tables
  "Returns the error for a missing or stale set of tables."
  []
  (let [why (str "No complete set of tables for " game
                 " in target/data or data")
        how (str "Make them with:"
                 " clojure -T:build tables")]
    (ex-info "no tables"
             {:what    "no game data"
              :why     why
              :command how})))

(def ^:private nbt-readers
  {'nbt/b byte 'nbt/s short 'nbt/i int 'nbt/l long 'nbt/f float
   'nbt/d double 'nbt/B byte-array 'nbt/I int-array
   'nbt/L long-array})

(defn- read-edn [name]
  (let [d (or (dir) (throw (no-tables)))]
    (with-open [r (io/reader (io/file d name))]
      (edn/read {:readers nbt-readers} (PushbackReader. r)))))

(defn pack
  "Returns the entries of registry path of the vanilla pack by id, as
  the codec of the registry writes them."
  [path]
  (read-edn (str "pack/" path ".edn")))

(def ^:private item-facts
  [:compost :wall-blocks :place-sounds :remainders :banner-colors
   :non-breakers :item-names :mob-buckets])

(def ^:private table-names
  (into [:packets :registries :blocks :synced :light :fire :fuel
         :brewing :dyes :sounds :potions :effects :entities
         :version :block-entities :growers]
        item-facts))

(def ^:private ^:table tables
  (delay (into {}
               (map (fn [k]
                      [k (read-edn (str (name k) ".edn"))]))
               table-names)))

(defn packets
  "Returns the packet ids by connection state, direction and name."
  [] (:packets @tables))

(defn version
  "Returns what the game tells of its version: :id, :name, :data,
  :series, :protocol, :build-time in epoch millis, :resource-pack,
  :data-pack and :stable."
  [] (:version @tables))

(defn registries
  "Returns the ids of the entries the client knows, by registry."
  [] (:registries @tables))

(defn blocks [] (:blocks @tables))

(defn growers
  "Returns the tree growers by name: the chance of the secondary
  trees and the features of each tree."
  [] (:growers @tables))

(defn light
  "Returns how block states pass and emit light."
  [] (:light @tables))

(defn fire
  "Returns how blocks catch fire and burn."
  [] (:fire @tables))

(def ^:private ^:table loot-tables
  (delay (let [t (pack "loot_table")]
           {:drops (loot/block-drops t)
            :entity-drops (loot/entity-drops t)})))

(defn drops
  "Returns what blocks drop when broken."
  [] (:drops @loot-tables))

(defn entity-drops
  "Returns the loot tables of the mobs.
  A shearing table has the name of the mob with the suffix -shear."
  [] (:entity-drops @loot-tables))

(defn sounds [] (:sounds @tables))

(defn entities
  "Returns the width, height and eye height of each entity type, and
  of its baby as :baby."
  [] (:entities @tables))

(defn block-entities
  "Returns the tags a fresh block entity of each type saves and sends,
  and whether it sends a block entity data packet."
  [] (:block-entities @tables))

(defn potions
  "Returns the effect instances every potion gives."
  [] (:potions @tables))

(defn mob-effects
  "Returns the colour, category and immediacy of every effect."
  [] (:effects @tables))

(defn snake
  "Returns the name of k with dashes as underscores."
  ^String [k]
  (.replace (name k) \- \_))

(defn wire
  "Returns k as a resource location with the default namespace."
  ^String [k]
  (str (or (namespace k) "minecraft") ":" (snake k)))

(defn kebab
  "Returns resource location s as a keyword.
  The default namespace goes away and underscores become dashes."
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

(defn- file-order [id]
  (let [i (str/index-of id ":")]
    [(str (subs id (inc i)) ".json") (subs id 0 i)]))

(defn- entry-names [registry]
  (into [] (map kebab) (sort-by file-order (keys (pack registry)))))

(def ^:private ^:table datapack-map
  (delay (PersistentArrayMap.
          (object-array
           (into [] (mapcat (fn [r] [r (entry-names r)]))
                 (:synced @tables))))))

(defn datapack
  "Returns the entries the server sends to the client, by registry,
  in the order the server sends them."
  [] @datapack-map)

(defn- index-entries [entries]
  (into {} (map-indexed (fn [i e] [e (long i)])) entries))

(def ^:private ^:table datapack-index
  (delay
    (into {}
          (map (fn [[registry entries]]
                 [registry (index-entries entries)]))
          (datapack))))

(defn datapack-id
  "Returns the id of an entry that the server sends to the client."
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

(defn- tag-paths
  "Returns the registries the pack has tag files for, by path."
  []
  (let [root (io/file (or (dir) (throw (no-tables))) "pack" "tags")
        skip (inc (count (str root)))]
    (into (sorted-set)
          (comp (map #(str/replace (str %) File/separatorChar \/))
                (filter #(str/ends-with? % ".edn"))
                (map #(subs % skip (- (count %) 4))))
          (file-seq root))))

(defn- element-exists [path]
  (if-let [ids (get (registries) path)]
    #(contains? ids (kebab %))
    (let [ids (set (keys (pack path)))] #(contains? ids %))))

(defn- tag-name [^String id]
  (if (str/starts-with? id "minecraft:") (subs id 10) id))

(defn- built-tags [path]
  (let [files (pack (str "tags/" path))
        built (tags/build files (element-exists path))]
    (into {}
          (map (fn [[id vs]] [(tag-name id) (mapv kebab vs)]))
          (sort-by (comp tag-name key) built))))

(def ^:private ^:table all-tags
  (delay (into {} (map (fn [p] [p (built-tags p)])) (tag-paths))))

(defn- sent-tags? [[path ts]]
  (and (seq ts)
       (or (contains? (registries) path)
           (contains? (datapack) path))))

(def ^:private ^:table sent-tags
  (delay (into {} (filter sent-tags?) (sort-by key @all-tags))))

(defn tags
  "Returns the tags the server sends to the client, by registry: each
  built-in or synced registry that has any."
  [] @sent-tags)

(defn registry-tags
  "Returns the tags of registry path by name, each a vector of its
  entries in the order vanilla builds it; nil when it has none."
  [path]
  (get @all-tags path))

(defn tag-values [registry tag]
  (get (registry-tags registry) tag []))

(def ^:private ^:table brewing-table
  (delay (assoc (:brewing @tables)
                :fuel (tag-values "item" "brewing_fuel"))))

(def ^:private ^:table recipe-table
  (delay
    (let [items (registry-tags "item")
          own (select-keys @tables [:fuel :brewing :dyes])]
      (merge (recipes/recipes (pack "recipe") items)
             (assoc own :brewing @brewing-table)))))

(defn recipes
  "Returns the recipes by kind, with the fuel, the brewing mixes and
  the dye colours."
  [] @recipe-table)

(defn cooking-recipes
  "Returns the cooking recipes in search order."
  [] (:cooking (recipes)))

(defn fuel
  "Returns how many ticks each item burns for in a furnace."
  [] (:fuel @tables))

(defn brewing
  "Returns the containers, mixes and fuel of a brewing stand."
  [] @brewing-table)

(defn smithing-recipes
  "Returns the transform and trim recipes of a smithing table."
  [] (:smithing (recipes)))

(def ^:private ^:table item-table
  (delay (items/items (pack "components/item") registry-tags
                      (select-keys @tables item-facts))))

(defn items [] @item-table)

(defn use-cooldown
  "Returns the cooldown group and the ticks item locks it for.
  Returns nil when the item has no cooldown."
  [item]
  (when-let [c (get-in (items) [item :use-cooldown])]
    [(get c :group item) (long (* 20.0 (double (:seconds c))))]))

(defn cooldown-group
  "Returns the group whose cooldown locks item."
  [item]
  (get-in (items) [item :use-cooldown :group] item))

(defn max-stack ^long [item]
  (long (get-in (items) [item :max-stack] 64)))

(defn jukebox-song [item]
  (get-in (items) [item :jukebox-song]))

(defn equip-slot [item]
  (get-in (items) [item :equip]))

(defn swap-slot
  "Returns the slot a player puts item on by using it, or nil."
  [item]
  (get-in (items) [item :swap]))

(defn mob-bucket
  "Returns the fluid and the sound of a bucket of a mob, or nil."
  [item]
  (get-in (items) [item :mob-bucket]))

(defn equip-sound [item]
  (get-in (items) [item :equip-sound] :item.armor.equip-generic))

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
  "Returns the rarity of an item."
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

(defn each-run!
  "Calls f with the id and the value of each state that table t
  gives a value. Each run [from to i] gives the value i of the
  palette to the states from to to. The walk skips states past the
  last known one."
  [{:keys [palette runs]} f]
  (let [n (block-state-count)]
    (doseq [[from to i] runs
            :let [v (nth palette i)]
            id (range from (inc (min (long to) (dec n))))]
      (f id v))))

(defn- object-table [t]
  (let [a (object-array (block-state-count))]
    (each-run! t (fn [id v] (aset a (int id) v)))
    a))

(defn- byte-table [t ^long default]
  (let [a (byte-array (block-state-count))]
    (Arrays/fill a (byte default))
    (each-run! t (fn [id v] (aset a (int id) (byte (long v)))))
    a))

(def ^:private ^:table shape-table
  (delay (object-table (read-edn "shapes.edn"))))

(def ^:private ^:table outline-table
  (delay (object-table (read-edn "outlines.edn"))))

(def ^:private ^:table sturdy-tables
  (delay (update-vals (read-edn "sturdy.edn") #(byte-table % 63))))

(def ^:private ^:table flag-table
  (delay (byte-table (read-edn "flags.edn") 0)))

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
  (:full @sturdy-tables))

(defn sturdy-center
  "Returns which faces of every state hold a thing at their center,
  by id."
  ^bytes []
  (:center @sturdy-tables))

(defn sturdy-rigid
  "Returns which faces of every state hold things rigidly, by id."
  ^bytes []
  (:rigid @sturdy-tables))

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

(defn- single ^double [v] (double (float v)))

(defn place-sound [block]
  (get-in (sounds) [(:sound (info block)) :place]))

(defn placed-sound [block item]
  (let [t (get (sounds) (:sound (info block)))]
    {:kind   (or (get-in (items) [item :place-sound]) (:place t))
     :volume (single (/ (+ 1.0 (single (:volume t 1.0))) 2.0))
     :pitch  (single (* (single 0.8) (single (:pitch t 1.0))))}))

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
