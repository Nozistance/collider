(ns collider.game.enchantment
  "Enchantments of the vanilla pack: their cost, reach and rivals."
  (:require [clojure.string :as str]
            [collider.data :as data]))

(set! *warn-on-reflection* true)

(defn- tagged [registry ^String v]
  (let [tag (str/replace (subs v 1) #"^minecraft:" "")]
    (or (get (data/registry-tags registry) tag)
        (throw (ex-info "unknown tag"
                        {:registry registry :tag tag})))))

(defn- holders [registry v]
  (into #{}
        (cond
          (sequential? v) (map data/kebab v)
          (str/starts-with? v "#") (tagged registry v)
          :else [(data/kebab v)])))

(defn- cost [v]
  {:base (get v "base" 0)
   :per-level (get v "per_level_above_first" 0)})

(defn- primary [json]
  (some->> (get json "primary_items") (holders "item")))

(defn- enchantment [json]
  {:anvil-cost (get json "anvil_cost")
   :exclusive (holders "enchantment" (get json "exclusive_set" []))
   :max-level (get json "max_level")
   :min-cost (cost (get json "min_cost"))
   :max-cost (cost (get json "max_cost"))
   :weight (get json "weight")
   :supported (holders "item" (get json "supported_items"))
   :primary (primary json)})

(def ^:private ^:table table
  (delay (into {}
               (map (fn [[id json]]
                      [(data/kebab id) (enchantment json)]))
               (data/pack "enchantment"))))

(defn all
  "Returns the cost, reach and rivals of every enchantment."
  []
  @table)

(defn info
  "Returns the cost, reach and rivals of enchantment name."
  [name]
  (get (all) name))
