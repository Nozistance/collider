(ns collider.game.enchantment
  "Enchantments with their cost, reach and rivals."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.num :as num]))

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

(defn- f [v k] (num/f32 (double (get v k))))

(defn- linear [v]
  (let [b (f v "base") p (f v "per_level_above_first")]
    (fn [l] (num/f32 (+ b (* p (double (dec (long l)))))))))

(defn- squared [v]
  (let [a (f v "added")]
    (fn [l] (num/f32 (+ (double (* (long l) (long l))) a)))))

(declare by-level)

(defn- fraction [v]
  (let [n (by-level (get v "numerator"))
        d (by-level (get v "denominator"))]
    (fn [l]
      (let [q (double (d l))]
        (if (zero? q) 0.0 (num/f32 (/ (double (n l)) q)))))))

(defn- by-level
  "Returns level-based value v as a function of the level."
  [v]
  (if (number? v)
    (constantly (num/f32 (double v)))
    (case (get v "type")
      "minecraft:constant" (constantly (f v "value"))
      "minecraft:linear" (linear v)
      "minecraft:levels_squared" (squared v)
      "minecraft:fraction" (fraction v)
      (throw (ex-info "level-based value not modelled" {:value v})))))

(defn- attribute-effect [v]
  {:attribute (data/kebab (get v "attribute")) :id (get v "id")
   :amount (by-level (get v "amount"))
   :operation (data/kebab (get v "operation"))})

(defn- primary [json]
  (some->> (get json "primary_items") (holders "item")))

(defn- attribute-effects [json]
  (let [es (get-in json ["effects" "minecraft:attributes"])]
    (mapv attribute-effect es)))

(defn- requirement [r]
  (case (get r "condition")
    nil (constantly true)
    "minecraft:match_tool"
    (let [items (holders "item" (get-in r ["predicate" "items"]))]
      #(contains? items %))
    "minecraft:inverted" (complement (requirement (get r "term")))
    (throw (ex-info "requirement not modelled" {:requirement r}))))

(defn- damage-effect [v]
  (let [e (get v "effect")]
    (when-not (= "minecraft:remove_binomial" (get e "type"))
      (throw (ex-info "item damage effect not modelled" {:effect e})))
    {:chance (by-level (get e "chance"))
     :when (requirement (get v "requirements"))}))

(defn- set-value [v]
  (let [e (get v "effect")]
    (when-not (= "minecraft:set" (get e "type"))
      (throw (ex-info "block experience effect not modelled" {:effect e})))
    (by-level (get e "value"))))

(defn- enchantment [json]
  {:anvil-cost (get json "anvil_cost")
   :exclusive (holders "enchantment" (get json "exclusive_set" []))
   :max-level (get json "max_level")
   :min-cost (cost (get json "min_cost"))
   :max-cost (cost (get json "max_cost"))
   :weight (get json "weight")
   :supported (holders "item" (get json "supported_items"))
   :primary (primary json)
   :slots (mapv data/kebab (get json "slots"))
   :attributes (attribute-effects json)
   :block-xp (mapv set-value
                   (get-in json ["effects" "minecraft:block_experience"]))
   :item-damage (mapv damage-effect
                      (get-in json ["effects" "minecraft:item_damage"]))})

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

(defn- gaussian ^double [roll]
  (let [u (max (double (roll :gauss-a)) Double/MIN_VALUE)]
    (* (Math/sqrt (* -2.0 (Math/log u)))
       (Math/cos (* 2.0 Math/PI (double (roll :gauss-b)))))))

(defn- normal-drop ^long [^double n ^double p roll]
  (let [miu (Math/floor (num/f32 (* n p)))
        sigma (Math/sqrt (num/f32 (* (num/f32 (* n p)) (- 1.0 p))))
        drop (Math/round (+ miu (* (gaussian roll) sigma)))]
    (max 0 (min (long n) drop))))

(defn- removed
  "Returns n less the points that each go with chance p, the way
  vanilla removes them as a binomial."
  ^double [^double n ^double p roll]
  (let [q (num/f32 (- 1.0 p))]
    (- n (if (and (> n 128.0) (>= (num/f32 (* n p)) 20.0)
                  (>= (num/f32 (* n q)) 20.0))
           (normal-drop n p roll)
           (count (filter #(< (double (roll %)) p) (range (long n))))))))

(defn item-damage
  "Returns the wear of n points that stack takes after its
  enchantments, with roll the random number of a key."
  [stack n roll]
  (let [effects (for [[k level] (get-in stack [:components :enchantments])
                      e (:item-damage (info k))
                      :when ((:when e) (:item stack))]
                  [e level])]
    (long (reduce (fn [^double n [e level]]
                    (removed n (double ((:chance e) level)) roll))
                  (double n) effects))))

(defn block-xp
  "Returns the experience n of a broken block after the enchantments
  of the tool stack."
  ^long [stack ^long n]
  (long (reduce (fn [^double v [k level]]
                  (reduce #(double (%2 level)) v (:block-xp (info k))))
                (double n)
                (get-in stack [:components :enchantments]))))
