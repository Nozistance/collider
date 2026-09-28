(ns collider.tables.items
  "Reading items from the reports and the jar."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.tables.tags :refer [item-set]]
            [collider.tables.value
             :refer [flt kw plain unknown]])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(def ^:private attack-damage-modifier
  ["minecraft:attack_damage" "add_value" "mainhand"])

(defn- attack-damage ^double [components]
  (let [mods (get components "minecraft:attribute_modifiers")
        match? #(= (map % ["type" "operation" "slot"])
                   attack-damage-modifier)]
    (reduce + 0.0 (for [a mods :when (match? a)]
                    (double (get a "amount"))))))

(defn- unmodelled [v]
  (throw (unknown "default component not modelled" {:value v})))

(defn- empty-or-throw [k v]
  (when (seq v)
    (throw (unknown "default component not modelled" {k v})))
  v)

(defn- potion-default [v]
  (empty-or-throw "custom_effects" (get v "custom_effects"))
  (when-let [extra (seq (dissoc v "potion" "custom_effects"))]
    (throw (unknown "default potion not modelled" {:extra extra})))
  {:potion (some-> (get v "potion") kw) :custom-color nil
   :custom-effects [] :custom-name nil})

(defn- pot-default [v]
  (vec (take 4 (concat (map kw v) (repeat :brick)))))

(defn- fireworks-default [v]
  (empty-or-throw "explosions" (get v "explosions"))
  {:flight-duration (get v "flight_duration" 0) :explosions []})

(defn- levels [v] (into (sorted-map) (map (fn [[e n]] [(kw e) n])) v))

(defn- swing-animation [v]
  (sorted-map :type (kw (get v "type" "whack"))
              :duration (get v "duration" 6)))

(def ^:private crafted-components
  {"minecraft:damage"               [:damage identity]
   "minecraft:max_damage"           [:max-damage identity]
   "minecraft:max_stack_size"       [:max-stack-size identity]
   "minecraft:block_state"          [:block-state identity]
   "minecraft:repair_cost"          [:repair-cost identity]
   "minecraft:dye"                  [:dye kw]
   "minecraft:swing_animation"      [:swing-animation swing-animation]
   "minecraft:enchantments"         [:enchantments levels]
   "minecraft:stored_enchantments"  [:stored-enchantments levels]
   "minecraft:potion_contents"      [:potion-contents potion-default]
   "minecraft:banner_patterns"
   [:banner-patterns #(empty-or-throw "banner_patterns" (vec %))]
   "minecraft:pot_decorations"      [:pot-decorations pot-default]
   "minecraft:fireworks"            [:fireworks fireworks-default]
   "minecraft:firework_explosion"   [:firework-explosion unmodelled]
   "minecraft:written_book_content" [:written-book-content unmodelled]
   "minecraft:map_id"               [:map-id unmodelled]
   "minecraft:dyed_color"           [:dyed-color unmodelled]
   "minecraft:base_color"           [:base-color unmodelled]})

(defn- default-components [cs]
  (into (sorted-map)
        (keep (fn [[json [k f]]]
                (when (contains? cs json) [k (f (get cs json))])))
        crafted-components))

(defn- effect-instance [v]
  (when (contains? v "hidden_effect")
    (throw (unknown "hidden effect not modelled" {:value v})))
  (let [visible? (get v "show_particles" true)]
    (sorted-map :id (kw (get v "id"))
                :duration (get v "duration" 0)
                :amplifier (get v "amplifier" 0)
                :ambient? (get v "ambient" false)
                :visible? visible?
                :icon? (get v "show_icon" visible?))))

(defn- effect-names [tags v]
  (let [one (fn [s]
              (if (str/starts-with? s "#")
                (or (get-in tags ["mob_effect" (plain (subs s 1))])
                    (throw (unknown "unknown effect tag" {:tag s})))
                [(kw s)]))]
    (into [] (mapcat one) (if (string? v) [v] v))))

(defn- consume-effect [tags e]
  (let [t (kw (get e "type"))
        m (sorted-map :type t)]
    (case t
      :apply-effects
      (assoc m :effects (mapv effect-instance (get e "effects"))
             :probability (flt (get e "probability" 1.0)))
      :remove-effects
      (assoc m :effects (effect-names tags (get e "effects")))
      :clear-all-effects m
      :teleport-randomly
      (assoc m :diameter (flt (get e "diameter" 16.0)))
      :play-sound (assoc m :sound (kw (get e "sound")))
      (throw (unknown "consume effect not modelled" {:value e})))))

(defn- consumable [tags v]
  (sorted-map
    :seconds (flt (get v "consume_seconds" 1.6))
    :animation (kw (get v "animation" "eat"))
    :sound (kw (get v "sound" "minecraft:entity.generic.eat"))
    :particles? (get v "has_consume_particles" true)
    :effects (mapv #(consume-effect tags %)
                   (get v "on_consume_effects"))))

(defn- food [v]
  (sorted-map :nutrition (get v "nutrition")
              :saturation (flt (get v "saturation"))
              :always? (get v "can_always_eat" false)))

(defn- use-remainder [v]
  (sorted-map :item (kw (get v "id")) :count (get v "count" 1)))

(defn- use-cooldown [v]
  (cond-> (sorted-map :seconds (flt (get v "seconds")))
    (get v "cooldown_group")
    (assoc :group (kw (get v "cooldown_group")))))

(defn- stack-fields [cs]
  (let [n (get cs "minecraft:max_stack_size" 64)
        slot (get-in cs ["minecraft:equippable" "slot"])
        sound (get-in cs ["minecraft:equippable" "equip_sound"])
        song (get cs "minecraft:jukebox_playable")
        dye (get cs "minecraft:dye")
        tool (get cs "minecraft:tool")]
    (cond-> (sorted-map)
      (false? (get tool "can_destroy_blocks_in_creative"))
      (assoc :creative-break? false)
      (not= n 64) (assoc :max-stack n)
      slot (assoc :equip (kw slot))
      (string? sound) (assoc :equip-sound (kw sound))
      song (assoc :jukebox-song (kw song))
      dye (assoc :dye (kw dye)))))

(defn- combat-fields [cs]
  (let [egg (get-in cs ["minecraft:entity_data" "id"])
        hit (attack-damage cs)
        resists (get-in cs ["minecraft:damage_resistant" "types"])
        pat (get cs "minecraft:provides_banner_patterns")
        tag #(str/replace (subs % 1) #"^minecraft:" "")]
    (cond-> (sorted-map)
      egg (assoc :spawns (kw egg))
      (pos? hit) (assoc :attack-damage (flt hit))
      (string? resists) (assoc :resists (tag resists))
      (string? pat) (assoc :patterns (tag pat)))))

(defn- consumable-fields [tags cs]
  (let [eats (get cs "minecraft:consumable")
        left (get cs "minecraft:use_remainder")
        wait (get cs "minecraft:use_cooldown")
        grub (get cs "minecraft:food")]
    (cond-> (sorted-map)
      eats (assoc :consumable (consumable tags eats))
      left (assoc :use-remainder (use-remainder left))
      wait (assoc :use-cooldown (use-cooldown wait))
      grub (assoc :food (food grub)))))

(defn- item [tags cs]
  (merge (sorted-map :components (default-components cs))
         (stack-fields cs) (combat-fields cs)
         (consumable-fields tags cs)))

(defn- item-components [reports]
  (let [dir (io/file reports "minecraft" "components" "item")]
    (for [^File f (sort (File/.listFiles dir))
          :when (str/ends-with? (File/.getName f) ".json")]
      [(kw (str/replace (File/.getName f) #"\.json$" ""))
       (get (json/read-str (slurp f)) "components")])))

(defn vanilla-items [reports tags]
  (into (sorted-map)
        (keep (fn [[name cs]]
                (let [m (item tags cs)]
                  (when (seq m) [name m]))))
        (item-components reports)))

(defn- shown-name [lang cs]
  (let [v (get cs "minecraft:item_name")]
    (if (map? v) (get lang (get v "translate")) v)))

(defn- title [cs]
  (let [v (get cs "minecraft:item_name")]
    (if (map? v) {:translate (get v "translate")} v)))

(defn- station-item [tags lang cs]
  (let [rep (get cs "minecraft:repairable")
        trim (get cs "minecraft:provides_trim_material")
        nm (shown-name lang cs)
        rarity (get cs "minecraft:rarity" "common")]
    (cond-> (sorted-map)
      nm (assoc :name nm :title (title cs))
      (not= "common" rarity) (assoc :rarity (kw rarity))
      rep (assoc :repairable (item-set tags (get rep "items")))
      trim (assoc :trim-material (kw trim)))))

(defn station-items
  "Returns what each item shows as, repairs with and trims as."
  [reports tags lang]
  (into (sorted-map)
        (keep (fn [[name cs]]
                (let [m (station-item tags lang cs)]
                  (when (seq m) [name m]))))
        (item-components reports)))
