(ns collider.data.items
  "The facts of items read from their default components."
  (:require [clojure.string :as str]
            [collider.data.pack :as pack :refer [flt kw]]))

(set! *warn-on-reflection* true)

(defn- unmodelled [v]
  (throw (pack/unknown "default component not modelled" {:value v})))

(defn- empty-or-throw [k v]
  (when (seq v)
    (throw (pack/unknown "default component not modelled" {k v})))
  v)

(defn- not-modelled [what v]
  (pack/unknown (str what " not modelled") {:value v}))

(def ^:private displays {"default" 0 "hidden" 1})

(defn- modifier-display [v]
  (let [t (get-in v ["display" "type"] "default")]
    (or (displays t) (throw (not-modelled "modifier display" v)))))

(defn- modifier-entry [v]
  {:attribute (kw (get v "type")) :slot (kw (get v "slot" "any"))
   :display {:display (modifier-display v)}
   :modifier {:id (kw (get v "id")) :amount (double (get v "amount"))
              :operation (kw (get v "operation"))}})

(defn- modifiers [v] (mapv modifier-entry v))

(defn- potion-default [v]
  (empty-or-throw "custom_effects" (get v "custom_effects"))
  (when-let [extra (seq (dissoc v "potion" "custom_effects"))]
    (throw (not-modelled "default potion" extra)))
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

(defn- no-patterns [v] (empty-or-throw "banner_patterns" (vec v)))

(defn- no-pages [v] (empty-or-throw "pages" (vec (get v "pages"))))

(defn- no-contents [v] (empty-or-throw "bundle_contents" (vec v)))

(def ^:private crafted-components
  {"minecraft:damage"               [:damage identity]
   "minecraft:max_damage"           [:max-damage identity]
   "minecraft:max_stack_size"       [:max-stack-size identity]
   "minecraft:block_state"          [:block-state identity]
   "minecraft:repair_cost"          [:repair-cost identity]
   "minecraft:enchantable"          [:enchantable #(get % "value")]
   "minecraft:bundle_contents"      [:bundle-contents no-contents]
   "minecraft:dye"                  [:dye kw]
   "minecraft:attribute_modifiers"  [:attribute-modifiers modifiers]
   "minecraft:instrument"           [:instrument kw]
   "minecraft:swing_animation"      [:swing-animation swing-animation]
   "minecraft:enchantments"         [:enchantments levels]
   "minecraft:stored_enchantments"  [:stored-enchantments levels]
   "minecraft:potion_contents"      [:potion-contents potion-default]
   "minecraft:banner_patterns"      [:banner-patterns no-patterns]
   "minecraft:pot_decorations"      [:pot-decorations pot-default]
   "minecraft:fireworks"            [:fireworks fireworks-default]
   "minecraft:firework_explosion"   [:firework-explosion unmodelled]
   "minecraft:writable_book_content" [:writable-book-content no-pages]
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
    (throw (not-modelled "hidden effect" v)))
  (let [visible? (get v "show_particles" true)]
    (sorted-map :id (kw (get v "id"))
                :duration (get v "duration" 0)
                :amplifier (get v "amplifier" 0)
                :ambient? (get v "ambient" false)
                :visible? visible?
                :icon? (get v "show_icon" visible?))))

(defn- tag-name [s] (str/replace s #"^minecraft:" ""))

(defn- effect-tag [tags s]
  (or (get (tags "mob_effect") (tag-name (subs s 1)))
      (throw (pack/unknown "unknown effect tag" {:tag s}))))

(defn- effect-names [tags v]
  (let [one #(if (str/starts-with? % "#")
               (effect-tag tags %)
               [(kw %)])]
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
      (throw (not-modelled "consume effect" e)))))

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

(defn- worn-by-player? [tags v]
  (let [one #(if (str/starts-with? % "#")
               (get (tags "entity_type") (tag-name (subs % 1)))
               [(kw %)])]
    (or (nil? v)
        (boolean (some #{:player}
                       (mapcat one (if (string? v) [v] v)))))))

(defn- equip-fields [tags cs]
  (let [e (get cs "minecraft:equippable")]
    (cond-> (sorted-map)
      (and e (get e "swappable" true)
           (worn-by-player? tags (get e "allowed_entities")))
      (assoc :swap (kw (get e "slot"))))))

(defn- combat-fields [cs]
  (let [egg (get-in cs ["minecraft:entity_data" "id"])
        resists (get-in cs ["minecraft:damage_resistant" "types"])
        pat (get cs "minecraft:provides_banner_patterns")
        weapon (get cs "minecraft:weapon")
        tag #(tag-name (subs % 1))]
    (cond-> (sorted-map)
      egg (assoc :spawns (kw egg))
      weapon (assoc :per-attack (get weapon "item_damage_per_attack" 1))
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

(defn- title [cs]
  (let [v (get cs "minecraft:item_name")]
    (if (map? v) {:translate (get v "translate")} v)))

(defn- station-fields [tags cs]
  (let [rep (get cs "minecraft:repairable")
        trim (get cs "minecraft:provides_trim_material")
        rarity (get cs "minecraft:rarity" "common")]
    (cond-> (sorted-map)
      (get cs "minecraft:item_name") (assoc :title (title cs))
      (not= "common" rarity) (assoc :rarity (kw rarity))
      rep (assoc :repairable
                 (pack/item-set (tags "item") (get rep "items")))
      trim (assoc :trim-material (kw trim)))))

(defn- tool-rule [tags r]
  (cond-> {:blocks (pack/item-set (tags "block") (get r "blocks"))}
    (contains? r "speed") (assoc :speed (flt (get r "speed")))
    (contains? r "correct_for_drops")
    (assoc :correct? (get r "correct_for_drops"))))

(defn- tool-fields [tags cs]
  (when-let [t (get cs "minecraft:tool")]
    {:tool {:rules (mapv #(tool-rule tags %) (get t "rules"))
            :default-speed (flt (get t "default_mining_speed" 1.0))
            :per-block (get t "damage_per_block" 1)}}))

(defn- item [tags cs]
  (merge (sorted-map :components (default-components cs))
         (stack-fields cs) (equip-fields tags cs) (combat-fields cs)
         (tool-fields tags cs)
         (consumable-fields tags cs) (station-fields tags cs)))

(defn- fact [k t] (update-vals t #(hash-map k %)))

(defn- class-facts [facts]
  (let [{:keys [compost wall-blocks place-sounds remainders
                banner-colors non-breakers item-names
                mob-buckets]} facts]
    [(fact :compost compost) (fact :wall wall-blocks)
     (fact :place-sound place-sounds) (fact :remainder remainders)
     (fact :banner-color banner-colors)
     (zipmap non-breakers (repeat {:breaks? false}))
     (fact :name item-names) (fact :mob-bucket mob-buckets)]))

(defn items
  "Returns the facts of every item by name.
  Components holds the default components of each item. Tags returns
  the tags of a registry by name."
  [components tags facts]
  (let [own (into (sorted-map)
                  (map (fn [[id cs]] [(kw id) (item tags cs)]))
                  components)]
    (pack/plain (apply merge-with merge own (class-facts facts)))))
