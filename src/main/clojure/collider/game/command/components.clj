(ns collider.game.command.components
  "Saved forms of item components as commands read them."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.decode :as dfu
             :refer [fixed from-file identifier named numbered
                     enum-of int-in float-in bool-of by-name]]
            [collider.game.command.reader :as r]
            [collider.hash-order :as hash-order]
            [collider.game.command.snbt :as snbt]
            [collider.game.command.text :as tc]))

(set! *warn-on-reflection* true)

(def ^:private fish-patterns
  "The tropical fish patterns by name, to their ids."
  (named (zipmap ["kob" "sunstreak" "snooper" "dasher" "brinely"
                  "spotty" "flopper" "stripey" "glitter" "blockfish"
                  "betty" "clayfish"]
                 (for [base [0 1] i (range 6)]
                   (bit-or base (bit-shift-left i 8))))))

(def ^:private rabbits
  (named {"brown" 0 "white" 1 "black" 2 "white_splotched" 3
          "gold" 4 "salt" 5 "evil" 99}))

(defn- all-ok [tag ks rs]
  (if (every? #(= :ok (first %)) rs)
    [:ok {:direct (zipmap ks (map second rs))}]
    [:raw tag]))

(defn- direct-sound [tag]
  (if (map? tag)
    (let [rng (:range tag)
          r (all-ok tag [:sound] [(identifier (:sound_id tag))])]
      (cond-> r
        (and (= :ok (first r)) (number? rng))
        (assoc-in [1 :direct :range] (double (unchecked-float rng)))))
    (dfu/not-map tag)))

(defn- inline-raw [tag]
  (if (map? tag) [:raw tag] (dfu/not-map tag)))

(def ^:private sound (from-file "sound_event" direct-sound))

(defn- direct-instrument [tag]
  (if (map? tag)
    (let [p dfu/positive-float]
      (all-ok tag [:sound :use-duration :range :description]
              [(sound (:sound_event tag)) (p (:use_duration tag))
               (p (:range tag)) (tc/text-of (:description tag))]))
    (dfu/not-map tag)))

(defn- channel ^long [x]
  (let [v (unchecked-float (* (double (unchecked-float x)) 255.0))]
    (bit-and 0xFF (long (Math/floor v)))))

(defn- argb ^long [tag]
  (let [add #(bit-or (bit-shift-left %1 8) (channel %2))]
    (long (unchecked-int (reduce add 0xFF tag)))))

(defn- floats3? [tag]
  (and (vector? tag) (= 3 (count tag)) (every? number? tag)))

(defn- not-rgb [tag]
  [:malformed (str "Failed to parse either. First: Not a number;"
                   " Second: Not a list: " (dfu/printed tag))])

(defn- rgb [tag]
  (cond (number? tag) [:ok (long (unchecked-int tag))]
        (floats3? tag) [:ok (argb tag)]
        (vector? tag) [:raw tag]
        :else (not-rgb tag)))

(def ^:private food
  (dfu/record [:nutrition :nutrition dfu/non-negative :req]
              [:saturation :saturation dfu/float-of :req]
              [:can_always_eat :can-always-eat bool-of :opt false]))

(def ^:private weapon
  (dfu/record [:item_damage_per_attack :damage-per-attack
               dfu/non-negative :opt 1]
              [:disable_blocking_for_seconds :disable-blocking-seconds
               dfu/non-negative-float :opt 0.0]))

(def ^:private use-cooldown
  (dfu/record [:seconds :seconds dfu/positive-float :req]
              [:cooldown_group :group identifier :opt]))

(def ^:private enchantable
  (dfu/mapped (dfu/record [:value :value dfu/positive-int :req])
              :value))

(def ^:private attack-range
  (let [reach (float-in 0 64
                        "Value must be within range [0.0;64.0]: ")]
    (dfu/record
      [:min_reach :min-reach reach :opt 0.0]
      [:max_reach :max-reach reach :opt 3.0]
      [:min_creative_reach :min-creative-reach reach :opt 0.0]
      [:max_creative_reach :max-creative-reach reach :opt 5.0]
      [:hitbox_margin :hitbox-margin
       (float-in 0 1 "Value must be within range [0.0;1.0]: ") :opt
       (double (float 0.3))]
      [:mob_factor :mob-factor (dfu/float-range 0 2) :opt 1.0])))

(def ^:private swing-animation
  (dfu/record [:type :type (enum-of [:none :whack :stab]) :opt :whack]
              [:duration :duration dfu/positive-int :opt 6]))

(def ^:private use-effects
  (dfu/record [:can_sprint :can-sprint bool-of :opt false]
              [:interact_vibrations :interact-vibrations bool-of :opt
               true]
              [:speed_multiplier :speed-multiplier
               (dfu/float-range 0 1) :opt (double (float 0.2))]))

(def ^:private custom-model-data
  (let [l #(dfu/listed %)]
    (dfu/record [:floats :floats (l dfu/float-of) :opt []]
                [:flags :flags (l bool-of) :opt []]
                [:strings :strings (l dfu/string-of) :opt []]
                [:colors :colors (l rgb) :opt []])))

(def ^:private tooltip-display
  (dfu/record [:hide_tooltip :hide-tooltip bool-of :opt false]
              [:hidden_components :hidden
               (dfu/mapped
                 (dfu/listed (by-name "data_component_type"))
                 (comp vec distinct))
               :opt []]))

(def ^:private enchantments
  (dfu/unbounded-map (fixed "enchantment") (dfu/int-range 1 255)))

(def ^:private stew-effects
  (dfu/listed
    (dfu/record [:id :effect (by-name "mob_effect") :req]
                [:duration :duration dfu/int-of :lenient 160])))

(def ^:private trim
  (dfu/record [:material :material
               (from-file "trim_material" inline-raw) :req]
              [:pattern :pattern
               (from-file "trim_pattern" inline-raw) :req]))

(def ^:private recipes
  (dfu/listed (dfu/mapped identifier dfu/full-id)))

(def ^:private block-state
  (dfu/unbounded-map dfu/string-of dfu/string-of))

(def ^:private pot-decorations
  "Decodes the four sides of a pot.
  A side not given is brick."
  (dfu/mapped (dfu/limited (by-name "item") 4)
              #(vec (take 4 (concat % (repeat :brick))))))

(defn- holders [registry]
  (dfu/holder-set registry (fixed registry)))

(def ^:private repairable
  (dfu/record [:items :items (holders "item") :req]))

(def ^:private damage-resistant
  (dfu/record [:types :types (holders "damage_type") :req]))

(defn- double-of [tag]
  (if (number? tag)
    [:ok (double tag)]
    [:malformed "Not a number"]))

(def ^:private display-kinds
  {"default" 0 "hidden" 1 "override" 2})

(def ^:private display-kind
  (dfu/record [:type :kind (named display-kinds) :req]))

(defn- display-of [tag kind]
  (if (= 2 kind)
    ((dfu/mapped (dfu/record [:value :text tc/text-of :req])
                 #(assoc % :display 2))
     tag)
    [:ok {:display kind}]))

(defn- modifier-display [tag]
  (let [[op v :as r] (display-kind tag)]
    (if (= :ok op) (display-of tag (:kind v)) r)))

(def ^:private slot-groups
  [:any :mainhand :offhand :hand :feet :legs :chest :head :armor
   :body :saddle])

(def ^:private operations
  [:add-value :add-multiplied-base :add-multiplied-total])

(def ^:private attribute-entry
  (dfu/mapped
    (dfu/record
      [:type :attribute (by-name "attribute") :req]
      [:id :id identifier :req]
      [:amount :amount double-of :req]
      [:operation :operation (enum-of operations) :req]
      [:slot :slot (enum-of slot-groups) :opt :any]
      [:display :display modifier-display :opt {:display 0}])
    (fn [v]
      (let [ks [:id :amount :operation]]
        (assoc (apply dissoc v ks) :modifier (select-keys v ks))))))

(def ^:private exact-value
  (dfu/mapped dfu/string-of #(hash-map :left {:value %})))

(def ^:private value-range
  (dfu/mapped (dfu/record [:min :min dfu/string-of :opt]
                          [:max :max dfu/string-of :opt])
              #(hash-map :right %)))

(defn- state-matcher [tag]
  (dfu/either (exact-value tag) (value-range tag)))

(def ^:private state-entries
  (dfu/mapped (dfu/unbounded-map dfu/string-of state-matcher)
              #(mapv (fn [[k v]] {:name k :matcher v}) %)))

(defn- read-compound [s]
  (let [res (snbt/read-fully (r/reader s))
        v (first res)]
    (cond (r/error? res)
          [:malformed (str (:key res) " at position " (:cursor res))]
          (map? v) [:ok v]
          :else [:malformed (str "Expected compound tag, got "
                                 (dfu/printed v))])))

(defn- snbt-compound [tag]
  (if (string? tag) (read-compound tag) [:malformed "Not a string"]))

(defn- compound [tag]
  (if (map? tag)
    [:ok tag]
    [:malformed (str "Not a compound tag: " (dfu/printed tag))]))

(defn- nbt-predicate [tag]
  (dfu/settled tag (dfu/either (snbt-compound tag) (compound tag))))

(declare decode)

(defn- exact-entry [[k v]]
  (let [[op t :as r] (identifier (name k))]
    (if (= :ok op)
      (let [[vo vv :as vr] (decode t v)]
        (if (= :ok vo) [:ok [t vv]] vr))
      r)))

(defn- exact-of [tag]
  (if (map? tag)
    (let [rs (mapv exact-entry tag)]
      (if (every? #(= :ok (first %)) rs)
        [:ok (mapv second rs)]
        [:raw tag]))
    (dfu/not-map tag)))

(defn- no-partial [_]
  [:malformed "Component predicates are not supported"])

(def ^:private block-predicate
  (dfu/mapped
    (dfu/record [:blocks :blocks (holders "block") :opt]
                [:state :state state-entries :opt]
                [:nbt :nbt nbt-predicate :opt]
                [:components :exact exact-of :opt []]
                [:predicates :partial no-partial :opt []])
    #(merge {:blocks nil :state nil :nbt nil} %)))

(defn- compact-list
  "Returns the decoder of one element of f or a non-empty list of
  them, to a vector. A map is decoded as the element alone."
  [f]
  (fn [tag]
    (let [one ((dfu/mapped f vector) tag)]
      (if (map? tag)
        one
        (let [many (dfu/non-empty (dfu/list-of f tag))]
          (dfu/settled tag (dfu/either many one)))))))

(def ^:private adventure
  (dfu/mapped (compact-list block-predicate) #(hash-map :predicates %)))

(def ^:private dyes
  [:white :orange :magenta :light-blue :yellow :lime :pink :gray
   :light-gray :cyan :purple :blue :brown :green :red :black])

(defn- nbt [m] (hash-order/compound (seq m)))

(defn- nbt-tree [x]
  (cond (map? x) (nbt (update-vals x nbt-tree))
        (vector? x) (mapv nbt-tree x)
        :else x))

(defn- with-nils [ks f]
  (dfu/mapped f #(merge (zipmap ks (repeat nil)) %)))

(defn- long-of [tag]
  (if (number? tag) [:ok (long tag)] [:malformed "Not a number"]))

(defn- unsigned-byte [tag]
  (if (number? tag)
    [:ok (bit-and (long (unchecked-byte tag)) 0xFF)]
    [:malformed "Not a number"]))

(defn- elements [f]
  (let [l (dfu/listed f)] (fn [tag] (l (or (dfu/as-list tag) tag)))))

(defn- string-to [most]
  (dfu/checked dfu/string-of #(<= (count %) most) (constantly nil)))

(def ^:private tag-data
  (dfu/mapped #(dfu/settled % (dfu/either (compound %) (snbt-compound %)))
              nbt-tree))

(defn- typed-data [registry]
  (fn [tag]
    (let [[op m :as r] (tag-data tag)]
      (cond (not= :ok op) r
            (nil? (:id m))
            [:malformed (str "Expected 'id' field in " (dfu/printed tag))]
            :else ((dfu/mapped (by-name registry)
                               #(hash-map :type % :data (dissoc m :id)))
                   (:id m))))))

(defn- property-of [block]
  (fn [tag]
    (let [[op v :as r] (dfu/string-of tag)]
      (cond (not= :ok op) r
            (some #(= v (data/snake %)) (keys (get-in (data/blocks)
                                                      [block :props])))
            r
            :else [:malformed (str "No property on " (data/wire block)
                                   " with name: " v)]))))

(def ^:private debug-stick-state
  (dfu/mapped (dfu/dispatched-map (by-name "block") property-of)
              #(nbt (update-keys % (comp keyword data/wire)))))

(def ^:private container-loot
  (dfu/mapped (dfu/record [:loot_table :table identifier :req]
                          [:seed :seed long-of :opt 0])
              (fn [{:keys [table seed]}]
                (nbt {:loot_table (dfu/full-id table)
                      :seed (when (not= 0 seed) seed)}))))

(def ^:private decoration
  (dfu/mapped
    (dfu/record [:type :type (by-name "map_decoration_type") :req]
                [:x :x double-of :req] [:z :z double-of :req]
                [:rotation :rotation dfu/float-of :req])
    #(nbt (-> (update % :type dfu/full-id) (update :rotation float)))))

(def ^:private map-decorations
  (dfu/mapped (dfu/unbounded-map dfu/string-of decoration)
              #(nbt (update-keys % keyword))))

(defn- holders-tag [v]
  (cond (map? v) (str "#" (dfu/full-id (:tag v)))
        (= 1 (count v)) (dfu/full-id (first v))
        :else (mapv dfu/full-id v)))

(defn- lock [tag]
  (cond (not (map? tag)) (dfu/not-map tag)
        (nil? (:items tag)) [:ok (nbt-tree tag)]
        :else ((dfu/mapped (holders "item")
                           #(nbt-tree (assoc tag :items (holders-tag %))))
               (:items tag))))

(defn- patch-key [s]
  (let [gone? (str/starts-with? s "!")
        [op id] (identifier (cond-> s gone? (subs 1)))]
    (cond (not= :ok op) [:raw s]
          (dfu/reg-key "data_component_type" id) [:ok [id gone?]]
          :else [:malformed (str "No component with type: '"
                                 (dfu/full-id id) "'")])))

(defn- patch-value [[id gone?]]
  (if gone? dfu/tag-unit #(decode id %)))

(defn- patch [m]
  (let [part #(into %1 (keep (fn [[[id gone?] v]] (%2 id gone? v))) m)]
    (not-empty
      (into {} (filter (comp seq val))
            {:components (part {} (fn [id gone? v] (when-not gone? [id v])))
             :removed (part #{} (fn [id gone? _] (when gone? id)))}))))

(def ^:private patch-of
  (dfu/mapped (dfu/dispatched-map patch-key patch-value) patch))

(def ^:private item-id
  (dfu/checked (by-name "item") #(not= :air %)
               (constantly "Item must not be minecraft:air")))

(def ^:private template-record
  (with-nils [:patch]
    (dfu/record [:id :item item-id :req]
                [:count :count (int-in 1 99 "Value must be within range [1;99]: ")
                 :opt 1]
                [:components :patch patch-of :opt])))

(defn- template [tag]
  (dfu/settled tag (dfu/either (template-record tag)
                               ((dfu/mapped item-id #(hash-map :item %
                                                               :count 1
                                                               :patch nil))
                                tag))))

(def ^:private slot
  (dfu/record [:slot :slot (dfu/int-range 0 255) :req]
              [:item :item template :req]))

(def ^:private container
  (dfu/mapped (dfu/limited slot 256)
              (fn [slots]
                (reduce (fn [v {:keys [slot item]}] (assoc v slot item))
                        (vec (repeat (inc (reduce max -1 (map :slot slots)))
                                     nil))
                        slots))))

(defn- effect-details [tag]
  ((dfu/mapped
     (dfu/record [:amplifier :amplifier unsigned-byte :opt 0]
                 [:duration :duration dfu/int-of :opt 0]
                 [:ambient :ambient bool-of :opt false]
                 [:show_particles :show-particles bool-of :opt true]
                 [:show_icon :show-icon bool-of :opt]
                 [:hidden_effect :hidden effect-details :opt])
     (fn [d] (merge {:show-icon (:show-particles d) :hidden nil} d)))
   tag))

(defn- effect [tag]
  (if (map? tag)
    (let [[io iv :as i] ((dfu/record [:id :effect (by-name "mob_effect")
                                      :req])
                         tag)
          [eo ev :as e] (effect-details tag)]
      (cond (dfu/raw-in? [i e]) [:raw tag]
            (= :ok io eo) [:ok (assoc iv :details ev)]
            :else (dfu/errors [i e])))
    (dfu/not-map tag)))

(def ^:private potion-record
  (with-nils [:potion :custom-color :custom-name]
    (dfu/record [:potion :potion (by-name "potion") :opt]
                [:custom_color :custom-color dfu/int-of :opt]
                [:custom_effects :custom-effects
                 (dfu/mapped (dfu/listed effect)
                             (fn [es]
                               (mapv #(assoc-in % [:details :hidden] nil)
                                     es)))
                 :opt []]
                [:custom_name :custom-name dfu/string-of :opt])))

(defn- potion-contents [tag]
  (let [id (dfu/mapped (by-name "potion")
                       #(hash-map :potion % :custom-color nil
                                  :custom-effects [] :custom-name nil))]
    (dfu/settled tag (dfu/either (potion-record tag) (id tag)))))

(defn- filterable [f]
  (let [full (with-nils [:filtered]
               (dfu/record [:raw :raw f :req] [:filtered :filtered f :opt]))
        simple (dfu/mapped f #(hash-map :raw % :filtered nil))]
    (fn [tag] (dfu/settled tag (dfu/either (full tag) (simple tag))))))

(def ^:private writable-book
  (dfu/mapped
    (dfu/record [:pages :pages
                 (dfu/limited (filterable (string-to 1024)) 100) :opt []])
    :pages))

(def ^:private written-book
  (dfu/record [:title :title (filterable (string-to 32)) :req]
              [:author :author dfu/string-of :req]
              [:generation :generation
               (int-in 0 3 "Value must be within range [0;3]: ") :opt 0]
              [:pages :pages (dfu/listed (filterable tc/text-of)) :opt []]
              [:resolved :resolved bool-of :opt false]))

(def ^:private lodestone-tracker
  (with-nils [:target]
    (dfu/record [:target :target
                 (dfu/record [:dimension :dimension identifier :req]
                             [:pos :pos (dfu/ints-of 3) :req])
                 :opt]
                [:tracked :tracked bool-of :opt true])))

(def ^:private firework-explosion
  (dfu/record [:shape :shape
               (enum-of [:small-ball :large-ball :star :creeper :burst])
               :req]
              [:colors :colors (elements dfu/int-of) :opt []]
              [:fade_colors :fade-colors (elements dfu/int-of) :opt []]
              [:has_trail :trail bool-of :opt false]
              [:has_twinkle :twinkle bool-of :opt false]))

(def ^:private fireworks
  (dfu/record [:flight_duration :flight-duration unsigned-byte :opt 0]
              [:explosions :explosions (dfu/limited firework-explosion 256)
               :opt []]))

(defn- player-name [tag]
  (let [[op v :as r] ((string-to 16) tag)]
    (if (and (= :ok op) (some #(or (<= (int %) 32) (<= 127 (int %))) v))
      [:malformed (str "Player name contained disallowed characters: '"
                       v "'")]
      r)))

(def ^:private uuid-of
  (dfu/mapped (dfu/ints-of 4)
              (fn [[a b c d]]
                (java.util.UUID.
                  (bit-or (bit-shift-left a 32) (bit-and b 0xFFFFFFFF))
                  (bit-or (bit-shift-left c 32) (bit-and d 0xFFFFFFFF))))))

(def ^:private property
  (with-nils [:signature]
    (dfu/record [:name :name (string-to 64) :req]
                [:value :value (string-to 32767) :req]
                [:signature :signature (string-to 1024) :opt])))

(defn- property-list [tag]
  (if (and (vector? tag) (< 16 (count tag)))
    ((dfu/limited property 16) tag)
    (dfu/list-of property tag)))

(def ^:private property-map
  (dfu/mapped
    (dfu/checked (dfu/unbounded-map dfu/string-of (dfu/listed dfu/string-of))
                 #(<= (count %) 16)
                 #(str "Cannot have more than 16 properties, but was "
                       (count %)))
    (fn [m] (vec (for [[k vs] m v vs] {:name k :value v :signature nil})))))

(defn- properties [tag]
  (dfu/settled tag (dfu/either (property-map tag) (property-list tag))))

(def ^:private named-profile
  (dfu/mapped (dfu/record [:id :id uuid-of :req]
                          [:name :name player-name :req]
                          [:properties :properties properties :opt []])
              #(hash-map :left %)))

(def ^:private partial-profile
  (dfu/mapped (with-nils [:name :id]
                (dfu/record [:name :name player-name :opt]
                            [:id :id uuid-of :opt]
                            [:properties :properties properties :opt []]))
              #(hash-map :right %)))

(def ^:private skin
  (with-nils [:body :cape :elytra :model]
    (dfu/record [:texture :body identifier :opt]
                [:cape :cape identifier :opt]
                [:elytra :elytra identifier :opt]
                [:model :model (enum-of [:wide :slim]) :opt])))

(defn- full-profile [tag]
  (if (map? tag)
    (let [[lo _ :as l] (named-profile tag)
          [ro _ :as r] (partial-profile tag)
          [po pv :as p] (cond (= :ok lo) l
                              (= :ok ro) r
                              :else (dfu/errors [l r]))
          [so sv :as s] (skin tag)]
      (cond (dfu/raw-in? [p s]) [:raw tag]
            (= :ok po so) [:ok {:profile pv :skin sv}]
            :else (dfu/errors [p s])))
    (dfu/not-map tag)))

(defn- profile [tag]
  (let [named #(hash-map :profile {:right {:name % :id nil
                                             :properties []}}
                           :skin {:body nil :cape nil :elytra nil
                                  :model nil})]
    (dfu/settled tag (dfu/either (full-profile tag)
                                 ((dfu/mapped player-name named) tag)))))

(def ^:private banner-patterns
  (dfu/listed
    (dfu/record [:pattern :pattern (from-file "banner_pattern" inline-raw)
                 :req]
                [:color :color (enum-of dyes) :req])))

(def ^:private bees
  (dfu/listed
    (dfu/record [:entity_data :data (typed-data "entity_type") :req]
                [:ticks_in_hive :ticks-in-hive dfu/int-of :req]
                [:min_ticks_in_hive :min-ticks-in-hive dfu/int-of :req])))

(def ^:private consume-kinds
  {:apply-effects
   (dfu/record [:effects :effects (dfu/listed effect) :req]
               [:probability :probability (dfu/float-range 0 1) :opt 1.0])
   :remove-effects
   (dfu/record [:effects :effects (holders "mob_effect") :req])
   :clear-all-effects (dfu/record)
   :teleport-randomly
   (dfu/record [:diameter :diameter dfu/positive-float :opt 16.0])
   :play-sound (dfu/record [:sound :sound sound :req])})

(defn- consume-effect [tag]
  (let [[op v :as r] ((dfu/record [:type :type
                                   (by-name "consume_effect_type") :req])
                      tag)]
    (if (= :ok op)
      ((dfu/mapped (consume-kinds (:type v)) #(assoc % :type (:type v)))
       (dissoc tag :type))
      r)))

(def ^:private consume-effects (dfu/listed consume-effect))

(def ^:private consumable
  (dfu/record [:consume_seconds :seconds dfu/non-negative-float :opt
               (double (float 1.6))]
              [:animation :animation
               (enum-of [:none :eat :drink :block :bow :trident :crossbow
                         :spyglass :toot-horn :brush :bundle :spear])
               :opt :eat]
              [:sound :sound sound :opt :entity.generic.eat]
              [:has_consume_particles :particles bool-of :opt true]
              [:on_consume_effects :on-consume consume-effects :opt []]))

(def ^:private death-protection
  (dfu/record [:death_effects :death-effects consume-effects :opt []]))

(def ^:private tool-rule
  (with-nils [:speed :correct-for-drops]
    (dfu/record [:blocks :blocks (holders "block") :req]
                [:speed :speed dfu/positive-float :opt]
                [:correct_for_drops :correct-for-drops bool-of :opt])))

(def ^:private tool
  (dfu/record [:rules :rules (dfu/listed tool-rule) :req]
              [:default_mining_speed :default-mining-speed dfu/float-of
               :opt 1.0]
              [:damage_per_block :damage-per-block dfu/non-negative :opt 1]
              [:can_destroy_blocks_in_creative :destroy-in-creative bool-of
               :opt true]))

(def ^:private equippable
  (with-nils [:asset :camera-overlay :allowed-entities]
    (dfu/record
      [:slot :slot (enum-of [:mainhand :feet :legs :chest :head :offhand
                             :body :saddle])
       :req]
      [:equip_sound :equip-sound sound :opt :item.armor.equip-generic]
      [:asset_id :asset identifier :opt]
      [:camera_overlay :camera-overlay identifier :opt]
      [:allowed_entities :allowed-entities (holders "entity_type") :opt]
      [:dispensable :dispensable bool-of :opt true]
      [:swappable :swappable bool-of :opt true]
      [:damage_on_hurt :damage-on-hurt bool-of :opt true]
      [:equip_on_interact :equip-on-interact bool-of :opt false]
      [:can_be_sheared :can-be-sheared bool-of :opt false]
      [:shearing_sound :shearing-sound sound :opt :item.shears.snip])))

(def ^:private damage-reduction
  (with-nils [:type]
    (dfu/record [:horizontal_blocking_angle :horizontal-blocking-angle
                 dfu/positive-float :opt 90.0]
                [:type :type (holders "damage_type") :opt]
                [:base :base dfu/float-of :req]
                [:factor :factor dfu/float-of :req])))

(def ^:private blocks-attacks
  (with-nils [:bypassed-by :block-sound :disable-sound]
    (dfu/record
      [:block_delay_seconds :block-delay-seconds dfu/non-negative-float
       :opt 0.0]
      [:disable_cooldown_scale :disable-cooldown-scale
       dfu/non-negative-float :opt 1.0]
      [:damage_reductions :damage-reductions (dfu/listed damage-reduction)
       :opt [{:horizontal-blocking-angle 90.0 :type nil :base 0.0
              :factor 1.0}]]
      [:item_damage :item-damage
       (dfu/record [:threshold :threshold dfu/non-negative-float :req]
                   [:base :base dfu/float-of :req]
                   [:factor :factor dfu/float-of :req])
       :opt {:threshold 1.0 :base 0.0 :factor 1.0}]
      [:bypassed_by :bypassed-by (holders "damage_type") :opt]
      [:block_sound :block-sound sound :opt]
      [:disabled_sound :disable-sound sound :opt])))

(def ^:private piercing-weapon
  (with-nils [:sound :hit-sound]
    (dfu/record [:deals_knockback :deals-knockback bool-of :opt true]
                [:dismounts :dismounts bool-of :opt false]
                [:sound :sound sound :opt]
                [:hit_sound :hit-sound sound :opt])))

(def ^:private kinetic-condition
  (dfu/record [:max_duration_ticks :max-duration-ticks dfu/non-negative
               :req]
              [:min_speed :min-speed dfu/float-of :opt 0.0]
              [:min_relative_speed :min-relative-speed dfu/float-of :opt
               0.0]))

(def ^:private kinetic-weapon
  (with-nils [:dismount :knockback :damage :sound :hit-sound]
    (dfu/record
      [:contact_cooldown_ticks :contact-cooldown-ticks dfu/non-negative
       :opt 10]
      [:delay_ticks :delay-ticks dfu/non-negative :opt 0]
      [:dismount_conditions :dismount kinetic-condition :opt]
      [:knockback_conditions :knockback kinetic-condition :opt]
      [:damage_conditions :damage kinetic-condition :opt]
      [:forward_movement :forward-movement dfu/float-of :opt 0.0]
      [:damage_multiplier :damage-multiplier dfu/float-of :opt 1.0]
      [:sound :sound sound :opt]
      [:hit_sound :hit-sound sound :opt])))

(def ^:private decoders
  "The components that commands check.
  Commands keep the others unchecked."
  (let [n dfu/non-negative p dfu/positive-int
        i (int-in Integer/MIN_VALUE Integer/MAX_VALUE "")
        dye (enum-of dyes)]
    {:damage n :repair-cost n :max-damage p :map-id i :map-color i
     :max-stack-size
     (int-in 1 99 "Value must be within range [1;99]: ")
     :ominous-bottle-amplifier
     (int-in 0 4 "Value must be within range [0;4]: ")
     :minimum-attack-charge
     (float-in 0 1 "Value must be within range [0.0;1.0]: ")
     :potion-duration-scale
     dfu/non-negative-float
     :enchantment-glint-override bool-of
     :unbreakable dfu/unit-of :glider dfu/unit-of
     :intangible-projectile dfu/tag-unit
     :item-model identifier :tooltip-style identifier
     :note-block-sound identifier
     :rarity (enum-of [:common :uncommon :rare :epic])
     :dyed-color rgb
     :dye dye :base-color dye :wolf/collar dye :cat/collar dye
     :sheep/color dye :shulker/color dye
     :tropical-fish/base-color dye :tropical-fish/pattern-color dye
     :food food :weapon weapon :use-cooldown use-cooldown
     :enchantable enchantable :attack-range attack-range
     :swing-animation swing-animation :use-effects use-effects
     :custom-model-data custom-model-data
     :tooltip-display tooltip-display :enchantments enchantments
     :stored-enchantments enchantments
     :suspicious-stew-effects stew-effects :trim trim
     :recipes recipes :block-state block-state
     :pot-decorations pot-decorations :repairable repairable
     :damage-resistant damage-resistant
     :can-break adventure :can-place-on adventure
     :attribute-modifiers (dfu/listed attribute-entry)
     :provides-banner-patterns (holders "banner_pattern")
     :custom-name tc/text-of :item-name tc/text-of :lore tc/lore
     :damage-type (fixed "damage_type")
     :instrument (from-file "instrument" direct-instrument)
     :provides-trim-material (from-file "trim_material" inline-raw)
     :jukebox-playable
     (dfu/mapped (fixed "jukebox_song") #(hash-map :song %))
     :break-sound sound
     :painting/variant (fixed "painting_variant")
     :villager/variant (fixed "villager_type")
     :wolf/variant (fixed "wolf_variant")
     :wolf/sound-variant (fixed "wolf_sound_variant")
     :pig/variant (fixed "pig_variant")
     :pig/sound-variant (fixed "pig_sound_variant")
     :cow/variant (fixed "cow_variant")
     :cow/sound-variant (fixed "cow_sound_variant")
     :chicken/variant (fixed "chicken_variant")
     :chicken/sound-variant (fixed "chicken_sound_variant")
     :zombie-nautilus/variant (fixed "zombie_nautilus_variant")
     :frog/variant (fixed "frog_variant")
     :cat/variant (fixed "cat_variant")
     :cat/sound-variant (fixed "cat_sound_variant")
     :fox/variant (numbered "red" "snow")
     :salmon/size (numbered "small" "medium" "large")
     :parrot/variant
     (numbered "red_blue" "blue" "green" "yellow_blue" "gray")
     :tropical-fish/pattern fish-patterns
     :mooshroom/variant (numbered "red" "brown")
     :rabbit/variant rabbits
     :horse/variant
     (numbered "white" "creamy" "chestnut" "brown" "black" "gray"
               "dark_brown")
     :llama/variant (numbered "creamy" "white" "brown" "gray")
     :axolotl/variant
     (numbered "lucy" "wild" "gold" "cyan" "blue")
     :custom-data tag-data :bucket-entity-data tag-data
     :entity-data (typed-data "entity_type")
     :block-entity-data (typed-data "block_entity_type")
     :debug-stick-state debug-stick-state :container-loot container-loot
     :map-decorations map-decorations :lock lock
     :use-remainder (dfu/mapped template #(hash-map :convert-into %))
     :sulfur-cube-content (dfu/mapped template #(hash-map :absorbed %))
     :charged-projectiles (dfu/limited template 1024)
     :bundle-contents (dfu/listed template) :container container
     :potion-contents potion-contents
     :writable-book-content writable-book
     :written-book-content written-book
     :lodestone-tracker lodestone-tracker
     :firework-explosion firework-explosion :fireworks fireworks
     :profile profile :banner-patterns banner-patterns :bees bees
     :consumable consumable :death-protection death-protection
     :tool tool :equippable equippable :blocks-attacks blocks-attacks
     :piercing-weapon piercing-weapon :kinetic-weapon kinetic-weapon}))

(defn decode
  [k tag]
  (if-let [f (decoders k)] (dfu/settled tag (f tag)) [:raw tag]))
