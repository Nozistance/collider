(ns collider.game.command.components
  "Saved forms of item components as commands read them."
  (:require [collider.game.command.decode :as dfu
             :refer [fixed from-file identifier named numbered
                     enum-of int-in float-in bool-of by-name]]
            [collider.game.command.reader :as r]
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
     (numbered "lucy" "wild" "gold" "cyan" "blue")}))

(defn decode
  [k tag]
  (if-let [f (decoders k)] (f tag) [:raw tag]))
