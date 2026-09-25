(ns collider.game.command.components
  "Saved forms of item components as commands read them.
  A decoder takes a tag and returns [:ok value] with the value a
  stack holds, [:malformed why] with the text of the codec error,
  or [:raw tag] when the check is not modelled."
  (:require [collider.game.command.dfu :as dfu
             :refer [fixed from-file identifier named numbered
                     enum-of int-in float-in bool-of by-name]]
            [collider.game.command.text-codec :as tc]))

(set! *warn-on-reflection* true)

(def ^:private fish-patterns
  "TropicalFish.Pattern by name, to the id base | index << 8."
  (named (zipmap ["kob" "sunstreak" "snooper" "dasher" "brinely"
                  "spotty" "flopper" "stripey" "glitter" "blockfish"
                  "betty" "clayfish"]
                 (for [base [0 1] i (range 6)]
                   (bit-or base (bit-shift-left i 8))))))

(def ^:private rabbits
  (named {"brown" 0 "white" 1 "black" 2 "white_splotched" 3
          "gold" 4 "salt" 5 "evil" 99}))

(defn- all-ok
  "Returns the direct holder of the map of ks to the values of
  results rs, or tag kept raw when one fails. Errors of inline
  values are not modelled."
  [tag ks rs]
  (if (every? #(= :ok (first %)) rs)
    [:ok {:direct (zipmap ks (map second rs))}]
    [:raw tag]))

(defn- direct-sound
  "Decodes an inline SoundEvent: sound_id and a lenient range."
  [tag]
  (if (map? tag)
    (let [rng (:range tag)
          r (all-ok tag [:sound] [(identifier (:sound_id tag))])]
      (cond-> r
        (and (= :ok (first r)) (number? rng))
        (assoc-in [1 :direct :range] (double (unchecked-float rng)))))
    (dfu/not-map tag)))

(defn- inline-raw
  "Keeps an inline value raw: its codec is not modelled."
  [tag]
  (if (map? tag) [:raw tag] (dfu/not-map tag)))

(def ^:private sound (from-file "sound_event" direct-sound))

(defn- direct-instrument
  "Decodes an inline Instrument."
  [tag]
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

(defn- rgb
  "Decodes RGB_COLOR_CODEC: an int, or three floats from 0 to 1.
  Errors of a list of floats are not modelled."
  [tag]
  (cond (number? tag) [:ok (long (unchecked-int tag))]
        (floats3? tag) [:ok (argb tag)]
        (vector? tag) [:raw tag]
        :else (not-rgb tag)))

(def ^:private food
  "FoodProperties.DIRECT_CODEC."
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
  "ItemEnchantments.CODEC. Several entries go in the order of our
  map; the order of vanilla is the one of an identity hash."
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
  "Recipe.KEY_CODEC list, kept as the tag the codec writes."
  (dfu/listed (dfu/mapped identifier dfu/full-id)))

(def ^:private block-state
  (dfu/unbounded-map dfu/string-of dfu/string-of))

(def ^:private pot-decorations
  "PotDecorations.CODEC: the four sides, brick where none is given."
  (dfu/mapped (dfu/limited (by-name "item") 4)
              #(vec (take 4 (concat % (repeat :brick))))))

(defn- holders [registry]
  (dfu/holder-set registry (fixed registry)))

(def ^:private repairable
  (dfu/record [:items :items (holders "item") :req]))

(def ^:private damage-resistant
  (dfu/record [:types :types (holders "damage_type") :req]))

(def ^:private dyes
  [:white :orange :magenta :light-blue :yellow :lime :pink :gray
   :light-gray :cyan :purple :blue :brown :green :red :black])

(def ^:private decoders
  "The saved forms of components commands check.
  Others are kept as the raw tag, unchecked."
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
  "Returns the decoded value of component k from tag."
  [k tag]
  (if-let [f (decoders k)] (f tag) [:raw tag]))
