(ns collider.proto.components
  "Data components of item stacks, and particles, on the wire."
  (:require [collider.data :as data]
            [collider.data.particles :as particles]
            [collider.proto.buf :as buf]
            [collider.proto.codec :as c]
            [collider.proto.nbt :as nbt]
            [collider.proto.text :as text])
  (:import (collider.proto Buf)))

(set! *warn-on-reflection* true)

(declare components read-patch write-patch)

(defn- codec [r w] {:r r :w w})

(def ^:private c-bool
  (codec (fn [^Buf b] (buf/read-boolean b))
         (fn [^Buf b v] (buf/write-boolean! b (boolean v)))))

(def ^:private c-varint
  (codec (fn [^Buf b] (c/read-varint b))
         (fn [^Buf b v] (c/write-varint b (long v)))))

(def ^:private c-int
  (codec (fn [^Buf b] (long (buf/read-int b)))
         (fn [^Buf b v] (buf/write-int! b (int v)))))

(def ^:private c-float
  (codec (fn [^Buf b] (buf/read-float b))
         (fn [^Buf b v] (buf/write-float! b (float v)))))

(def ^:private c-double
  (codec (fn [^Buf b] (buf/read-double b))
         (fn [^Buf b v] (buf/write-double! b (double v)))))

(def ^:private c-string
  (codec (fn [^Buf b] (c/read-string b))
         (fn [^Buf b v] (c/write-string b (str v)))))

(def ^:private c-ident
  (codec c/read-id (fn [^Buf b v] (c/write-id b v))))

(def ^:private c-uuid
  (codec (fn [^Buf b] (c/read-uuid b))
         (fn [^Buf b v] (c/write-uuid b v))))

(def ^:private c-nbt
  (codec (fn [^Buf b] (nbt/read-nbt b))
         (fn [^Buf b v] (nbt/write-nbt b v))))

(def ^:private c-text
  (codec (fn [^Buf b] (nbt/read-nbt b))
         (fn [^Buf b v] (text/write-component b v))))

(def ^:private c-unit (codec (fn [^Buf _] true) (fn [^Buf _ _] nil)))

(def ^:private c-block-pos
  (codec (fn [^Buf b] (c/read-block-pos b))
         (fn [^Buf b [x y z]]
           (c/write-block-pos b (long x) (long y) (long z)))))

(defn- c-opt [{:keys [r w]}]
  (codec (fn [^Buf b] (when (buf/read-boolean b) (r b)))
         (fn [^Buf b v]
           (buf/write-boolean! b (some? v))
           (when (some? v) (w b v)))))

(defn- c-list [{:keys [r w]}]
  (codec (fn [^Buf b]
           (let [n (c/read-count b)] (mapv (fn [_] (r b)) (range n))))
         (fn [^Buf b v]
           (c/write-varint b (count v))
           (doseq [x v] (w b x)))))

(defn- c-map [k v]
  (codec (fn [^Buf b]
           (let [n (c/read-count b)
                 pair (fn [_] [((:r k) b) ((:r v) b)])]
             (apply array-map (mapcat pair (range n)))))
         (fn [^Buf b m]
           (c/write-varint b (count m))
           (doseq [[a x] m] ((:w k) b a) ((:w v) b x)))))

(defn- c-either [l r]
  (codec (fn [^Buf b]
           (if (buf/read-boolean b)
             {:left ((:r l) b)}
             {:right ((:r r) b)}))
         (fn [^Buf b v]
           (if (contains? v :left)
             (do (buf/write-boolean! b true) ((:w l) b (:left v)))
             (do (buf/write-boolean! b false)
                 ((:w r) b (:right v)))))))

(defn- record-codec [& kvs]
  (let [fields (mapv vec (partition 2 kvs))
        ks (mapv first fields)]
    (codec (fn [^Buf b]
             (let [vs (mapv (fn [[_ c]] ((:r c) b)) fields)]
               (apply array-map (interleave ks vs))))
           (fn [^Buf b v]
             (doseq [[k c] fields] ((:w c) b (get v k)))))))

(defn- c-enum [names]
  (let [by-id (vec names)
        by-name (into {} (map-indexed (fn [i n] [n (long i)])) names)]
    (codec (fn [^Buf b] (let [i (c/read-varint b)] (get by-id i i)))
           (fn [^Buf b v]
             (c/write-varint
               b (long (if (keyword? v) (get by-name v) v)))))))

(defn- c-reg [registry]
  (codec (fn [^Buf b] (data/entry-name registry (c/read-varint b)))
         (fn [^Buf b v]
           (c/write-varint b (data/entry-id registry v)))))

(defn- c-holder [registry direct]
  (codec (fn [^Buf b]
           (let [i (c/read-varint b)]
             (if (zero? i)
               {:direct ((:r direct) b)}
               (data/entry-name registry (dec i)))))
         (fn [^Buf b v]
           (if (map? v)
             (do (c/write-varint b 0) ((:w direct) b (:direct v)))
             (c/write-varint b (inc (data/entry-id registry v)))))))

(defn- c-holder-set [registry]
  (codec (fn [^Buf b]
           (let [n (dec (c/read-count b))]
             (if (neg? n)
               {:tag (c/read-id b)}
               (mapv (fn [_]
                       (data/entry-name registry (c/read-varint b)))
                     (range n)))))
         (fn [^Buf b v]
           (if (map? v)
             (do (c/write-varint b 0) (c/write-id b (:tag v)))
             (do (c/write-varint b (inc (count v)))
                 (doseq [x v]
                   (c/write-varint b (data/entry-id registry x))))))))

(defn- c-filterable [inner]
  (record-codec :raw inner :filtered (c-opt inner)))

(def ^:private dye-colors
  [:white :orange :magenta :light-blue :yellow :lime :pink :gray
   :light-gray :cyan :purple :blue :brown :green :red :black])

(def ^:private c-dye (c-enum dye-colors))

(def ^:private c-sound
  (c-holder "sound_event"
            (record-codec :sound c-ident :range (c-opt c-float))))

(def ^:private c-effect-details
  (let [self (promise)
        nested (codec (fn [^Buf b] ((:r @self) b))
                      (fn [^Buf b v] ((:w @self) b v)))
        details (record-codec
                 :amplifier c-varint :duration c-varint
                 :ambient c-bool :show-particles c-bool
                 :show-icon c-bool :hidden (c-opt nested))]
    @(deliver self details)))

(def ^:private c-effect-instance
  (record-codec :effect (c-reg "mob_effect")
                :details c-effect-details))

(def ^:private c-apply-effects
  (record-codec :effects (c-list c-effect-instance)
                :probability c-float))

(def ^:private consume-effects
  {:apply-effects c-apply-effects
   :remove-effects (record-codec :effects (c-holder-set "mob_effect"))
   :clear-all-effects (record-codec)
   :teleport-randomly (record-codec :diameter c-float)
   :play-sound        (record-codec :sound c-sound)})

(def ^:private c-consume-effect
  (codec (fn [^Buf b]
           (let [i (c/read-varint b)
                 t (data/entry-name "consume_effect_type" i)]
             (assoc ((:r (get consume-effects t)) b) :type t)))
         (fn [^Buf b v]
           (c/write-varint
             b (data/entry-id "consume_effect_type" (:type v)))
           ((:w (get consume-effects (:type v))) b v))))

(def ^:private c-patch
  (codec (fn [^Buf b] (read-patch b))
         (fn [^Buf b v] (write-patch b v))))

(def ^:private c-template
  (record-codec :item (c-reg "item") :count c-varint
                :patch c-patch))

(def ^:private c-typed-component
  (codec (fn [^Buf b]
           (let [i (c/read-varint b)
                 t (data/entry-name "data_component_type" i)]
             [t ((:r (get components t)) b)]))
         (fn [^Buf b [t v]]
           (c/write-varint b (data/entry-id "data_component_type" t))
           ((:w (get components t)) b v))))

(def ^:private no-predicates
  "component predicates not supported")

(def ^:private c-no-predicates
  (codec (fn [^Buf b]
           (let [n (c/read-varint b)]
             (when (pos? n)
               (throw (ex-info no-predicates {:count n})))
             []))
         (fn [^Buf b v]
           (when (seq v) (throw (ex-info no-predicates {})))
           (c/write-varint b 0))))

(def ^:private c-state-range
  (record-codec :min (c-opt c-string) :max (c-opt c-string)))

(def ^:private c-state-matcher
  (c-either (record-codec :value c-string) c-state-range))

(def ^:private c-state-entry
  (record-codec :name c-string :matcher c-state-matcher))

(def ^:private c-block-predicate
  (record-codec :blocks (c-opt (c-holder-set "block"))
                :state (c-opt (c-list c-state-entry))
                :nbt (c-opt c-nbt)
                :exact (c-list c-typed-component)
                :partial c-no-predicates))

(def ^:private c-adventure
  (record-codec :predicates (c-list c-block-predicate)))

(def ^:private c-attribute-display
  (let [types {0 (record-codec) 1 (record-codec)
               2 (record-codec :text c-text)}
        rd (fn [^Buf b]
             (let [t (c/read-varint b)]
               (assoc ((:r (get types t)) b) :display t)))
        wr (fn [^Buf b v]
             (let [t (long (:display v 0))]
               (c/write-varint b t)
               ((:w (get types t)) b v)))]
    (codec rd wr)))

(def ^:private attribute-operations
  [:add-value :add-multiplied-base :add-multiplied-total])

(def ^:private attribute-slots
  [:any :mainhand :offhand :hand :feet :legs :chest :head :armor
   :body :saddle])

(def ^:private c-attribute-modifier
  (record-codec :id c-ident :amount c-double
                :operation (c-enum attribute-operations)))

(def ^:private c-attribute-entry
  (record-codec :attribute (c-reg "attribute")
                :modifier c-attribute-modifier
                :slot (c-enum attribute-slots)
                :display c-attribute-display))

(def ^:private c-tool-rule
  (record-codec :blocks (c-holder-set "block") :speed (c-opt c-float)
                :correct-for-drops (c-opt c-bool)))

(def ^:private c-damage-reduction
  (record-codec :horizontal-blocking-angle c-float
                :type (c-opt (c-holder-set "damage_type"))
                :base c-float :factor c-float))

(def ^:private c-kinetic-condition
  (record-codec :max-duration-ticks c-varint :min-speed c-float
                :min-relative-speed c-float))

(def ^:private firework-shapes
  [:small-ball :large-ball :star :creeper :burst])

(def ^:private c-firework-explosion
  (record-codec :shape (c-enum firework-shapes)
                :colors (c-list c-int) :fade-colors (c-list c-int)
                :trail c-bool :twinkle c-bool))

(def ^:private c-game-profile-properties
  (c-list (record-codec :name c-string :value c-string
                        :signature (c-opt c-string))))

(def ^:private c-named-profile
  (record-codec :id c-uuid :name c-string
                :properties c-game-profile-properties))

(def ^:private c-partial-profile
  (record-codec :name (c-opt c-string) :id (c-opt c-uuid)
                :properties c-game-profile-properties))

(def ^:private c-skin
  (record-codec :body (c-opt c-ident) :cape (c-opt c-ident)
                :elytra (c-opt c-ident)
                :model (c-opt (c-enum [:wide :slim]))))

(def ^:private c-profile
  (record-codec :profile (c-either c-named-profile c-partial-profile)
                :skin c-skin))

(def ^:private c-typed-entity-data
  (fn [type-codec] (record-codec :type type-codec :data c-nbt)))

(def ^:private c-instrument-data
  (record-codec :sound c-sound :use-duration c-float
                :range c-float :description c-text))

(def ^:private c-instrument (c-holder "instrument" c-instrument-data))

(def ^:private c-material-assets
  (record-codec :base c-string
                :overrides (c-map c-ident c-string)))

(def ^:private c-trim-material-data
  (record-codec :assets c-material-assets :description c-text))

(def ^:private c-trim-material
  (c-holder "trim_material" c-trim-material-data))

(def ^:private c-trim-pattern-data
  (record-codec :asset c-ident :description c-text :decal c-bool))

(def ^:private c-trim-pattern
  (c-holder "trim_pattern" c-trim-pattern-data))

(def ^:private c-banner-pattern-data
  (record-codec :asset c-ident :translation-key c-string))

(def ^:private c-banner-pattern
  (c-holder "banner_pattern" c-banner-pattern-data))

(def ^:private c-jukebox-song-data
  (record-codec :sound c-sound :description c-text
                :length c-float :comparator-output c-varint))

(def ^:private c-jukebox-song
  (c-holder "jukebox_song" c-jukebox-song-data))

(def ^:private c-painting-variant-data
  (record-codec :width c-varint :height c-varint :asset c-ident
                :title (c-opt c-text) :author (c-opt c-text)))

(def ^:private c-painting-variant
  (c-holder "painting_variant" c-painting-variant-data))

(defn write-painting-variant
  "Writes a painting variant, by reference or in full."
  [^Buf buf v]
  ((:w c-painting-variant) buf v))

(defn read-painting-variant
  [^Buf buf]
  ((:r c-painting-variant) buf))

(def ^:private c-use-effects
  (record-codec :can-sprint c-bool :interact-vibrations c-bool
                :speed-multiplier c-float))

(def ^:private c-custom-model-data
  (record-codec :floats (c-list c-float) :flags (c-list c-bool)
                :strings (c-list c-string) :colors (c-list c-int)))

(def ^:private c-tooltip-display
  (record-codec :hide-tooltip c-bool
                :hidden (c-list (c-reg "data_component_type"))))

(def ^:private c-food
  (record-codec :nutrition c-varint :saturation c-float
                :can-always-eat c-bool))

(def ^:private consume-animations
  [:none :eat :drink :block :bow :trident :crossbow :spyglass
   :toot-horn :brush :bundle :spear])

(def ^:private c-consumable
  (record-codec :seconds c-float
                :animation (c-enum consume-animations)
                :sound c-sound :particles c-bool
                :on-consume (c-list c-consume-effect)))

(def ^:private c-tool
  (record-codec :rules (c-list c-tool-rule)
                :default-mining-speed c-float
                :damage-per-block c-varint
                :destroy-in-creative c-bool))

(def ^:private c-weapon
  (record-codec :damage-per-attack c-varint
                :disable-blocking-seconds c-float))

(def ^:private c-attack-range
  (record-codec :min-reach c-float :max-reach c-float
                :min-creative-reach c-float
                :max-creative-reach c-float
                :hitbox-margin c-float :mob-factor c-float))

(def ^:private equipment-slots
  [:mainhand :feet :legs :chest :head :offhand :body :saddle])

(def ^:private c-equippable
  (record-codec :slot (c-enum equipment-slots)
                :equip-sound c-sound
                :asset (c-opt c-ident)
                :camera-overlay (c-opt c-ident)
                :allowed-entities (c-opt (c-holder-set "entity_type"))
                :dispensable c-bool :swappable c-bool
                :damage-on-hurt c-bool :equip-on-interact c-bool
                :can-be-sheared c-bool :shearing-sound c-sound))

(def ^:private c-item-damage
  (record-codec :threshold c-float :base c-float :factor c-float))

(def ^:private c-blocks-attacks
  (record-codec :block-delay-seconds c-float
                :disable-cooldown-scale c-float
                :damage-reductions (c-list c-damage-reduction)
                :item-damage c-item-damage
                :bypassed-by (c-opt (c-holder-set "damage_type"))
                :block-sound (c-opt c-sound)
                :disable-sound (c-opt c-sound)))

(def ^:private c-piercing-weapon
  (record-codec :deals-knockback c-bool :dismounts c-bool
                :sound (c-opt c-sound) :hit-sound (c-opt c-sound)))

(def ^:private c-kinetic-weapon
  (record-codec :contact-cooldown-ticks c-varint
                :delay-ticks c-varint
                :dismount (c-opt c-kinetic-condition)
                :knockback (c-opt c-kinetic-condition)
                :damage (c-opt c-kinetic-condition)
                :forward-movement c-float :damage-multiplier c-float
                :sound (c-opt c-sound) :hit-sound (c-opt c-sound)))

(def ^:private c-swing-animation
  (record-codec :type (c-enum [:none :whack :stab])
                :duration c-varint))

(def ^:private c-potion-contents
  (record-codec :potion (c-opt (c-reg "potion"))
                :custom-color (c-opt c-int)
                :custom-effects (c-list c-effect-instance)
                :custom-name (c-opt c-string)))

(def ^:private c-stew-effect
  (record-codec :effect (c-reg "mob_effect") :duration c-varint))

(def ^:private c-written-book
  (record-codec :title (c-filterable c-string) :author c-string
                :generation c-varint
                :pages (c-list (c-filterable c-text))
                :resolved c-bool))

(def ^:private c-lodestone-target
  (record-codec :dimension c-ident :pos c-block-pos))

(def ^:private c-lodestone-tracker
  (record-codec :target (c-opt c-lodestone-target) :tracked c-bool))

(def ^:private c-fireworks
  (record-codec :flight-duration c-varint
                :explosions (c-list c-firework-explosion)))

(def ^:private c-bee
  (record-codec :data (c-typed-entity-data (c-reg "entity_type"))
                :ticks-in-hive c-varint
                :min-ticks-in-hive c-varint))

(def ^:private c-use-cooldown
  (record-codec :seconds c-float :group (c-opt c-ident)))

(def ^:private c-damage-resistant
  (record-codec :types (c-holder-set "damage_type")))

(def ^:private c-death-protection
  (record-codec :death-effects (c-list c-consume-effect)))

(def ^:private c-trim
  (record-codec :material c-trim-material :pattern c-trim-pattern))

(def ^:private c-block-entity-data
  (c-typed-entity-data (c-reg "block_entity_type")))

(def ^:private c-banner-patterns
  (c-list (record-codec :pattern c-banner-pattern :color c-dye)))

(def components
  {:custom-data                 c-nbt
   :max-stack-size c-varint
   :max-damage c-varint
   :damage c-varint
   :unbreakable c-unit
   :use-effects c-use-effects
   :custom-name c-text
   :minimum-attack-charge c-float
   :damage-type (c-reg "damage_type")
   :item-name c-text
   :item-model c-ident
   :lore (c-list c-text)
   :rarity (c-enum [:common :uncommon :rare :epic])
   :enchantments (c-map (c-reg "enchantment") c-varint)
   :can-place-on c-adventure
   :can-break c-adventure
   :attribute-modifiers (c-list c-attribute-entry)
   :custom-model-data c-custom-model-data
   :tooltip-display c-tooltip-display
   :repair-cost c-varint
   :creative-slot-lock c-unit
   :enchantment-glint-override c-bool
   :intangible-projectile c-nbt
   :food c-food
   :consumable c-consumable
   :use-remainder (record-codec :convert-into c-template)
   :use-cooldown c-use-cooldown
   :damage-resistant c-damage-resistant
   :tool c-tool
   :weapon c-weapon
   :attack-range c-attack-range
   :enchantable c-varint
   :equippable c-equippable
   :repairable (record-codec :items (c-holder-set "item"))
   :glider c-unit
   :tooltip-style c-ident
   :death-protection c-death-protection
   :blocks-attacks c-blocks-attacks
   :piercing-weapon c-piercing-weapon
   :kinetic-weapon c-kinetic-weapon
   :swing-animation c-swing-animation
   :additional-trade-cost c-varint
   :stored-enchantments (c-map (c-reg "enchantment") c-varint)
   :dye c-dye
   :dyed-color c-int
   :map-color c-int
   :map-id c-varint
   :map-decorations c-nbt
   :map-post-processing (c-enum [:lock :scale])
   :charged-projectiles (c-list c-template)
   :bundle-contents (c-list c-template)
   :potion-contents c-potion-contents
   :potion-duration-scale c-float
   :suspicious-stew-effects (c-list c-stew-effect)
   :writable-book-content (c-list (c-filterable c-string))
   :written-book-content c-written-book
   :trim c-trim
   :debug-stick-state c-nbt
   :entity-data (c-typed-entity-data (c-reg "entity_type"))
   :bucket-entity-data c-nbt
   :block-entity-data c-block-entity-data
   :instrument c-instrument
   :provides-trim-material c-trim-material
   :ominous-bottle-amplifier c-varint
   :jukebox-playable (record-codec :song c-jukebox-song)
   :provides-banner-patterns (c-holder-set "banner_pattern")
   :recipes c-nbt
   :lodestone-tracker c-lodestone-tracker
   :firework-explosion c-firework-explosion
   :fireworks c-fireworks
   :profile c-profile
   :note-block-sound c-ident
   :banner-patterns c-banner-patterns
   :base-color c-dye
   :pot-decorations (c-list (c-reg "item"))
   :container (c-list (c-opt c-template))
   :block-state (c-map c-string c-string)
   :bees (c-list c-bee)
   :sulfur-cube-content (record-codec :absorbed c-template)
   :lock c-nbt
   :container-loot c-nbt
   :break-sound c-sound
   :villager/variant (c-reg "villager_type")
   :wolf/variant (c-reg "wolf_variant")
   :wolf/sound-variant (c-reg "wolf_sound_variant")
   :wolf/collar c-dye
   :fox/variant c-varint
   :salmon/size c-varint
   :parrot/variant c-varint
   :tropical-fish/pattern c-varint
   :tropical-fish/base-color c-dye
   :tropical-fish/pattern-color c-dye
   :mooshroom/variant c-varint
   :rabbit/variant c-varint
   :pig/variant (c-reg "pig_variant")
   :pig/sound-variant (c-reg "pig_sound_variant")
   :cow/variant (c-reg "cow_variant")
   :cow/sound-variant (c-reg "cow_sound_variant")
   :chicken/variant (c-reg "chicken_variant")
   :chicken/sound-variant (c-reg "chicken_sound_variant")
   :zombie-nautilus/variant (c-reg "zombie_nautilus_variant")
   :frog/variant (c-reg "frog_variant")
   :horse/variant c-varint
   :painting/variant c-painting-variant
   :llama/variant c-varint
   :axolotl/variant c-varint
   :cat/variant (c-reg "cat_variant")
   :cat/sound-variant (c-reg "cat_sound_variant")
   :cat/collar c-dye
   :sheep/color c-dye
   :shulker/color c-dye})

(defn- component-codec [kw]
  (or (get components kw)
      (throw (ex-info "no codec for data component"
                      {:component kw}))))

(defn- read-component [^Buf buf delimited?]
  (let [k (data/entry-name "data_component_type" (c/read-varint buf))]
    (when delimited? (c/read-varint buf))
    [k ((:r (component-codec k)) buf)]))

(defn- read-removed [^Buf buf]
  (data/entry-name "data_component_type" (c/read-varint buf)))

(defn- read-changes [^Buf buf delimited? added removed]
  (let [cs (mapv (fn [_] (read-component buf delimited?))
                 (range added))
        rs (mapv (fn [_] (read-removed buf)) (range removed))
        m (apply array-map (apply concat cs))]
    (cond-> {}
            (seq cs) (assoc :components m)
            (seq rs) (assoc :removed (set rs)))))

(defn read-patch
  "Returns the changes to the default components of an item, or nil
  when there are none."
  ([^Buf buf] (read-patch buf false))
  ([^Buf buf delimited?]
   (let [added (c/read-count buf)
         removed (c/read-count buf)]
     (when-not (and (zero? added) (zero? removed))
       (read-changes buf delimited? added removed)))))

(defn write-patch
  "Writes the changes to the default components of an item."
  [^Buf buf patch]
  (let [cs (:components patch)
        id-of #(data/entry-id "data_component_type" %)
        rs (sort-by id-of (:removed patch))]
    (c/write-varint buf (count cs))
    (c/write-varint buf (count rs))
    (doseq [[k v] cs]
      (c/write-varint buf (data/entry-id "data_component_type" k))
      ((:w (component-codec k)) buf v))
    (doseq [k rs] (c/write-varint buf (id-of k)))))

(defn write-item-stack
  "Writes a stack of items, nil as the empty stack."
  [^Buf buf stack]
  (if (nil? stack)
    (c/write-varint buf 0)
    (do (c/write-varint buf (long (:count stack 1)))
        (c/write-varint buf (data/registry-id "item" (:item stack)))
        (write-patch buf stack))))

(defn read-item-stack
  "Returns the item stack at the read point, or nil.
  With delimited? each component value follows its length."
  ([^Buf buf] (read-item-stack buf false))
  ([^Buf buf delimited?]
   (let [n (c/read-varint buf)]
     (when (pos? n)
       (let [item (data/entry-name "item" (c/read-varint buf))]
         (merge {:item item :count n}
                (read-patch buf delimited?)))))))

(defn read-hashed-stack
  "Returns the stack a client reports in a slot, or nil.
  The client sends hashes of components, so the result only tells
  whether components are present."
  [^Buf buf]
  (when (buf/read-boolean buf)
    (let [item (c/read-varint buf)
          n (c/read-varint buf)
          added (c/read-count buf)]
      (dotimes [_ added] (c/read-varint buf) (buf/read-int buf))
      (let [removed (c/read-count buf)]
        (dotimes [_ removed] (c/read-varint buf))
        (cond-> {:item (data/entry-name "item" item) :count n}
                (or (pos? (long added)) (pos? (long removed)))
                (assoc :components? true))))))

(defn- particle-entry [[k id]]
  [(long id) [k (particles/kind k)]])

(def ^:private ^:table particle-kinds
  (delay (into {} (map particle-entry)
               (get (data/registries) "particle_type"))))

(defn- particle-kind [t]
  (let [[k kind] (@particle-kinds (long t))]
    (when-not k
      (throw (ex-info "unknown particle type" {:particle t})))
    kind))

(defn- shape-error [t opts]
  (ex-info "particle options do not fit the type"
           {:particle t :options opts}))

(defn- tuple [& cs]
  (codec (fn [^Buf b] (mapv #((:r %) b) cs))
         (fn [^Buf b v]
           (when-not (and (sequential? v) (= (count cs) (count v)))
             (throw (IllegalArgumentException.)))
           (dorun (map #((:w %1) b %2) cs v)))))

(def ^:private c-vec3
  (codec c/read-vec3 (fn [^Buf b v] (c/write-vec3 b v))))

(def ^:private c-source
  (codec
    (fn [^Buf b]
      (case (data/entry-name "position_source_type" (c/read-varint b))
        :block [:block ((:r c-block-pos) b)]
        :entity [:entity (c/read-varint b) (buf/read-float b)]))
    (fn [^Buf b [how a off]]
      (c/write-varint b (data/entry-id "position_source_type" how))
      (case how
        :block ((:w c-block-pos) b a)
        :entity (do (c/write-varint b (long a))
                    (buf/write-float! b (float off)))))))

(def ^:private particle-codecs
  {:state c-varint :color c-int :power c-float :roll c-float
   :delay c-varint :geyser c-int :item c-template
   :spell (tuple c-int c-float) :dust (tuple c-int c-float)
   :transition (tuple c-int c-int c-float)
   :geyser-base (tuple c-int c-float)
   :trail (tuple c-vec3 c-int c-varint)
   :vibration (tuple c-source c-varint)})

(defn write-particle
  "Writes a particle as [type options].
  The type decides the shape of the options."
  [^Buf buf [t opts]]
  (c/write-varint buf (long t))
  (when-let [cd (particle-codecs (particle-kind t))]
    (try ((:w cd) buf opts)
         (catch RuntimeException _ (throw (shape-error t opts))))))

(defn read-particle
  "Returns the particle at the read point as [type options]."
  [^Buf buf]
  (let [t (c/read-varint buf)]
    [t (when-let [cd (particle-codecs (particle-kind t))]
         ((:r cd) buf))]))
