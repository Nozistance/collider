(ns collider.game.command.components
  "Saved forms of item components as commands read them.
  A decoder takes a tag and returns [:ok value] with the value a
  stack holds, [:malformed why] with the text of the codec error,
  or [:raw tag] when the check is not modelled."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.proto.text :as text]))

(set! *warn-on-reflection* true)

(def ^:private suffixes
  {Byte "b" Short "s" Integer "" Long "L" Float "f" Double "d"})

(defn- plain? [s]
  (every? #(and (<= 32 (int %) 126) (not (#{\\ \" \'} %))) s))

(defn printed
  "Returns tag as SNBT for numbers, plain strings and lists of them,
  else nil. Printing other tags is not modelled."
  [tag]
  (if-let [sfx (suffixes (class tag))]
    (str tag sfx)
    (cond (and (string? tag) (plain? tag)) (str "\"" tag "\"")
          (vector? tag) (let [ps (map printed tag)]
                          (when (every? some? ps)
                            (str "[" (str/join "," ps) "]"))))))

(defn- not-a
  "Returns the error of a codec of what for tag, or tag kept raw."
  [what tag]
  (if-let [p (printed tag)]
    [:malformed (str "Not a " what ": " p)]
    [:raw tag]))

(defn not-map
  "Returns the error a map codec gives for tag, or tag kept raw."
  [tag]
  (not-a "map" tag))

(defn- int-in [lo hi why]
  (fn [tag]
    (if (number? tag)
      (let [n (unchecked-int tag)]
        (if (<= lo n hi) [:ok n] [:malformed (str why n)]))
      [:malformed "Not a number"])))

(defn- float-in
  "Returns the decoder of a float from lo to hi. The bounds compare
  as Float.compareTo does, so -0.0 is below 0.0."
  [lo hi why]
  (fn [tag]
    (if (number? tag)
      (let [f (unchecked-float tag)]
        (if (and (<= 0 (Float/compare f (float lo)))
                 (<= (Float/compare f (float hi)) 0))
          [:ok (double f)]
          [:malformed (str why f)]))
      [:malformed "Not a number"])))

(defn- bool-of [tag]
  (if (number? tag)
    [:ok (not (zero? (unchecked-byte tag)))]
    [:malformed "Not a number"]))

(defn- unit-of [tag] (if (map? tag) [:ok true] (not-map tag)))

(defn- tag-unit [tag] (if (map? tag) [:ok {}] (not-map tag)))

(defn- id-value
  "Returns full resource location s as a stack holds it."
  [s]
  (let [k (data/kebab s)] (if (= s (data/wire k)) k s)))

(defn- bad-id [s ns path]
  (cond (not (re-matches #"[a-z0-9_.-]*" ns))
        "Non [a-z0-9_.-] character in namespace of identifier: "
        (not (re-matches #"[a-z0-9/._-]*" path))
        "Non [a-z0-9/._-] character in path of location: "))

(defn- identifier
  "Decodes a resource location as Identifier.CODEC does."
  [tag]
  (if (string? tag)
    (let [i (str/index-of tag \:)
          ns (if (and i (pos? (long i))) (subs tag 0 i) "minecraft")
          path (if i (subs tag (inc (long i))) tag)]
      (if-let [why (bad-id tag ns path)]
        [:malformed (str "Not a valid resource location: " tag " "
                         why ns ":" path)]
        [:ok (id-value (str ns ":" path))]))
    [:malformed "Not a string"]))

(defn- named
  "Returns the decoder of StringRepresentable.fromEnum: a name out
  of the map by-name, to its value."
  [by-name]
  (fn [tag]
    (cond (not (string? tag)) [:malformed "Not a string"]
          (by-name tag) [:ok (by-name tag)]
          :else [:malformed (str "Unknown element name:" tag)])))

(defn- enum-of
  "Returns the decoder of a name out of the keywords ks."
  [ks]
  (named (into {} (map (fn [k] [(data/snake k) k])) ks)))

(defn- numbered
  "Returns the decoder of a name out of names, to its index."
  [& names]
  (named (zipmap names (range))))

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

;; Registry holders

(defn- entry?
  "Tells whether registry, built in or from the datapack, has k."
  [registry k]
  (if-let [m (get (data/registries) registry)]
    (contains? m k)
    (boolean (some #{k} (get (data/datapack) registry)))))

(defn- full-id [v] (if (keyword? v) (data/wire v) v))

(defn- fixed
  "Returns the decoder of RegistryFixedCodec over registry."
  [registry]
  (fn [tag]
    (let [[op v :as r] (identifier tag)]
      (cond (not= :ok op) r
            (entry? registry v) r
            :else [:malformed (str "Failed to get element "
                                   (full-id v))]))))

(defn- from-file
  "Returns the decoder of RegistryFileCodec over registry. A tag
  that is not an identifier goes to direct, the inline value."
  [registry direct]
  (fn [tag]
    (let [[op v :as r] (identifier tag)]
      (cond (not= :ok op) (direct tag)
            (entry? registry v) r
            :else [:malformed (str "Failed to get element ResourceKey"
                                   "[minecraft:" registry " / "
                                   (full-id v) "]")]))))

(defn- mapped
  "Returns decoder f with g applied to the value it decodes."
  [f g]
  (fn [tag]
    (let [[op v :as r] (f tag)] (if (= :ok op) [:ok (g v)] r))))

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
    (not-map tag)))

(defn- inline-raw
  "Keeps an inline value raw: its codec is not modelled."
  [tag]
  (if (map? tag) [:raw tag] (not-map tag)))

(def ^:private sound (from-file "sound_event" direct-sound))

;; Text

(def ^:private text-colors
  #{"black" "dark_blue" "dark_green" "dark_aqua" "dark_red"
    "dark_purple" "gold" "gray" "dark_gray" "blue" "green" "aqua"
    "red" "light_purple" "yellow" "white"})

(defn- hex-color [^String s]
  (try (let [v (Integer/parseInt (subs s 1) 16)]
         (if (<= 0 v 0xFFFFFF)
           [:ok (format "#%06X" v)]
           [:malformed (str "Color value out of range: " s)]))
       (catch NumberFormatException _
         [:malformed (str "Invalid color value: " s)])))

(defn- text-color
  "Decodes TextColor.CODEC: a color name or #RRGGBB."
  [tag]
  (cond (not (string? tag)) [:malformed "Not a string"]
        (str/starts-with? tag "#") (hex-color tag)
        (text-colors tag) [:ok tag]
        :else [:malformed (str "Invalid color name: " tag)]))

(defn- string-of [tag]
  (if (string? tag) [:ok tag] [:malformed "Not a string"]))

(defn- shadow
  "Decodes an ARGB int. The form of four floats is not modelled."
  [tag]
  (if (number? tag) [:ok (long (unchecked-int tag))] [:raw tag]))

(defn- font [tag] ((mapped identifier full-id) tag))

(defn- unmodelled [tag] [:raw tag])

(def ^:private style-fields
  "Style.Serializer.MAP_CODEC in codec order: key in the tag, key
  in a text, decoder. Click and hover events are not modelled."
  [[:color :color text-color] [:shadow_color :shadow-color shadow]
   [:bold :bold bool-of] [:italic :italic bool-of]
   [:underlined :underlined bool-of]
   [:strikethrough :strikethrough bool-of]
   [:obfuscated :obfuscated bool-of]
   [:click_event :click unmodelled] [:hover_event :hover unmodelled]
   [:insertion :insertion string-of] [:font :font font]])

(def ^:private own-keys
  (into #{:type :text :translate :fallback :with :extra}
        (map first) style-fields))

(defn- op-of [r] (first r))

(defn- failed? [r] (#{:malformed :partial} (op-of r)))

(defn- raw-in? [rs] (some #(= :raw (op-of %)) rs))

(defn- errors
  "Returns the error of results rs as DataResult joins them: the
  last one first. It keeps a partial result when each one does.
  The text is nil when one of them is not modelled."
  [rs]
  (let [fs (filter failed? rs)]
    [(if (some #(= :malformed (op-of %)) fs) :malformed :partial)
     (when (every? second fs)
       (str/join "; " (map second (reverse fs))))]))

(defn- not-a-text
  "Returns the error of a codec of what for tag, with no text when
  printing tag is not modelled."
  [what tag]
  [:malformed (some->> (printed tag) (str "Not a " what ": "))])

(defn- either
  "Returns the result of Codec.either over results x and y."
  [[xo xm :as x] [yo ym :as y]]
  (cond (#{:ok :raw} xo) x
        (#{:ok :raw} yo) y
        (= :partial xo) x
        (= :partial yo) y
        :else [:malformed (when (and xm ym)
                            (str "Failed to parse either. First: "
                                 xm "; Second: " ym))]))

(defn- list-of
  "Decodes each element of list tag with f as ListCodec does. The
  partial result counts the elements that decode."
  [f tag]
  (if (vector? tag)
    (let [rs (mapv f tag)]
      (cond (raw-in? rs) [:raw tag]
            (not-any? failed? rs) [:ok (mapv second rs)]
            :else (conj (assoc (errors rs) 0 :partial)
                        (count (remove #(= :malformed (op-of %))
                                       rs)))))
    (not-a-text "list" tag)))

(defn- non-empty
  "Checks list result r as ExtraCodecs.nonEmptyList does."
  [[op v n :as r]]
  (let [why "List must have contents"]
    (cond (and (= :ok op) (empty? v)) [:malformed why]
          (and (= :partial op) (zero? (long n)))
          [:malformed (some-> v (str "; " why))]
          :else r)))

(defn- settled
  "Returns result r of tag as a decoder gives it: an error is
  malformed, or tag kept raw when its text is not modelled."
  [tag [op v :as r]]
  (cond (not (failed? r)) r
        (nil? v) [:raw tag]
        :else [:malformed v]))

(declare text)

(defn- from-list
  "Joins texts as ComponentSerialization.createFromList: the first
  one gets the others as extra children."
  [[x & more]]
  (let [c (if (string? x) {:text x} x)]
    (if more (assoc c :extra (into (vec (:extra c)) more)) x)))

(defn- arg
  "Decodes an argument of a translation: a number or string kept as
  it is, else a text. Short and long numbers are not modelled."
  [x]
  (cond (some #(instance? % x) [String Byte Integer Float Double])
        [:ok x]
        (number? x) [:raw x]
        :else (text x)))

(defn- translatable
  "Decodes TranslatableContents, or nil when the tag does not fit."
  [{:keys [translate fallback with] :as m}]
  (when (string? translate)
    (let [[op v] (cond (nil? with) [:ok nil]
                       (vector? with) (list-of arg with)
                       (coll? with) [:raw m]
                       :else [:malformed ""])]
      (case op
        :ok [:ok (cond-> {:translate translate}
                   (string? fallback) (assoc :fallback fallback)
                   (seq v) (assoc :with v))]
        :raw [:raw m]
        nil))))

(defn- plain [m] (when (string? (:text m)) [:ok {:text (:text m)}]))

(defn- contents
  "Decodes the contents of a text: by type, else the first codec
  that fits. Contents other than text and translation are raw."
  [m]
  (let [t (:type m)]
    (cond (nil? t)
          (or (plain m) (translatable m)
              (if (every? own-keys (keys m))
                [:malformed "No matching codec found"]
                [:raw m]))
          (not (string? t)) [:malformed "Not a string"]
          (not (#{"text" "translatable" "keybind" "score" "selector"
                  "nbt" "object"} t))
          [:malformed (str "Unknown element id: " t)]
          :else (or ({"text" (plain m)
                      "translatable" (translatable m)} t)
                    [:raw m]))))

(defn- field
  "Decodes optional field k of m with f to [out value]. An error
  keeps a partial result, as optionalFieldOf does."
  [m [k out f]]
  (if-some [tag (get m k)]
    (let [[op v :as r] (f tag)]
      (case op :ok [:ok [out v]] :raw r [:partial v]))
    [:ok nil]))

(defn- full
  "Decodes the map form of a text: contents, extra and style."
  [tag]
  (if (map? tag)
    (let [extra #(non-empty (list-of text %))
          rs (into [(contents tag) (field tag [:extra :extra extra])]
                   (map #(field tag %)) style-fields)]
      (cond (raw-in? rs) [:raw tag]
            (some failed? rs) (errors rs)
            :else (let [c (into (second (first rs))
                                (keep second) (rest rs))]
                    [:ok (if (text/plain? c) (:text c) c)])))
    (not-a-text "map" tag)))

(defn- text
  "Decodes a text as ComponentSerialization.CODEC does."
  [tag]
  (let [[op v :as r] (non-empty (list-of text tag))]
    (either (either (string-of tag)
                    (if (= :ok op) [:ok (from-list v)] r))
            (full tag))))

(defn- text-of [tag] (settled tag (text tag)))

(defn- lore
  "Decodes at most 256 lines of lore. Errors of lines past that
  are not modelled."
  [tag]
  (let [r (settled tag (list-of text tag))
        n (when (vector? tag) (count tag))]
    (cond (not (and n (< 256 (long n)))) r
          (= :ok (op-of r))
          [:malformed (str "List is too long: " n
                           ", expected range [0-256]")]
          :else [:raw tag])))

(def ^:private positive (float-in Float/MIN_VALUE Float/MAX_VALUE ""))

(defn- direct-instrument
  "Decodes an inline Instrument."
  [tag]
  (if (map? tag)
    (all-ok tag [:sound :use-duration :range :description]
            [(sound (:sound_event tag)) (positive (:use_duration tag))
             (positive (:range tag)) (text-of (:description tag))])
    (not-map tag)))

(defn- channel ^long [x]
  (let [v (unchecked-float (* (double (unchecked-float x)) 255.0))]
    (bit-and 0xFF (long (Math/floor v)))))

(defn- rgb
  "Decodes RGB_COLOR_CODEC: an int, or three floats from 0 to 1.
  Errors of the float form are not modelled."
  [tag]
  (cond (number? tag) [:ok (long (unchecked-int tag))]
        (and (vector? tag) (= 3 (count tag)) (every? number? tag))
        [:ok (long (unchecked-int
                     (reduce #(bit-or (bit-shift-left %1 8)
                                      (channel %2))
                             0xFF tag)))]
        :else [:raw tag]))

(def ^:private dyes
  [:white :orange :magenta :light-blue :yellow :lime :pink :gray
   :light-gray :cyan :purple :blue :brown :green :red :black])

(def ^:private decoders
  "The saved forms of components commands check.
  Others are kept as the raw tag, unchecked."
  (let [n (int-in 0 Integer/MAX_VALUE "Value must be non-negative: ")
        p (int-in 1 Integer/MAX_VALUE "Value must be positive: ")
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
     (float-in 0 Float/MAX_VALUE "Value must be non-negative: ")
     :enchantment-glint-override bool-of
     :unbreakable unit-of :glider unit-of
     :intangible-projectile tag-unit
     :item-model identifier :tooltip-style identifier
     :note-block-sound identifier
     :rarity (enum-of [:common :uncommon :rare :epic])
     :dyed-color rgb
     :dye dye :base-color dye :wolf/collar dye :cat/collar dye
     :sheep/color dye :shulker/color dye
     :tropical-fish/base-color dye :tropical-fish/pattern-color dye
     :custom-name text-of :item-name text-of :lore lore
     :damage-type (fixed "damage_type")
     :instrument (from-file "instrument" direct-instrument)
     :provides-trim-material (from-file "trim_material" inline-raw)
     :jukebox-playable
     (mapped (fixed "jukebox_song") #(hash-map :song %))
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
     :horse/variant (numbered "white" "creamy" "chestnut" "brown"
                              "black" "gray" "dark_brown")
     :llama/variant (numbered "creamy" "white" "brown" "gray")
     :axolotl/variant
     (numbered "lucy" "wild" "gold" "cyan" "blue")}))

(defn decode
  "Returns the decoded value of component k from tag."
  [k tag]
  (if-let [f (decoders k)] (f tag) [:raw tag]))
