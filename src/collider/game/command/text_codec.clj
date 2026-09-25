(ns collider.game.command.text-codec
  "Texts in commands as ComponentSerialization.CODEC reads them."
  (:require [clojure.string :as str]
            [collider.game.command.dfu :as dfu]
            [collider.proto.text :as text]))

(set! *warn-on-reflection* true)

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

(defn- shadow
  "Decodes an ARGB int. The form of four floats is not modelled."
  [tag]
  (if (number? tag) [:ok (long (unchecked-int tag))] [:raw tag]))

(defn- font [tag] ((dfu/mapped dfu/identifier dfu/full-id) tag))

(defn- unmodelled [tag] [:raw tag])

(def ^:private style-fields
  "Style.Serializer.MAP_CODEC in codec order: key in the tag, key
  in a text, decoder. Click and hover events are not modelled."
  [[:color :color text-color] [:shadow_color :shadow-color shadow]
   [:bold :bold dfu/bool-of] [:italic :italic dfu/bool-of]
   [:underlined :underlined dfu/bool-of]
   [:strikethrough :strikethrough dfu/bool-of]
   [:obfuscated :obfuscated dfu/bool-of]
   [:click_event :click unmodelled] [:hover_event :hover unmodelled]
   [:insertion :insertion dfu/string-of] [:font :font font]])

(def ^:private other-keys
  "Keys that other contents of the fuzzy codec need."
  #{:keybind :score :selector :nbt :object :sprite :player})

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
                       (vector? with) (dfu/list-of arg with)
                       (coll? with) [:raw m]
                       :else [:malformed ""])]
      (case op
        :ok [:ok (cond-> {:translate translate}
                   (string? fallback) (assoc :fallback fallback)
                   (seq v) (assoc :with v))]
        :raw [:raw m]
        nil))))

(defn- plain [m] (when (string? (:text m)) [:ok {:text (:text m)}]))

(def ^:private content-types
  #{"text" "translatable" "keybind" "score" "selector" "nbt"
    "object"})

(defn- fuzzy
  "Decodes contents with no type: the first codec that fits."
  [m]
  (or (plain m) (translatable m)
      (if (not-any? other-keys (keys m))
        [:malformed "No matching codec found"]
        [:raw m])))

(defn- contents
  "Decodes the contents of a text: by type, else the first codec
  that fits. Contents other than text and translation are raw."
  [m]
  (let [t (:type m)]
    (cond (nil? t) (fuzzy m)
          (not (string? t)) [:malformed "Not a string"]
          (not (content-types t))
          [:malformed (str "Unknown element id: " t)]
          (= "text" t) (or (plain m) [:raw m])
          (= "translatable" t) (or (translatable m) [:raw m])
          :else [:raw m])))

(defn- joined
  "Returns the text of contents and fields rs as one map, or its
  string when only the text is set."
  [rs]
  (let [c (into (second (first rs)) (keep second) (rest rs))]
    [:ok (if (text/plain? c) (:text c) c)]))

(defn- full
  "Decodes the map form of a text: contents, extra and style."
  [tag]
  (if (map? tag)
    (let [extra #(dfu/non-empty (dfu/list-of text %))
          rs (into [(contents tag)
                    (dfu/field tag [:extra :extra extra])]
                   (map #(dfu/field tag %)) style-fields)]
      (cond (dfu/raw-in? rs) [:raw tag]
            (some dfu/failed? rs) (dfu/errors rs)
            :else (joined rs)))
    (dfu/not-map tag)))

(defn- text
  "Decodes a text as ComponentSerialization.CODEC does."
  [tag]
  (let [[op v :as r] (dfu/non-empty (dfu/list-of text tag))
        listed (if (= :ok op) [:ok (from-list v)] r)]
    (dfu/either (dfu/either (dfu/string-of tag) listed)
                (full tag))))

(defn text-of
  "Decodes a text component value."
  [tag]
  (dfu/settled tag (text tag)))

(def lore
  "Decodes at most 256 lines of lore."
  (dfu/limited text 256))
