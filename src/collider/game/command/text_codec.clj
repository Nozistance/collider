(ns collider.game.command.text-codec
  "Text components in commands."
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

(defn- text-color [tag]
  (cond (not (string? tag)) [:malformed "Not a string"]
        (str/starts-with? tag "#") (hex-color tag)
        (text-colors tag) [:ok tag]
        :else [:malformed (str "Invalid color name: " tag)]))

(defn- shadow [tag]
  (if (number? tag) [:ok (long (unchecked-int tag))] [:raw tag]))

(defn- font [tag] ((dfu/mapped dfu/identifier dfu/full-id) tag))

(defn- unmodelled [tag] [:raw tag])

(def ^:private style-fields
  "The style fields in decode order.
  Each is a tag key, a text key and a decoder."
  [[:color :color text-color] [:shadow_color :shadow-color shadow]
   [:bold :bold dfu/bool-of] [:italic :italic dfu/bool-of]
   [:underlined :underlined dfu/bool-of]
   [:strikethrough :strikethrough dfu/bool-of]
   [:obfuscated :obfuscated dfu/bool-of]
   [:click_event :click unmodelled] [:hover_event :hover unmodelled]
   [:insertion :insertion dfu/string-of] [:font :font font]])

(def ^:private other-keys
  "The keys that other text contents need."
  #{:keybind :score :selector :nbt :object :sprite :player})

(declare text)

(defn- from-list
  [[x & more]]
  (let [c (if (string? x) {:text x} x)]
    (if more (assoc c :extra (into (vec (:extra c)) more)) x)))

(defn- arg [x]
  (cond (some #(instance? % x) [String Byte Integer Float Double])
        [:ok x]
        (number? x) [:raw x]
        :else (text x)))

(defn- translatable
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

(defn- fuzzy [m]
  (or (plain m) (translatable m)
      (if (not-any? other-keys (keys m))
        [:malformed "No matching codec found"]
        [:raw m])))

(defn- contents [m]
  (let [t (:type m)]
    (cond (nil? t) (fuzzy m)
          (not (string? t)) [:malformed "Not a string"]
          (not (content-types t))
          [:malformed (str "Unknown element id: " t)]
          (= "text" t) (or (plain m) [:raw m])
          (= "translatable" t) (or (translatable m) [:raw m])
          :else [:raw m])))

(defn- joined [rs]
  (let [c (into (second (first rs)) (keep second) (rest rs))]
    [:ok (if (text/plain? c) (:text c) c)]))

(defn- full [tag]
  (if (map? tag)
    (let [extra #(dfu/non-empty (dfu/list-of text %))
          rs (into [(contents tag)
                    (dfu/field tag [:extra :extra extra])]
                   (map #(dfu/field tag %)) style-fields)]
      (cond (dfu/raw-in? rs) [:raw tag]
            (some dfu/failed? rs) (dfu/errors rs)
            :else (joined rs)))
    (dfu/not-map tag)))

(defn- text [tag]
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
