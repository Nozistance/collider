(ns collider.game.command.text
  "Text components in commands."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.decode :as dfu]
            [collider.proto.text :as text])
  (:import (java.net URI URISyntaxException)
           (java.util Locale UUID)))

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

(declare text)

(def ^:private web #{"http" "https"})

(defn- untrusted-uri ^URI [^String s]
  (let [u (URI. s) p (.getScheme u)]
    (cond (nil? p)
          (throw (URISyntaxException. s (str "Missing protocol in URI: " s)))
          (web (.toLowerCase p Locale/ROOT)) u
          :else
          (throw (URISyntaxException.
                   s (str "Unsupported protocol in URI: " s))))))

(defn- uri [tag]
  (if (string? tag)
    (try [:ok (str (untrusted-uri tag))]
         (catch URISyntaxException e [:malformed (.getMessage e)]))
    [:malformed "Not a string"]))

(defn- chat-char? [c]
  (let [n (int c)] (and (not= 167 n) (>= n 32) (not= 127 n))))

(defn- chat-string [tag]
  (let [[op v :as r] (dfu/string-of tag)
        bad (when (= :ok op) (first (remove chat-char? v)))]
    (if bad [:malformed (str "Disallowed chat character: '" bad "'")] r)))

(def ^:private positive
  (dfu/checked dfu/int-of pos? #(str "Value must be positive: " %)))

(defn- uuid-of-ints [[a b c d]]
  (UUID. (bit-or (bit-shift-left a 32) (bit-and b 0xffffffff))
         (bit-or (bit-shift-left c 32) (bit-and d 0xffffffff))))

(defn- uuid-string [tag]
  (if (string? tag)
    (try [:ok (UUID/fromString tag)]
         (catch IllegalArgumentException e
           [:malformed (str "Invalid UUID " tag ": " (.getMessage e))]))
    [:malformed "Not a string"]))

(defn- uuid [tag]
  (dfu/either ((dfu/mapped (dfu/ints-of 4) uuid-of-ints) tag)
              (uuid-string tag)))

(defn- command [] (dfu/record [:command :command chat-string :req]))

(def ^:private clicks
  {"open_url" (dfu/record [:url :url uri :req])
   "run_command" (command) "suggest_command" (command)
   "change_page" (dfu/record [:page :page positive :req])
   "copy_to_clipboard" (dfu/record [:value :value dfu/string-of :req])
   "custom" (dfu/record [:id :id dfu/identifier :req]
                        [:payload :payload #(vector :ok %) :opt])
   "open_file" :blocked "show_dialog" :raw})

(def ^:private hovers
  {"show_text" (dfu/record [:value :value #(text %) :req])
   "show_entity" (dfu/record [:id :id (dfu/by-name "entity_type") :req]
                             [:uuid :uuid uuid :req]
                             [:name :name #(text %) :opt])
   "show_item" :raw})

(def ^:private action (dfu/record [:action :action dfu/string-of :req]))

(defn- acted [actions kind tag a]
  (let [f (actions a)]
    (cond (nil? f) [:malformed (str "Unknown element name:" a)]
          (= :raw f) [:raw tag]
          (= :blocked f)
          [:malformed (str kind " event type not allowed: "
                           (str/upper-case a))]
          :else (let [[op v :as r] (f tag)]
                  (if (= :ok op)
                    [:ok (assoc v :action (data/kebab a))]
                    r)))))

(defn- event
  "Returns the decoder of an event of kind, one of actions by name."
  [actions kind]
  (fn [tag]
    (let [[op v :as r] (action tag)]
      (if (= :ok op) (acted actions kind tag (:action v)) r))))

(def ^:private style-fields
  "The style fields in decode order.
  Each is a tag key, a text key and a decoder."
  [[:color :color text-color] [:shadow_color :shadow-color shadow]
   [:bold :bold dfu/bool-of] [:italic :italic dfu/bool-of]
   [:underlined :underlined dfu/bool-of]
   [:strikethrough :strikethrough dfu/bool-of]
   [:obfuscated :obfuscated dfu/bool-of]
   [:click_event :click (event clicks "Click")]
   [:hover_event :hover (event hovers "Hover")]
   [:insertion :insertion dfu/string-of] [:font :font font]])

(def ^:private other-keys
  "The keys that other text contents need."
  #{:keybind :score :selector :nbt :object :sprite :player})

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

(defn- style [tag]
  (if (map? tag)
    (let [rs (mapv #(dfu/field tag %) style-fields)]
      (cond (dfu/raw-in? rs) [:raw tag]
            (some dfu/failed? rs) (dfu/errors rs)
            :else [:ok (into {} (keep second) rs)]))
    (dfu/not-map tag)))

(defn style-of
  "Decodes a text style value."
  [tag]
  (dfu/settled tag (style tag)))

(def lore
  "Decodes at most 256 lines of lore."
  (dfu/limited text 256))
