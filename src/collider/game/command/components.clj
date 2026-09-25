(ns collider.game.command.components
  "Saved forms of item components as commands read them.
  A decoder takes a tag and returns [:ok value] with the value a
  stack holds, [:malformed why] with the text of the codec error,
  or [:raw tag] when the check is not modelled."
  (:require [clojure.string :as str]
            [collider.data :as data]))

(set! *warn-on-reflection* true)

(def ^:private suffixes
  {Byte "b" Short "s" Integer "" Long "L" Float "f" Double "d"})

(defn- plain? [s]
  (every? #(and (<= 32 (int %) 126) (not (#{\\ \" \'} %))) s))

(defn printed
  "Returns tag as SNBT for numbers and plain strings, else nil.
  Printing other tags is not modelled."
  [tag]
  (if-let [sfx (suffixes (class tag))]
    (str tag sfx)
    (when (and (string? tag) (plain? tag)) (str "\"" tag "\""))))

(defn not-map
  "Returns the error a map codec gives for tag, or tag kept raw."
  [tag]
  (if-let [p (printed tag)]
    [:malformed (str "Not a map: " p)]
    [:raw tag]))

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

(defn- enum-of
  "Returns the decoder of a name out of the keywords ks."
  [ks]
  (let [by-name (into {} (map (fn [k] [(data/snake k) k])) ks)]
    (fn [tag]
      (cond (not (string? tag)) [:malformed "Not a string"]
            (by-name tag) [:ok (by-name tag)]
            :else [:malformed (str "Unknown element name:" tag)]))))

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
     :tropical-fish/base-color dye :tropical-fish/pattern-color dye}))

(defn decode
  "Returns the decoded value of component k from tag."
  [k tag]
  (if-let [f (decoders k)] (f tag) [:raw tag]))
