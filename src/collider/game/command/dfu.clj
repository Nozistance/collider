(ns collider.game.command.dfu
  "Decoders of tags as DataFixerUpper codecs read them in commands.
  A decoder takes a tag and returns [:ok value], [:malformed why]
  with the text of the codec error, [:partial why value] when the
  error keeps a partial result, or [:raw tag] when the check is not
  modelled. The error texts print tags as Tag.toString does."
  (:require [clojure.string :as str]
            [collider.data :as data])
  (:import (clojure.lang IPersistentMap IPersistentVector)
           (java.util HashMap)))

(set! *warn-on-reflection* true)

(def ^:private suffixes
  {Byte "b" Short "s" Integer "" Long "L" Float "f" Double "d"})

(defn- control-escape
  "Returns the escape of char c as SnbtGrammar gives it, or nil."
  [c]
  (case c
    \backspace "b" \tab "t" \newline "n" \formfeed "f" \return "r"
    (when (< (int c) 32) (format "x%02X" (int c)))))

(defn- quote-char [s]
  (case (some #{\" \'} s) \" \' \"))

(defn- escaped [q c]
  (let [e (control-escape c)]
    (cond (= c \\) "\\\\"
          (= c q) (str "\\" c)
          e (str "\\" e)
          :else (str c))))

(defn quoted
  "Returns string s quoted and escaped as StringTag.quoteAndEscape
  does: the quote is the one of the two that s does not start with."
  [s]
  (let [q (quote-char s)]
    (str q (apply str (map #(escaped q %) s)) q)))

(defn- plain-key? [^String k]
  (and (not (#{"true" "false"} (str/lower-case k)))
       (re-matches #"[A-Za-z._]+[A-Za-z0-9._+-]*" k)))

(declare printed)

(defn- key-str [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn- entry-str [[k v]]
  (let [k (key-str k)]
    (str (if (plain-key? k) k (quoted k)) ":" (printed v))))

(defn- array-str [prefix sfx xs]
  (str "[" prefix ";" (str/join "," (map #(str % sfx) xs)) "]"))

(def ^:private array-classes
  (mapv #(Class/forName %) ["[B" "[I" "[J"]))

(defn- scalar-str [tag]
  (let [[b i l] array-classes]
    (condp instance? tag
      b (array-str "B" "B" tag)
      i (array-str "I" "" tag)
      l (array-str "L" "L" tag)
      (str tag (suffixes (class tag))))))

(defn printed
  "Returns tag as Tag.toString in 26.2 does (StringTagVisitor): keys
  of a compound in string order, numbers with their suffix."
  [tag]
  (condp instance? tag
    String (quoted tag)
    IPersistentMap
    (let [es (sort-by (comp key-str key) tag)]
      (str "{" (str/join "," (map entry-str es)) "}"))
    IPersistentVector
    (str "[" (str/join "," (map printed tag)) "]")
    (scalar-str tag)))

(defn not-a
  "Returns the error of a codec of what for tag."
  [what tag]
  [:malformed (str "Not a " what ": " (printed tag))])

(defn not-map
  "Returns the error a map codec gives for tag."
  [tag]
  (not-a "map" tag))

(defn op-of
  "Returns the kind of result r: :ok, :malformed, :partial or :raw."
  [r]
  (first r))

(defn failed?
  "Returns true when result r is an error, with or without a
  partial value."
  [r]
  (#{:malformed :partial} (op-of r)))

(defn raw-in?
  "Returns true when one of results rs is not modelled."
  [rs]
  (some #(= :raw (op-of %)) rs))

(defn- malformed? [r] (= :malformed (op-of r)))

(defn errors
  "Returns the error of results rs as DataResult joins them: the
  last one first. It keeps a partial result when each one does.
  The text is nil when one of them is not modelled."
  [rs]
  (let [fs (filter failed? rs)]
    [(if (some malformed? fs) :malformed :partial)
     (when (every? second fs)
       (str/join "; " (map second (reverse fs))))]))

(defn settled
  "Returns result r of tag as a decoder gives it: an error is
  malformed, or tag kept raw when its text is not modelled."
  [tag [op v :as r]]
  (cond (not (failed? r)) r
        (nil? v) [:raw tag]
        :else [:malformed v]))

(defn mapped
  "Returns decoder f with g applied to the value it decodes."
  [f g]
  (fn [tag]
    (let [[op v :as r] (f tag)] (if (= :ok op) [:ok (g v)] r))))

(defn- either-why [xm ym]
  (when (and xm ym)
    (str "Failed to parse either. First: " xm "; Second: " ym)))

(defn either
  "Returns the result of Codec.either over results x and y."
  [[xo xm :as x] [yo ym :as y]]
  (cond (#{:ok :raw} xo) x
        (#{:ok :raw} yo) y
        (= :partial xo) x
        (= :partial yo) y
        :else [:malformed (either-why xm ym)]))

(defn list-of
  "Decodes each element of list tag with f as ListCodec does. The
  partial result counts the elements that decode."
  [f tag]
  (if (vector? tag)
    (let [rs (mapv f tag)]
      (cond (raw-in? rs) [:raw tag]
            (not-any? failed? rs) [:ok (mapv second rs)]
            :else (conj (assoc (errors rs) 0 :partial)
                        (count (remove malformed? rs)))))
    (not-a "list" tag)))

(defn non-empty
  "Checks list result r as ExtraCodecs.nonEmptyList does."
  [[op v n :as r]]
  (let [why "List must have contents"]
    (cond (and (= :ok op) (empty? v)) [:malformed why]
          (and (= :partial op) (zero? (long n)))
          [:malformed (some-> v (str "; " why))]
          :else r)))

(defn listed
  "Returns the decoder of a list of what f decodes."
  [f]
  (fn [tag] (settled tag (list-of f tag))))

(defn field
  "Decodes optional field k of m with f to [out value]. An error
  keeps a partial result, as optionalFieldOf does."
  [m [k out f]]
  (if-some [tag (get m k)]
    (let [[op v :as r] (f tag)]
      (case op :ok [:ok [out v]] :raw r [:partial v]))
    [:ok nil]))

(defn- no-key [m k]
  [:malformed (str "No key " (name k) " in MapLike["
                   (printed m) "]")])

(defn- record-field
  "Decodes field [k out f how dflt] of map m to [out value]. How is
  :req for fieldOf, :opt for optionalFieldOf and :lenient for
  lenientOptionalFieldOf; a missing field gives dflt."
  [m [k out f how dflt]]
  (let [tag (get m k)
        [op v :as r] (when (some? tag) (f tag))]
    (cond (nil? tag) (if (= :req how)
                       (no-key m k)
                       [:ok (when (some? dflt) [out dflt])])
          (= :ok op) [:ok [out v]]
          (= :raw op) r
          (= :lenient how) [:ok (when (some? dflt) [out dflt])]
          (= :opt how) [:partial v]
          :else [:malformed v])))

(defn record
  "Returns the decoder of a RecordCodecBuilder codec over fields,
  each [key-in-tag key-out decoder how default]. The value is the
  map of the fields that have one."
  [& fields]
  (fn [tag]
    (if (map? tag)
      (let [rs (mapv #(record-field tag %) fields)]
        (cond (raw-in? rs) [:raw tag]
              (some failed? rs) (settled tag (errors rs))
              :else [:ok (into {} (keep second) rs)]))
      (not-map tag))))

(defn checked
  "Returns decoder f that also checks its value with ok?, else
  gives the error why of the value."
  [f ok? why]
  (fn [tag]
    (let [[op v :as r] (f tag)]
      (if (and (= :ok op) (not (ok? v))) [:malformed (why v)] r))))

(defn int-in
  "Returns the decoder of an int from lo to hi. The error is why
  followed by the value."
  [lo hi why]
  (fn [tag]
    (if (number? tag)
      (let [n (unchecked-int tag)]
        (if (<= lo n hi) [:ok n] [:malformed (str why n)]))
      [:malformed "Not a number"])))

(defn float-in
  "Returns the decoder of a float from lo to hi. The bounds compare
  as Float.compareTo does, so -0.0 is below 0.0. When open, lo is
  not in the range."
  ([lo hi why] (float-in lo hi why false))
  ([lo hi why open]
   (fn [tag]
     (if (number? tag)
       (let [f (unchecked-float tag)
             c (Float/compare f (float lo))]
         (if (and (if open (pos? c) (<= 0 c))
                  (<= (Float/compare f (float hi)) 0))
           [:ok (double f)]
           [:malformed (str why f)]))
       [:malformed "Not a number"]))))

(defn float-range
  "Returns the decoder of Codec.floatRange, the one of DFU."
  [lo hi]
  (let [why (str " outside of range [" (float lo) ":" (float hi) "]")
        f (float-in lo hi "")]
    (fn [tag]
      (let [[op v :as r] (f tag)]
        (if (and (= :malformed op) (not= "Not a number" v))
          [:malformed (str "Value " v why)]
          r)))))

(def non-negative
  (int-in 0 Integer/MAX_VALUE "Value must be non-negative: "))

(def positive-int
  (int-in 1 Integer/MAX_VALUE "Value must be positive: "))

(def non-negative-float
  (float-in 0 Float/MAX_VALUE "Value must be non-negative: "))

(def positive-float
  (float-in 0 Float/MAX_VALUE "Value must be positive: " true))

(defn bool-of
  "Decodes a boolean as a byte tag holds it: zero is false."
  [tag]
  (if (number? tag)
    [:ok (not (zero? (unchecked-byte tag)))]
    [:malformed "Not a number"]))

(defn unit-of
  "Decodes a unit component: any map, to true."
  [tag]
  (if (map? tag) [:ok true] (not-map tag)))

(defn tag-unit
  "Decodes a unit value: any map, to the empty map."
  [tag]
  (if (map? tag) [:ok {}] (not-map tag)))

(defn string-of [tag]
  (if (string? tag) [:ok tag] [:malformed "Not a string"]))

(defn id-value
  "Returns full resource location s as a stack holds it."
  [s]
  (let [k (data/kebab s)] (if (= s (data/wire k)) k s)))

(defn full-id
  "Returns id v as the full resource location string."
  [v]
  (if (keyword? v) (data/wire v) v))

(defn- bad-id [ns path]
  (cond (not (re-matches #"[a-z0-9_.-]*" ns))
        "Non [a-z0-9_.-] character in namespace of identifier:"
        (not (re-matches #"[a-z0-9/._-]*" path))
        "Non [a-z0-9/._-] character in path of location:"))

(defn parse-id
  "Returns [ns path] of string s as Identifier.parse splits it."
  [s]
  (let [i (str/index-of s \:)]
    [(if (and i (pos? (long i))) (subs s 0 i) "minecraft")
     (if i (subs s (inc (long i))) s)]))

(defn- bad-location [tag why ns path]
  (str "Not a valid resource location: " tag
       \space why \space ns ":" path))

(defn identifier
  "Decodes a resource location as Identifier.CODEC does."
  [tag]
  (if (string? tag)
    (let [[ns path] (parse-id tag)]
      (if-let [why (bad-id ns path)]
        [:malformed (bad-location tag why ns path)]
        [:ok (id-value (str ns ":" path))]))
    [:malformed "Not a string"]))

(defn named
  "Returns the decoder of StringRepresentable.fromEnum: a name out
  of the map by-name, to its value."
  [by-name]
  (fn [tag]
    (cond (not (string? tag)) [:malformed "Not a string"]
          (by-name tag) [:ok (by-name tag)]
          :else [:malformed (str "Unknown element name:" tag)])))

(defn enum-of
  "Returns the decoder of a name out of the keywords ks."
  [ks]
  (named (into {} (map (fn [k] [(data/snake k) k])) ks)))

(defn numbered
  "Returns the decoder of a name out of names, to its index."
  [& names]
  (named (zipmap names (range))))

(defn- keys-of [v]
  (if (and (string? v) (str/starts-with? v "minecraft:"))
    [v (keyword (str/replace (subs v 10) \_ \-))]
    [v]))

(defn reg-key
  "Returns the key of id v in registry, built in or from the
  datapack, or nil. A path with a slash is a keyword namespace."
  [registry v]
  (let [m (get (data/registries) registry)
        has? (or (when m (partial contains? m))
                 (set (get (data/datapack) registry)))]
    (some #(when (has? %) %) (keys-of v))))

(defn- no-element [v]
  [:malformed (str "Failed to get element " (full-id v))])

(defn fixed
  "Returns the decoder of RegistryFixedCodec over registry."
  [registry]
  (fn [tag]
    (let [[op v :as r] (identifier tag)]
      (cond (not= :ok op) r
            (reg-key registry v) [:ok (reg-key registry v)]
            :else (no-element v)))))

(defn- no-file-element [registry v]
  [:malformed (str "Failed to get element ResourceKey[minecraft:"
                   registry " / " (full-id v) "]")])

(defn from-file
  "Returns the decoder of RegistryFileCodec over registry. A tag
  that is not an identifier goes to direct, the inline value."
  [registry direct]
  (fn [tag]
    (let [[op v :as r] (identifier tag)]
      (cond (not= :ok op) (direct tag)
            (reg-key registry v) [:ok (reg-key registry v)]
            :else (no-file-element registry v)))))

(defn float-of
  "Decodes a number to the float it holds, as a double."
  [tag]
  (if (number? tag)
    [:ok (double (unchecked-float tag))]
    [:malformed "Not a number"]))

(defn int-of
  "Decodes a number to the int it holds, as a long."
  [tag]
  (if (number? tag)
    [:ok (long (unchecked-int tag))]
    [:malformed "Not a number"]))

(defn int-range
  "Returns the decoder of Codec.intRange, the one of DFU."
  [lo hi]
  (checked int-of #(<= lo % hi)
           #(str "Value " % " outside of range [" lo ":" hi "]")))

(defn- decoded-until
  "Decodes the elements of list tag with f until most of them give
  a value, as ListCodec skips the ones past its size."
  [f most tag]
  (reduce (fn [rs x]
            (if (<= most (count (remove malformed? rs)))
              (reduced rs)
              (conj rs (f x))))
          [] tag))

(defn- too-long [n most]
  [:malformed (str "List is too long: " n
                   ", expected range [0-" most "]")])

(defn limited
  "Returns the decoder of a list of at most most elements of f, as
  Codec.sizeLimitedListOf."
  [f most]
  (fn [tag]
    (let [n (when (vector? tag) (count tag))]
      (cond (not (and n (< (long most) (long n))))
            (settled tag (list-of f tag))
            (raw-in? (decoded-until f most tag)) [:raw tag]
            :else (too-long n most)))))

(defn- unknown-key [registry v]
  [:malformed (str "Unknown registry key in ResourceKey"
                   "[minecraft:root / minecraft:" registry
                   "]: " (full-id v))])

(defn by-name
  "Returns the decoder of Registry.byNameCodec over registry."
  [registry]
  (fn [tag]
    (let [[op v :as r] (identifier tag)]
      (cond (not= :ok op) r
            (reg-key registry v) [:ok (reg-key registry v)]
            :else (unknown-key registry v)))))

(defn- tag-key
  "Decodes TagKey.hashedCodec: #id, to the id."
  [tag]
  (cond (not (string? tag)) [:malformed "Not a string"]
        (str/starts-with? tag "#") (identifier (subs tag 1))
        :else [:malformed "Not a tag id"]))

(defn- known-tag [registry id]
  (let [[ns path] (parse-id (full-id id))]
    (if (and (= "minecraft" ns)
             (contains? (get (data/tags) registry) path))
      [:ok {:tag id}]
      [:malformed (str "Missing tag: '" ns ":" path "' in 'minecraft:"
                       registry "'")])))

(defn holder-set
  "Returns the decoder of HolderSetCodec over registry: a tag #id,
  a list of what element decodes, or one of them."
  [registry element]
  (fn [tag]
    (let [[op v :as r] (tag-key tag)]
      (if (= :ok op)
        (known-tag registry v)
        (->> ((mapped element vector) tag)
             (either (list-of element tag))
             (either r)
             (settled tag))))))

(defn- entry-of
  "Decodes one entry of an unbounded map to [key value]."
  [kf vf [k v]]
  (let [[ko kv :as kr] (kf k) [vo vv :as vr] (vf v)]
    (cond (raw-in? [kr vr]) [:raw v]
          (= :ok ko vo) [:ok [kv vv]]
          :else (errors [kr vr]))))

(defn- hash-order
  "Returns the entries of map m in the order a HashMap iterates
  when its keys are put one by one."
  [m]
  (let [hm (HashMap.)]
    (doseq [[k v] m] (HashMap/.put hm (key-str k) v))
    (seq hm)))

(defn- distinct-keys? [rs]
  (let [ks (map #(first (second %)) rs)]
    (or (empty? ks) (apply distinct? ks))))

(defn- missed [rs bad]
  [:malformed (str (second (errors rs))
                   " missed input: " (printed (into {} bad)))])

(defn- entries-map [rs] (apply array-map (mapcat second rs)))

(defn unbounded-map
  "Returns the decoder of Codec.unboundedMap: entries go in the
  order a HashMap of the keys gives. The error names the entries
  that fail as missed input."
  [kf vf]
  (fn [tag]
    (if (map? tag)
      (let [es (hash-order tag)
            rs (mapv #(entry-of kf vf %) es)
            bad (keep-indexed #(when (failed? %2) (nth es %1)) rs)]
        (cond (raw-in? rs) [:raw tag]
              (seq bad) (missed rs bad)
              (distinct-keys? rs) [:ok (entries-map rs)]
              :else [:raw tag]))
      (not-map tag))))
