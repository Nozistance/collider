(ns collider.game.command.snbt
  (:require [clojure.string :as str]
            [collider.game.command.reader :as r])
  (:import (java.util Objects UUID)))

(set! *warn-on-reflection* true)

(defn- why [k & args] {:key k :args (vec args)})

(defn- err [k & args] (apply why (str "snbt.parser." k) args))

(defn- mark ^long [st] @(:pos st))

(defn- restore! [st n] (vreset! (:pos st) (long n)) nil)

(defn- ch-at [st i] (nth (:s st) i))

(defn- skip-ws! [st]
  (restore! st (second (r/skip-whitespace [(:s st) (mark st)]))))

(defn- longest [{:keys [cursor reason] :as e} c w]
  (cond (> (long c) (long cursor)) {:cursor c :reason w}
        (and (= c cursor) (nil? reason)) (assoc e :reason w)
        :else e))

(defn- store! [st c w]
  (when-not (:silent? st) (vswap! (:err st) longest c w))
  nil)

(defn- chars-term [label pred]
  (let [w (why "argument.literal.incorrect" label)]
    (fn [st _ _]
      (skip-ws! st)
      (let [c (mark st) more? (< c (long (:len st)))
            ok (and more? (pred (ch-at st c)))]
        (when more? (restore! st (inc c)))
        (or (boolean ok) (do (store! st c w) false))))))

(defn- ch [c] (chars-term (str c) #(= c %)))

(defn- chs [a b] (chars-term (str a "|" b) #(or (= a %) (= b %))))

(defn- sq [& ts]
  (fn [st fr ctl]
    (let [m (mark st)]
      (or (every? #(% st fr ctl) ts) (do (restore! st m) false)))))

(defn- alt [& ts]
  (fn [st fr _]
    (let [ctl (volatile! false) m (mark st)]
      (loop [ts ts]
        (if (empty? ts)
          false
          (let [f (volatile! {})]
            (if ((first ts) st f ctl)
              (do (vswap! fr merge @f) true)
              (do (restore! st m)
                  (if @ctl false (recur (rest ts)))))))))))

(defn- cut [_ _ ctl] (when ctl (vreset! ctl true)) true)

(defn- opt [t]
  (fn [st fr ctl]
    (let [m (mark st)]
      (when-not (t st fr ctl) (restore! st m))
      true)))

(defn- as [k v] (fn [_ fr _] (vswap! fr assoc k v) true))

(defn- fail [w] (fn [st _ _] (store! st (mark st) w) false))

(defn- ahead [t]
  (fn [st fr ctl]
    (let [m (mark st) ok (t (assoc st :silent? true) fr ctl)]
      (restore! st m)
      ok)))

(defn- to [rule k]
  (fn [st fr _]
    (if-some [v (rule st)]
      (do (vswap! fr assoc k v) true)
      false)))

(defn- rule [t action]
  (fn [st]
    (let [fr (volatile! {})]
      (when (t st fr nil) (action st @fr)))))

(defn- many [rule k]
  (fn [st fr _]
    (loop [acc []]
      (let [m (mark st) v (rule st)]
        (if (some? v)
          (recur (conj acc v))
          (do (restore! st m) (vswap! fr assoc k acc) true))))))

(defn- sep-by [rule k sep]
  (fn [st fr ctl]
    (loop [acc [] first? true]
      (let [m (mark st)]
        (if (and (not first?) (not (sep st fr ctl)))
          (do (restore! st m) (vswap! fr assoc k acc) true)
          (let [m2 (mark st) v (rule st)]
            (if (some? v)
              (recur (conj acc v) false)
              (do (restore! st m2)
                  (vswap! fr assoc k acc)
                  true))))))))

(defn- scan ^long [st ^long from pred ^long most]
  (let [len (long (:len st))]
    (loop [i from]
      (if (and (< i len) (< (- i from) most) (pred (ch-at st i)))
        (recur (inc i))
        i))))

(defn- numeral [accept? none]
  (fn [st]
    (skip-ws! st)
    (let [a (mark st) b (scan st a accept? Long/MAX_VALUE)]
      (cond (= a b) (store! st a none)
            (or (= \_ (ch-at st a)) (= \_ (ch-at st (dec b))))
            (store! st a (err "underscore_not_allowed"))
            :else (do (restore! st b) (subs (:s st) a b))))))

(defn- greedy [lo hi pred w]
  (fn [st]
    (let [a (mark st) b (scan st a pred hi)]
      (if (< (- b a) (long lo))
        (store! st a w)
        (do (restore! st b) (subs (:s st) a b))))))

(defn- unquoted [st]
  (skip-ws! st)
  (let [a (mark st) [v [_ b]] (r/read-unquoted [(:s st) a])]
    (if (= "" v)
      (store! st a (err "expected_unquoted_string"))
      (do (restore! st b) v))))

(defn- in? [^String cs] (fn [c] (str/includes? cs (str c))))

(def ^:private digits "0123456789")

(def ^:private hex-digits "0123456789ABCDEFabcdef")

(def ^:private bin-rule
  (numeral (in? "01_") (err "expected_binary_numeral")))

(def ^:private dec-rule
  (numeral (in? (str digits "_")) (err "expected_decimal_numeral")))

(def ^:private hex-rule
  (numeral (in? (str hex-digits "_")) (err "expected_hex_numeral")))

(def ^:private sign-rule
  (rule (alt (sq (ch \+) (as :sign :plus))
             (sq (ch \-) (as :sign :minus)))
        (fn [_ f] (:sign f))))

(defn- typed-suffixes [signed]
  (for [[a b t] [[\b \B :byte] [\s \S :short]
                 [\i \I :int] [\l \L :long]]]
    (sq (chs a b) (as :suffix [signed t]))))

(def ^:private suffix-rule
  (rule (apply alt
               (sq (chs \u \U) (apply alt (typed-suffixes :unsigned)))
               (sq (chs \s \S) (apply alt (typed-suffixes :signed)))
               (typed-suffixes nil))
        (fn [_ f] (:suffix f))))

(def ^:private zero-led
  (alt (sq (chs \x \X) cut (to hex-rule :hex))
       (sq (chs \b \B) (to bin-rule :bin))
       (sq (to dec-rule :dec) cut
           (fail (err "leading_zero_not_allowed")))
       (as :dec "0")))

(defn- integer-literal [_ f]
  {:sign (:sign f :plus) :suffix (:suffix f [nil nil])
   :base (cond (:dec f) 10 (:hex f) 16 :else 2)
   :digits (or (:dec f) (:hex f) (:bin f))})

(def ^:private integer-rule
  (rule (sq (opt (to sign-rule :sign))
            (alt (sq (ch \0) cut zero-led) (to dec-rule :dec))
            (opt (to suffix-rule :suffix)))
        integer-literal))

(defn- signed? [{:keys [suffix base]}]
  (= :signed (or (first suffix) (if (= 10 base) :signed :unsigned))))

(defn- clean [s] (str/replace s "_" ""))

(defn- digits-of ^String [{:keys [sign digits]}]
  (str (when (= :minus sign) "-") (clean digits)))

(defn- out-of-range [p]
  (throw (NumberFormatException. (str "out of range: " p))))

(defn- unsigned-small [d r bits]
  (let [p (Integer/parseInt d (int r))]
    (if (zero? (bit-shift-right p (long bits))) p (out-of-range p))))

(defn- parse-signed [t ^String d r]
  (case t
    :byte (Byte/valueOf (Byte/parseByte d (int r)))
    :short (Short/valueOf (Short/parseShort d (int r)))
    :int (Integer/valueOf (Integer/parseInt d (int r)))
    :long (Long/valueOf (Long/parseLong d (int r)))))

(defn- parse-unsigned [t ^String d r]
  (case t
    :byte (Byte/valueOf (unchecked-byte (unsigned-small d r 8)))
    :short (Short/valueOf (unchecked-short (unsigned-small d r 16)))
    :int (Integer/valueOf (Integer/parseUnsignedInt d (int r)))
    :long (Long/valueOf (Long/parseUnsignedLong d (int r)))))

(defn- integer-value [st lit t]
  (let [s? (signed? lit)]
    (if (and (not s?) (= :minus (:sign lit)))
      (store! st (mark st) (err "expected_non_negative_number"))
      (try ((if s? parse-signed parse-unsigned)
            t (digits-of lit) (:base lit))
           (catch NumberFormatException e
             (store! st (mark st)
                     (err "number_parse_failure" (ex-message e))))))))

(def ^:private fsuffix-rule
  (rule (alt (sq (chs \f \F) (as :type :float))
             (sq (chs \d \D) (as :type :double)))
        (fn [_ f] (:type f))))

(def ^:private exp-rule
  (rule (sq (chs \e \E) (opt (to sign-rule :sign)) (to dec-rule :dec))
        (fn [_ f] [(:sign f :plus) (:dec f)])))

(defn- float-text [{:keys [sign whole fraction exp]}]
  (let [minus #(when (= :minus %) "-")]
    (str (minus sign) (some-> whole clean)
         (some->> fraction clean (str "."))
         (when-let [[es ed] exp] (str "e" (minus es) (clean ed))))))

(defn- finite [st v]
  (if (Double/isFinite (double v))
    v
    (store! st (mark st) (err "infinity_not_allowed"))))

(defn- float-literal [st f]
  (let [s (float-text f)]
    (if (= :float (:type f))
      (finite st (Float/valueOf (Float/parseFloat s)))
      (finite st (Double/valueOf (Double/parseDouble s))))))

(def ^:private float-rule
  (let [exp (opt (to exp-rule :exp)) typ (opt (to fsuffix-rule :type))
        whole (to dec-rule :whole)]
    (rule (sq (opt (to sign-rule :sign))
              (alt (sq whole (ch \.) cut
                       (opt (to dec-rule :fraction)) exp typ)
                   (sq (ch \.) cut (to dec-rule :fraction) exp typ)
                   (sq whole (to exp-rule :exp) cut typ)
                   (sq whole exp (to fsuffix-rule :type))))
          float-literal)))

(defn- hex-rule-of [n]
  (greedy n n (in? hex-digits) (err "expected_hex_escape" (str n))))

(def ^:private letters "abcdefghijklmnopqrstuvwxyz")

(def ^:private name-rule
  (greedy 1 Long/MAX_VALUE
          (in? (str "- " digits letters (str/upper-case letters)))
          (err "invalid_character_name")))

(def ^:private plain-escapes
  [[\b "\b"] [\s " "] [\t "\t"] [\n "\n"] [\f "\f"] [\r "\r"]
   [\\ "\\"] [\' "'"] [\" "\""]])

(defn- codepoint [st h]
  (let [cp (unchecked-int (Long/parseLong h 16))]
    (if (Character/isValidCodePoint cp)
      (Character/toString cp)
      (store! st (mark st)
              (err "invalid_codepoint"
                   (format "U+%08X" (Integer/valueOf cp)))))))

(defn- named-char [st ^String n]
  (try (Character/toString (Character/codePointOf n))
       (catch IllegalArgumentException _
         (store! st (mark st) (err "invalid_character_name")))))

(defn- escape-value [st f]
  (cond (:esc f) (:esc f)
        (:hex f) (codepoint st (:hex f))
        :else (named-char st (:name f))))

(def ^:private escape-rule
  (rule (apply alt
               (concat
                (for [[c v] plain-escapes] (sq (ch c) (as :esc v)))
                [(sq (ch \x) (to (hex-rule-of 2) :hex))
                 (sq (ch \u) (to (hex-rule-of 4) :hex))
                 (sq (ch \U) (to (hex-rule-of 8) :hex))
                 (sq (ch \N) (ch \{) (to name-rule :name) (ch \}))]))
        escape-value))

(def ^:private plain-rule
  (greedy 1 Long/MAX_VALUE #(not (#{\" \' \\} %))
          (err "invalid_string_contents")))

(defn- chunk-rule [other]
  (rule (alt (to plain-rule :c)
             (sq (ch \\) (to escape-rule :c))
             (sq (ch other) (as :c (str other))))
        (fn [_ f] (:c f))))

(defn- contents-rule [other]
  (rule (many (chunk-rule other) :chunks)
        (fn [_ f] (str/join (:chunks f)))))

(def ^:private quoted-rule
  (rule (alt (sq (ch \") cut (opt (to (contents-rule \') :c)) (ch \"))
             (sq (ch \') (opt (to (contents-rule \") :c)) (ch \')))
        (fn [_ f] (:c f))))

(declare literal)

(def ^:private args-rule
  (rule (sep-by #'literal :args (ch \,)) (fn [_ f] (:args f))))

(defn- bool-op [st [a]]
  (if (number? a)
    (Byte/valueOf (byte (if (== 0.0 (double a)) 0 1)))
    (store! st (mark st) (err "expected_number_or_boolean"))))

(defn- uuid-ints [^UUID u]
  (let [hi (UUID/.getMostSignificantBits u)
        lo (UUID/.getLeastSignificantBits u)]
    (int-array (map unchecked-int
                    [(bit-shift-right hi 32) hi
                     (bit-shift-right lo 32) lo]))))

(defn- uuid-op [st [a]]
  (or (when (string? a)
        (try (uuid-ints (UUID/fromString a))
             (catch IllegalArgumentException _ nil)))
      (store! st (mark st) (err "expected_string_uuid"))))

(def ^:private operations {["bool" 1] bool-op ["uuid" 1] uuid-op})

(def ^:private number-start? (in? "+-.0123456789"))

(defn- builtin [st {s :str args :args}]
  (cond (number-start? (first s))
        (store! st (mark st) (err "invalid_unquoted_start"))
        args (if-let [op (operations [s (count args)])]
               (op st args)
               (let [k (str s "/" (count args))]
                 (store! st (mark st) (err "no_such_operation" k))))
        :else (case (str/lower-case s)
                "true" (Byte/valueOf (byte 1))
                "false" (Byte/valueOf (byte 0))
                s)))

(def ^:private builtin-rule
  (rule (sq (to unquoted :str)
            (opt (sq (ch \() (to args-rule :args) (ch \)))))
        builtin))

(def ^:private key-rule
  (rule (alt (to quoted-rule :k) (to unquoted :k)) (fn [_ f] (:k f))))

(defn- map-entry [st {:keys [k v]}]
  (if (= "" k)
    (store! st (mark st) (err "empty_key"))
    [(keyword k) v]))

(def ^:private entry-rule
  (rule (sq (to key-rule :k) (ch \:) (to #'literal :v)) map-entry))

(defn- comma-list [element]
  (rule (sep-by element :es (ch \,)) (fn [_ f] (:es f))))

(def ^:private map-rule
  (rule (sq (ch \{) (to (comma-list entry-rule) :entries) (ch \}))
        (fn [_ f] (into {} (:entries f)))))

(def ^:private arrays
  {:byte {:default :byte :allowed #{:byte} :make byte-array
          :cast unchecked-byte}
   :int {:default :int :allowed #{:int :byte :short} :make int-array
         :cast unchecked-int}
   :long {:default :long :allowed #{:long :byte :short :int}
          :make long-array :cast long}})

(defn- element [st {:keys [default allowed cast]} lit]
  (let [t (or (second (:suffix lit)) default)]
    (if (allowed t)
      (some-> (integer-value st lit t) cast)
      (store! st (mark st) (err "invalid_array_element_type")))))

(defn- build-array [st prefix entries]
  (let [spec (arrays prefix)]
    (loop [es entries acc []]
      (if (empty? es)
        ((:make spec) acc)
        (when-some [v (element st spec (first es))]
          (recur (rest es) (conj acc v)))))))

(def ^:private prefix-rule
  (rule (alt (sq (ch \B) (as :p :byte)) (sq (ch \L) (as :p :long))
             (sq (ch \I) (as :p :int)))
        (fn [_ f] (:p f))))

(defn- list-value [st f]
  (if-some [p (:prefix f)]
    (build-array st p (:ints f))
    (:items f)))

(def ^:private list-rule
  (rule (sq (ch \[)
            (alt (sq (to prefix-rule :prefix) (ch \;)
                     (to (comma-list integer-rule) :ints))
                 (to (comma-list #'literal) :items))
            (ch \]))
        list-value))

(def ^:private number-ahead (chars-term "" number-start?))

(defn- literal-value [st f]
  (cond (some? (:quoted f)) (:quoted f)
        (:int f) (let [lit (:int f) t (second (:suffix lit))]
                   (integer-value st lit (or t :int)))
        :else (:literal f)))

(def ^:private literal
  (rule (alt (sq (ahead number-ahead)
                 (alt (to float-rule :literal)
                      (to integer-rule :int)))
             (sq (ahead (chs \" \')) cut (to quoted-rule :quoted))
             (sq (ahead (ch \{)) cut (to map-rule :literal))
             (sq (ahead (ch \[)) cut (to list-rule :literal))
             (to builtin-rule :literal))
        literal-value))

(defn- state-of [[s n]]
  {:s s :len (count s) :pos (volatile! (long n))
   :err (volatile! {:cursor -1}) :silent? false})

(defn- failure [st]
  (let [{:keys [cursor reason]} @(:err st)]
    (assoc (or reason (err "failed")) :cursor cursor :input (:s st))))

(defn read-tag [rd]
  (let [st (state-of rd) v (literal st)]
    (if (some? v) [v [(:s st) (mark st)]] (failure st))))

(def ^:private not-compound "argument.nbt.expected.compound")

(defn read-compound [rd]
  (let [res (read-tag rd)]
    (cond (r/error? res) res
          (map? (first res)) res
          :else (r/error-at (second res) not-compound))))

(defn read-fully [rd]
  (let [res (read-tag rd)
        end (when-not (r/error? res)
              (r/skip-whitespace (second res)))]
    (cond (r/error? res) res
          (r/can-read? end) (r/error-at end "argument.nbt.trailing")
          :else [(first res) end])))

(defn- tag-of [x]
  (if (boolean? x) (Byte/valueOf (byte (if x 1 0))) x))

(def ^:private array-classes
  (mapv #(Class/forName %) ["[B" "[I" "[J"]))

(defn- array? [x] (some #(instance? % x) array-classes))

(defn- kind [x]
  (cond (map? x) :compound (vector? x) :list :else (class x)))

(declare tag=)

(defn- scalar= [e a]
  (and (= (class e) (class a))
       (if (array? e) (= (seq e) (seq a)) (Objects/equals e a))))

(defn- tag= [e a]
  (let [e (tag-of e) a (tag-of a)]
    (cond (map? e)
          (and (map? a) (= (set (keys e)) (set (keys a)))
               (every? #(tag= (e %) (a %)) (keys e)))
          (vector? e)
          (and (vector? a) (= (count e) (count a))
               (every? true? (map tag= e a)))
          :else (scalar= e a))))

(declare compare-nbt)

(defn- compound-match? [e a partial?]
  (and (>= (count a) (count e))
       (every? (fn [[k v]] (compare-nbt v (get a k) partial?)) e)))

(defn- list-match? [e a]
  (if (empty? e)
    (empty? a)
    (and (>= (count a) (count e))
         (every? (fn [x] (some #(compare-nbt x % true) a)) e))))

(defn compare-nbt [expected actual partial?]
  (let [e (tag-of expected) a (tag-of actual)]
    (cond (nil? e) true
          (nil? a) false
          (not= (kind e) (kind a)) false
          (map? e) (compound-match? e a partial?)
          (and (vector? e) partial?) (list-match? e a)
          :else (tag= e a))))
