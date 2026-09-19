(ns collider.proto.wire
  "Wire types as malli schemas, and the readers and writers
  compiled from them."
  (:refer-clojure :exclude [boolean byte bytes double float int long
                            short string])
  (:require [collider.data :as data]
            [collider.proto.buf :as buf]
            [collider.proto.codec :as c]
            [malli.core :as m]))

(set! *warn-on-reflection* true)

(defn- wire-type
  "A leaf schema carrying its codec in the type properties."
  ([nm pred read write] (wire-type nm pred read write nil))
  ([nm pred read write gen]
   (m/-simple-schema
     {:type nm
      :pred pred
      :type-properties (cond-> {:wire/read read :wire/write write}
                         gen (assoc :gen/schema gen))})))

(declare -reader -writer)

(def ^:private int-range
  [:int {:min -2147483648 :max 2147483647}])

(def ^:private double-range
  [:double {:min -1000000.0 :max 1000000.0}])

(def varint
  (wire-type :wire/varint int? c/read-varint c/write-varint
             int-range))

(def varlong
  (wire-type :wire/varlong int? c/read-varlong c/write-varlong
             [:int {:min 0 :max 1000000}]))

(def byte
  (wire-type :wire/byte int? buf/read-byte buf/write-byte!
             [:int {:min -128 :max 127}]))

(def unsigned-byte
  (wire-type :wire/unsigned-byte int? buf/read-unsigned-byte
             buf/write-byte! [:int {:min 0 :max 255}]))

(def short
  (wire-type :wire/short int? buf/read-short buf/write-short!
             [:int {:min -32768 :max 32767}]))

(def unsigned-short
  (wire-type :wire/unsigned-short int? buf/read-unsigned-short
             buf/write-short! [:int {:min 0 :max 65535}]))

(def int
  (wire-type :wire/int int? buf/read-int buf/write-int! int-range))

(def long
  (wire-type :wire/long int? buf/read-long buf/write-long! :int))

(def ^:private float-gen
  "Doubles a float keeps whole, so a round trip stays an equality."
  [:enum 0.0 1.0 -1.0 0.5 -0.25 90.0 -1024.0 0.125])

(def float
  (wire-type :wire/float number? buf/read-float buf/write-float!
             float-gen))

(def double
  (wire-type :wire/double number? buf/read-double buf/write-double!
             double-range))

(def boolean
  (wire-type :wire/boolean boolean? buf/read-boolean
             buf/write-boolean! :boolean))

(def uuid
  (wire-type :wire/uuid uuid? c/read-uuid c/write-uuid :uuid))

(def id
  (wire-type :wire/id #(or (keyword? %) (string? %)) c/read-id
             c/write-id [:enum :stone :dirt :oak-log]))

(def ^:private pos-gen
  [:tuple [:int {:min -1000 :max 1000}] [:int {:min -2048 :max 2047}]
   [:int {:min -1000 :max 1000}]])

(def block-pos
  (wire-type :wire/block-pos sequential? c/read-block-pos
             (fn [b [x y z]] (c/write-block-pos b x y z)) pos-gen))

(def ^:private section-gen
  [:tuple [:int {:min -1000 :max 1000}] [:int {:min -64 :max 63}]
   [:int {:min -1000 :max 1000}]])

(def section-pos
  (wire-type :wire/section-pos sequential? c/read-section-pos
             (fn [b [x y z]]
               (buf/write-long! b (c/section-pos x y z)))
             section-gen))

(def section-change
  "One block of a section update: its place in the section and its
  state, both in a single varlong."
  (wire-type :wire/section-change sequential? c/read-section-change
             c/write-section-change
             [:tuple [:int {:min 0 :max 4095}]
              [:int {:min 0 :max 30000}]]))

(def ^:private angle-gen
  "The degrees a byte of 256ths of a turn keeps whole."
  [:enum 0.0 45.0 90.0 -45.0 -90.0 -180.0 1.40625 -1.40625])

(def angle
  (wire-type :wire/angle number? c/read-angle c/write-angle
             angle-gen))

(def vec3
  (wire-type :wire/vec3 sequential?
             (fn [b] [(buf/read-double b) (buf/read-double b)
                      (buf/read-double b)])
             c/write-vec3
             [:tuple double-range double-range double-range]))

(def ^:private eighth-gen
  "The coordinates a fixed point of eighths keeps whole."
  [:enum 0.0 0.5 -0.125 64.0 -1024.25 3.375])

(def fixed-vec3
  "A position the wire carries as three ints of eighths of a block."
  (wire-type :wire/fixed-vec3 sequential? c/read-fixed-vec3
             c/write-fixed-vec3
             [:tuple eighth-gen eighth-gen eighth-gen]))

(def float-vec3
  (wire-type :wire/float-vec3 sequential?
             (fn [b] [(buf/read-float b) (buf/read-float b)
                      (buf/read-float b)])
             (fn [b [x y z]] (buf/write-float! b x)
               (buf/write-float! b y) (buf/write-float! b z))
             [:tuple float-gen float-gen float-gen]))

(def ^:private lp-gen
  "The vectors quantization keeps whole: unit ends at scale one."
  [:enum [0.0 0.0 0.0] [1.0 0.0 0.0] [0.0 -1.0 0.0] [0.0 0.0 1.0]
   [1.0 -1.0 1.0] [-1.0 1.0 -1.0]])

(def lp-vec3
  (wire-type :wire/lp-vec3 sequential? c/read-lp-vec3 c/write-lp-vec3
             lp-gen))

(def text
  (wire-type :wire/text #(or (string? %) (map? %)) c/read-nbt
             c/write-component [:string {:max 32}]))

(def nbt
  (wire-type :wire/nbt any? c/read-nbt c/write-nbt
             [:maybe [:map-of [:enum :a :b :c] [:string {:max 5}]]]))

(def stat
  "A statistic named by its type and its entry: :mined/stone."
  (wire-type :wire/stat qualified-keyword? c/read-stat c/write-stat
             [:enum :custom/jump :mined/stone :killed/cow]))

(def hashed-stack
  (wire-type :wire/hashed-stack #(or (nil? %) (map? %))
             c/read-hashed-stack nil
             [:enum nil {:item :stone :count 1}]))

(def holder-ref
  (wire-type :wire/holder-ref int? c/read-holder-ref
             c/write-holder-ref
             [:int {:min 0 :max 100}]))

(def entity-data
  (wire-type :wire/entity-data sequential? c/read-entity-data
             c/write-entity-data
             [:enum [] [[0 :byte 5]] [[0 :byte 5] [8 :float 20.0]]]))

(def string
  (m/-simple-schema
    {:type :wire/string
     :compile
     (fn [props _ _]
       (let [max (:max props c/max-string-length)]
         {:pred string?
          :type-properties
          {:wire/read (fn [b] (c/read-string b max))
           :wire/write c/write-string
           :gen/schema [:string {:max (min max 32)}]}}))}))

(def item-stack
  (m/-simple-schema
    {:type :wire/item-stack
     :compile
     (fn [props _ _]
       (let [delimited? (:delimited props)]
         {:pred #(or (nil? %) (map? %))
          :type-properties
          {:wire/read (fn [b] (c/read-item-stack b delimited?))
           :wire/write c/write-item-stack
           :gen/schema [:enum nil {:item :stone :count 1}
                        {:item :dirt :count 64}]}}))}))

(defn- read-fixed [n]
  (fn [b]
    (let [a (byte-array n)]
      (buf/read-bytes! b a)
      (vec a))))

(defn- write-fixed [b v]
  (buf/write-bytes! b (byte-array v)))

(def bytes
  (m/-simple-schema
    {:type :wire/bytes
     :compile
     (fn [_ [n] _]
       {:pred #(and (vector? %) (= n (count %))) :min 1 :max 1
        :type-properties
        {:wire/read (read-fixed n)
         :wire/write write-fixed
         :gen/schema [:vector {:min n :max n}
                      [:int {:min -128 :max 127}]]}})}))

(def bitset
  (m/-simple-schema
    {:type :wire/bitset
     :compile
     (fn [_ [bits] _]
       (let [n (quot (+ bits 7) 8)
             top (dec (bit-shift-left 1 bits))]
         {:pred int? :min 1 :max 1
          :type-properties
          {:wire/read (fn [b] (c/read-bits b n))
           :wire/write (fn [b v] (c/write-bits b v n))
           :gen/schema [:int {:min 0 :max top}]}}))}))

(def bare
  "A value the wire pairs with an optional we never fill: the data of
  a registry entry, the tooltip of a command suggestion."
  (m/-simple-schema
    {:type :wire/bare
     :compile
     (fn [_ [child] options]
       (let [s (m/schema child options)
             r (-reader s) w (-writer s)]
         {:pred any? :min 1 :max 1
          :type-properties
          {:wire/read (fn [b] (let [v (r b)] (buf/read-boolean b) v))
           :wire/write (fn [b v] (w b v) (buf/write-boolean! b false))
           :gen/schema child}}))}))

(def tail
  "A value taking whatever is left of the packet, nil when nothing
  is: the options of a particle the client already knows the type of."
  (m/-simple-schema
    {:type :wire/tail
     :compile
     (fn [_ [child] options]
       (let [s (m/schema child options)
             r (-reader s) w (-writer s)]
         {:pred any? :min 1 :max 1
          :type-properties
          {:wire/read (fn [b]
                        (when (pos? (buf/readable-bytes b)) (r b)))
           :wire/write (fn [b v] (when (some? v) (w b v)))
           :gen/schema [:maybe child]}}))}))

(def reg
  (m/-simple-schema
    {:type :wire/reg
     :compile
     (fn [_ [registry] _]
       {:pred keyword? :min 1 :max 1
        :type-properties
        {:wire/read
         (fn [b] (data/entry-name registry (c/read-varint b)))
         :wire/write
         (fn [b v]
           (c/write-varint b (data/entry-id registry v)))}})}))

(defn- codec-of [schema kind]
  (or (kind (m/type-properties schema))
      (throw (ex-info (str "no wire " (name kind))
                      {:schema (m/form schema)}))))

(defn- const-codec [schema kind]
  (let [w (:wire (m/properties schema))]
    (when-not w
      (throw (ex-info "constant without a wire type"
                      {:schema (m/form schema)})))
    ((if (= :wire/read kind) -reader -writer) (m/schema w))))

(defn- field [[k props child]]
  (when (and (:optional props) (not= := (m/type child)))
    (throw (ex-info "optional field on the wire" {:field k})))
  child)

(defn- map-writer [schema]
  (let [fs (mapv (fn [[k _ _ :as e]]
                   (let [w (-writer (field e))]
                     (fn [b m] (w b (get m k)))))
                 (m/children schema))]
    (fn [b m] (run! (fn [f] (f b m)) fs))))

(defn- map-field-reader
  "A filler is on the wire but not in the message, so its value is
  checked and dropped."
  [[k props _ :as e]]
  (let [r (-reader (field e))]
    (if (:optional props)
      (fn [b m] (r b) m)
      (fn [b m] (assoc m k (r b))))))

(defn- map-reader [schema]
  (let [fs (mapv map-field-reader (m/children schema))]
    (fn [b] (reduce (fn [m f] (f b m)) {} fs))))

(defn- tuple-writer [schema]
  (let [ws (mapv -writer (m/children schema))]
    (fn [b v]
      (dotimes [i (count ws)] ((nth ws i) b (nth v i))))))

(defn- tuple-reader [schema]
  (let [rs (mapv -reader (m/children schema))]
    (fn [b] (mapv (fn [r] (r b)) rs))))

(defn- limit [schema]
  (:max (m/properties schema) Long/MAX_VALUE))

(defn- fits
  "The count of a wire collection, refused past the vanilla limit."
  ^long [^long n ^long mx]
  (if (> n mx)
    (throw (ex-info "too many elements" {:count n :max mx}))
    n))

(defn- seq-writer [schema]
  (let [w (-writer (first (m/children schema)))
        mx (limit schema)]
    (fn [b xs]
      (c/write-varint b (fits (count xs) mx))
      (run! (fn [x] (w b x)) xs))))

(defn- seq-reader [schema]
  (let [r (-reader (first (m/children schema)))
        mx (limit schema)]
    (fn [b]
      (mapv (fn [_] (r b))
            (range (fits (c/read-count b) mx))))))

(defn- map-of-writer [schema]
  (let [[kw vw] (mapv -writer (m/children schema))
        mx (limit schema)]
    (fn [b m]
      (c/write-varint b (fits (count m) mx))
      (run! (fn [[k v]] (kw b k) (vw b v)) m))))

(defn- map-of-reader [schema]
  (let [[kr vr] (mapv -reader (m/children schema))
        mx (limit schema)]
    (fn [b]
      (into {} (mapv (fn [_] [(kr b) (vr b)])
                     (range (fits (c/read-count b) mx)))))))

(defn- maybe-writer [schema]
  (let [w (-writer (first (m/children schema)))]
    (fn [b v]
      (buf/write-boolean! b (some? v))
      (when (some? v) (w b v)))))

(defn- maybe-reader [schema]
  (let [r (-reader (first (m/children schema)))]
    (fn [b] (when (buf/read-boolean b) (r b)))))

(defn- enum-writer [schema]
  (let [ids (zipmap (m/children schema) (range))]
    (fn [b v] (c/write-varint b (ids v)))))

(defn- enum-reader [schema]
  (let [names (vec (m/children schema))]
    (fn [b] (nth names (c/read-varint b)))))

(defn- const-writer [schema]
  (let [v (first (m/children schema))
        w (const-codec schema :wire/write)]
    (fn [b _] (w b v))))

(defn- const-reader [schema]
  (let [v (first (m/children schema))
        r (const-codec schema :wire/read)]
    (fn [b]
      (let [got (r b)]
        (when (not= v got)
          (throw (ex-info "wire constant does not match"
                          {:want v :got got})))
        v))))

(defn- -writer [schema]
  (case (m/type schema)
    :map (map-writer schema)
    :tuple (tuple-writer schema)
    :sequential (seq-writer schema)
    :map-of (map-of-writer schema)
    :maybe (maybe-writer schema)
    :enum (enum-writer schema)
    := (const-writer schema)
    (codec-of schema :wire/write)))

(defn- -reader [schema]
  (case (m/type schema)
    :map (map-reader schema)
    :tuple (tuple-reader schema)
    :sequential (seq-reader schema)
    :map-of (map-of-reader schema)
    :maybe (maybe-reader schema)
    :enum (enum-reader schema)
    := (const-reader schema)
    (codec-of schema :wire/read)))

(defn reader
  "Compiles a fn taking the value of schema off a buffer."
  [schema]
  (-reader (m/schema schema)))

(defn writer
  "Compiles a fn putting a value of schema on a buffer."
  [schema]
  (-writer (m/schema schema)))
