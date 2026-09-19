(ns collider.proto.wire
  "Wire types as malli schemas, and the readers and writers
  compiled from them."
  (:refer-clojure :exclude [boolean byte double float int long short
                            string])
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

(def ^:private int-range
  [:int {:min -2147483648 :max 2147483647}])

(def ^:private double-range
  [:double {:min -1000000.0 :max 1000000.0}])

(def varint
  (wire-type :wire/varint int? c/read-varint c/write-varint
             int-range))

(def varlong
  (wire-type :wire/varlong int? nil c/write-varlong :int))

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

(def float
  (wire-type :wire/float number? buf/read-float buf/write-float!
             double-range))

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
  (wire-type :wire/block-pos vector? c/read-block-pos
             (fn [b [x y z]] (c/write-block-pos b x y z)) pos-gen))

(def vec3
  (wire-type :wire/vec3 vector?
             (fn [b] [(buf/read-double b) (buf/read-double b)
                      (buf/read-double b)])
             c/write-vec3
             [:tuple double-range double-range double-range]))

(def float-vec3
  (wire-type :wire/float-vec3 vector?
             (fn [b] [(buf/read-float b) (buf/read-float b)
                      (buf/read-float b)])
             (fn [b [x y z]] (buf/write-float! b x)
               (buf/write-float! b y) (buf/write-float! b z))))

(def text
  (wire-type :wire/text #(or (string? %) (map? %)) nil
             c/write-component :string))

(def nbt
  (wire-type :wire/nbt some? c/read-nbt c/write-nbt
             [:map-of :string :string]))

(def hashed-stack
  (wire-type :wire/hashed-stack #(or (nil? %) (map? %))
             c/read-hashed-stack nil
             [:enum nil {:item :stone :count 1}]))

(def holder-ref
  (wire-type :wire/holder-ref int? nil c/write-holder-ref
             [:int {:min 0 :max 100}]))

(def entity-data
  (wire-type :wire/entity-data sequential? nil c/write-entity-data
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

(declare -reader -writer)

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

(defn- map-reader [schema]
  (let [fs (mapv (fn [[k _ _ :as e]]
                   (let [r (-reader (field e))]
                     (fn [b m] (assoc m k (r b)))))
                 (m/children schema))]
    (fn [b] (reduce (fn [m f] (f b m)) {} fs))))

(defn- tuple-writer [schema]
  (let [ws (mapv -writer (m/children schema))]
    (fn [b v]
      (dotimes [i (count ws)] ((nth ws i) b (nth v i))))))

(defn- tuple-reader [schema]
  (let [rs (mapv -reader (m/children schema))]
    (fn [b] (mapv (fn [r] (r b)) rs))))

(defn- seq-writer [schema]
  (let [w (-writer (first (m/children schema)))]
    (fn [b xs]
      (c/write-varint b (count xs))
      (run! (fn [x] (w b x)) xs))))

(defn- seq-reader [schema]
  (let [r (-reader (first (m/children schema)))]
    (fn [b] (mapv (fn [_] (r b)) (range (c/read-count b))))))

(defn- map-of-writer [schema]
  (let [[kw vw] (mapv -writer (m/children schema))]
    (fn [b m]
      (c/write-varint b (count m))
      (run! (fn [[k v]] (kw b k) (vw b v)) m))))

(defn- map-of-reader [schema]
  (let [[kr vr] (mapv -reader (m/children schema))]
    (fn [b]
      (into {} (mapv (fn [_] [(kr b) (vr b)])
                     (range (c/read-count b)))))))

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
