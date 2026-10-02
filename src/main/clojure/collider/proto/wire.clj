(ns collider.proto.wire
  "Wire types of packet fields."
  (:refer-clojure
    :exclude [boolean byte bytes double float int long short string])
  (:require [collider.data :as data]
            [collider.proto.buf :as buf]
            [collider.proto.codec :as c]
            [collider.proto.components :as comps]
            [collider.proto.entitydata :as ed]
            [collider.proto.nbt :as nbt]
            [collider.proto.text :as text]
            [malli.core :as m]))

(set! *warn-on-reflection* true)

(defn- wire-type [nm pred read write]
  (m/-simple-schema
    {:type nm
     :pred pred
     :type-properties {:wire/read read :wire/write write}}))

(declare reader* writer*)

(def varint
  (wire-type :wire/varint int? c/read-varint c/write-varint))

(def varlong
  (wire-type :wire/varlong int? c/read-varlong c/write-varlong))

(def byte
  (wire-type :wire/byte int? buf/read-byte buf/write-byte!))

(def unsigned-byte
  (wire-type :wire/unsigned-byte int? buf/read-unsigned-byte
             buf/write-byte!))

(def short
  (wire-type :wire/short int? buf/read-short buf/write-short!))

(def unsigned-short
  (wire-type :wire/unsigned-short int? buf/read-unsigned-short
             buf/write-short!))

(def int
  (wire-type :wire/int int? buf/read-int buf/write-int!))

(def long
  (wire-type :wire/long int? buf/read-long buf/write-long!))

(def float
  (wire-type :wire/float number? buf/read-float buf/write-float!))

(def double
  (wire-type :wire/double number? buf/read-double buf/write-double!))

(def boolean
  (wire-type :wire/boolean boolean? buf/read-boolean
             buf/write-boolean!))

(def uuid
  (wire-type :wire/uuid uuid? c/read-uuid c/write-uuid))

(def id
  (wire-type :wire/id #(or (keyword? %) (string? %)) c/read-id
             c/write-id))

(def block-pos
  (wire-type :wire/block-pos sequential? c/read-block-pos
             (fn [b [x y z]] (c/write-block-pos b x y z))))

(def section-pos
  (wire-type :wire/section-pos sequential? c/read-section-pos
             (fn [b [x y z]]
               (buf/write-long! b (c/section-pos x y z)))))

(def section-change
  "One block change of a section update."
  (wire-type :wire/section-change sequential? c/read-section-change
             c/write-section-change))

(def angle
  (wire-type :wire/angle number? c/read-angle c/write-angle))

(def vec3
  (wire-type :wire/vec3 sequential? c/read-vec3 c/write-vec3))

(def fixed-vec3
  "A position the wire carries as three ints of eighths of a block."
  (wire-type :wire/fixed-vec3 sequential? c/read-fixed-vec3
             c/write-fixed-vec3))

(defn- read-float-vec3 [b]
  [(buf/read-float b) (buf/read-float b) (buf/read-float b)])

(defn- write-float-vec3 [b [x y z]]
  (buf/write-float! b x)
  (buf/write-float! b y)
  (buf/write-float! b z))

(def float-vec3
  (wire-type :wire/float-vec3 sequential? read-float-vec3
             write-float-vec3))

(def lp-vec3
  (wire-type :wire/lp-vec3 sequential? c/read-lp-vec3
             c/write-lp-vec3))

(def text
  (wire-type :wire/text #(or (string? %) (map? %)) nbt/read-nbt
             text/write-component))

(def nbt
  (wire-type :wire/nbt any? nbt/read-nbt nbt/write-nbt))

(def stat
  "A statistic named by its type and its entry, like mined/stone."
  (wire-type :wire/stat qualified-keyword? c/read-stat c/write-stat))

(def hashed-stack
  (wire-type :wire/hashed-stack #(or (nil? %) (map? %))
             comps/read-hashed-stack nil))

(def holder-ref
  (wire-type :wire/holder-ref int? c/read-holder-ref
             c/write-holder-ref))

(defn- write-sound-holder [b v]
  (if (string? v)
    (do (c/write-varint b 0) (c/write-string b v)
        (buf/write-boolean! b false))
    (c/write-holder-ref b v)))

(defn- read-sound-holder [b]
  (let [i (c/read-varint b)]
    (if (zero? i)
      (let [s (c/read-string b)]
        (when (buf/read-boolean b) (buf/read-float b))
        s)
      (dec i))))

(def sound-holder
  "A sound event by id, or by name with no fixed range."
  (wire-type :wire/sound-holder #(or (int? %) (string? %))
             read-sound-holder write-sound-holder))

(def entity-data
  (wire-type :wire/entity-data sequential? ed/read-entity-data
             ed/write-entity-data))

(def string
  (m/-simple-schema
    {:type :wire/string
     :compile
     (fn [props _ _]
       (let [max (:max props c/max-string-length)]
         {:pred string?
          :type-properties
          {:wire/read (fn [b] (c/read-string b max))
           :wire/write c/write-string}}))}))

(def item-stack
  (m/-simple-schema
    {:type :wire/item-stack
     :compile
     (fn [props _ _]
       (let [delimited? (:delimited props)]
         {:pred #(or (nil? %) (map? %))
          :type-properties
          {:wire/read (fn [b] (comps/read-item-stack b delimited?))
           :wire/write comps/write-item-stack}}))}))

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
         :wire/write write-fixed}})}))

(defn- read-blob [b]
  ((read-fixed (c/read-count b)) b))

(defn- write-blob [b v]
  (c/write-varint b (count v))
  (write-fixed b v))

(def blob
  "Bytes after their count."
  (wire-type :wire/blob vector? read-blob write-blob))

(def bitset
  (m/-simple-schema
    {:type :wire/bitset
     :compile
     (fn [_ [bits] _]
       (let [n (quot (+ bits 7) 8)]
         {:pred int? :min 1 :max 1
          :type-properties
          {:wire/read (fn [b] (c/read-bits b n))
           :wire/write (fn [b v] (c/write-bits b v n))}}))}))

(def bare
  "A value followed by an absent optional field."
  (m/-simple-schema
    {:type :wire/bare
     :compile
     (fn [_ [child] options]
       (let [s (m/schema child options)
             r (reader* s) w (writer* s)]
         {:pred any? :min 1 :max 1
          :type-properties
          {:wire/read (fn [b] (let [v (r b)] (buf/read-boolean b) v))
           :wire/write
           (fn [b v] (w b v) (buf/write-boolean! b false))}}))}))

(def particle
  "A particle as [type options], the options shaped by the type."
  (wire-type :wire/particle vector? comps/read-particle
             comps/write-particle))

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
    ((if (= :wire/read kind) reader* writer*) (m/schema w))))

(defn- field [[k props child]]
  (when (and (:optional props) (not= := (m/type child)))
    (throw (ex-info "optional field on the wire" {:field k})))
  child)

(defn- map-writer [schema]
  (let [fs (mapv (fn [[k _ _ :as e]]
                   (let [w (writer* (field e))]
                     (fn [b m] (w b (get m k)))))
                 (m/children schema))]
    (fn [b m] (run! (fn [f] (f b m)) fs))))

(defn- map-field-reader
  [[k props _ :as e]]
  (let [r (reader* (field e))]
    (if (:optional props)
      (fn [b m] (r b) m)
      (fn [b m] (assoc m k (r b))))))

(defn- map-reader [schema]
  (let [fs (mapv map-field-reader (m/children schema))]
    (fn [b] (reduce (fn [m f] (f b m)) {} fs))))

(defn- tuple-writer [schema]
  (let [ws (mapv writer* (m/children schema))]
    (fn [b v]
      (dotimes [i (count ws)] ((nth ws i) b (nth v i))))))

(defn- tuple-reader [schema]
  (let [rs (mapv reader* (m/children schema))]
    (fn [b] (mapv (fn [r] (r b)) rs))))

(defn- limit [schema]
  (:max (m/properties schema) Long/MAX_VALUE))

(defn- fits ^long [^long n ^long mx]
  (if (> n mx)
    (throw (ex-info "too many elements" {:count n :max mx}))
    n))

(defn- seq-writer [schema]
  (let [w (writer* (first (m/children schema)))
        mx (limit schema)]
    (fn [b xs]
      (c/write-varint b (fits (count xs) mx))
      (run! (fn [x] (w b x)) xs))))

(defn- seq-reader [schema]
  (let [r (reader* (first (m/children schema)))
        mx (limit schema)]
    (fn [b]
      (mapv (fn [_] (r b))
            (range (fits (c/read-count b) mx))))))

(defn- map-of-writer [schema]
  (let [[kw vw] (mapv writer* (m/children schema))
        mx (limit schema)]
    (fn [b m]
      (c/write-varint b (fits (count m) mx))
      (run! (fn [[k v]] (kw b k) (vw b v)) m))))

(defn- map-of-reader [schema]
  (let [[kr vr] (mapv reader* (m/children schema))
        mx (limit schema)]
    (fn [b]
      (into {} (map (fn [_] [(kr b) (vr b)]))
            (range (fits (c/read-count b) mx))))))

(defn- maybe-writer [schema]
  (let [w (writer* (first (m/children schema)))]
    (fn [b v]
      (buf/write-boolean! b (some? v))
      (when (some? v) (w b v)))))

(defn- maybe-reader [schema]
  (let [r (reader* (first (m/children schema)))]
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
          (let [data {:want v :got got}]
            (throw (ex-info "wire constant does not match" data))))
        v))))

(defn- writer* [schema]
  (case (m/type schema)
    :map (map-writer schema)
    :tuple (tuple-writer schema)
    :sequential (seq-writer schema)
    :map-of (map-of-writer schema)
    :maybe (maybe-writer schema)
    :enum (enum-writer schema)
    := (const-writer schema)
    (codec-of schema :wire/write)))

(defn- reader* [schema]
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
  "Returns a fn that reads a value of schema."
  [schema]
  (reader* (m/schema schema)))

(defn writer
  "Returns a fn that writes a value of schema."
  [schema]
  (writer* (m/schema schema)))
