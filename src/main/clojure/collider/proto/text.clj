(ns collider.proto.text
  "Text components on the wire."
  (:require [clojure.string :as str]
            [collider.hash-order :as hash-order]
            [collider.proto.buf :as buf]
            [collider.proto.nbt :as nbt])
  (:import (clojure.lang BigInt)
           (collider.proto Buf)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(def ^:private component-keys
  [[:text "text"] [:translate "translate"] [:fallback "fallback"]
   [:with "with"] [:extra "extra"] [:color "color"]
   [:shadow-color "shadow_color"] [:bold "bold"] [:italic "italic"]
   [:underlined "underlined"] [:strikethrough "strikethrough"]
   [:obfuscated "obfuscated"] [:click "click_event"]
   [:hover "hover_event"] [:insertion "insertion"] [:font "font"]])

(def ^:private event-keys
  [[:action "action"] [:url "url"] [:path "path"]
   [:command "command"] [:page "page"] [:value "value"]
   [:id "id"] [:count "count"] [:uuid "uuid"] [:name "name"]])

(def ^:private ordered (memoize #(hash-order/by-name second %)))

(defn plain?
  "Returns true when component c is bare text.
  Bare text has no style and no children."
  [c]
  (or (string? c) (and (= 1 (count c)) (string? (:text c)))))

(defn- tag [v]
  (cond (string? v) :string
        (map? v) (if (plain? v) :string :compound)
        (boolean? v) :byte
        (instance? Byte v) :byte
        (instance? Float v) :float
        (instance? Double v) :double
        (instance? UUID v) :int-array
        (instance? BigInt v) :long
        :else :int))

(defn- list-tag [xs]
  (reduce (fn [t x]
            (let [u (tag x)]
              (cond (= :end t) u
                    (= t u) t
                    :else (reduced :compound))))
          :end xs))

(declare write-payload)

(defn- write-uuid [^Buf b ^UUID u]
  (buf/write-int! b 4)
  (let [hi (.getMostSignificantBits u)
        lo (.getLeastSignificantBits u)]
    (doseq [v [(bit-shift-right hi 32) hi
               (bit-shift-right lo 32) lo]]
      (buf/write-int! b (unchecked-int v)))))

(defn- write-entry [^Buf b ^String nm v]
  (buf/write-byte! b (nbt/tag-id (tag v)))
  (buf/write-utf! b nm)
  (write-payload b v))

(defn- write-element [^Buf b t v]
  (if (and (= :compound t) (not= :compound (tag v)))
    (do (write-entry b "" v) (buf/write-byte! b 0))
    (write-payload b v)))

(defn- write-list [^Buf b xs]
  (let [t (list-tag xs)]
    (buf/write-byte! b (nbt/tag-id t))
    (buf/write-int! b (count xs))
    (run! #(write-element b t %) xs)))

(defn- wire-value [k v]
  (case k
    (:action :id) (str (when (= :id k) "minecraft:")
                       (str/replace (name v) \- \_))
    v))

(defn- written? [m [k]]
  (let [v (get m k)]
    (and (some? v) (not (and (vector? v) (empty? v))))))

(defn- write-fields [^Buf b m fields]
  (run! (fn [[k nm]]
          (let [v (get m k)]
            (if (vector? v)
              (do (buf/write-byte! b (nbt/tag-id :list))
                  (buf/write-utf! b nm)
                  (write-list b v))
              (write-entry b nm (wire-value k v)))))
        (ordered (filterv #(written? m %) fields)))
  (buf/write-byte! b 0))

(defn- write-map [^Buf b m]
  (cond (plain? m) (buf/write-utf! b (:text m))
        (contains? m :action) (write-fields b m event-keys)
        :else (write-fields b m component-keys)))

(defn- write-payload [^Buf b v]
  (case (tag v)
    :string (buf/write-utf! b (if (string? v) v (:text v)))
    :compound (write-map b v)
    :byte (buf/write-byte! b (if (boolean? v) (if v 1 0) (long v)))
    :float (buf/write-float! b (double v))
    :double (buf/write-double! b (double v))
    :int-array (write-uuid b v)
    :long (buf/write-long! b (long v))
    :int (buf/write-int! b (long v))))

(defn write-component
  "Writes component c as the network NBT of a text."
  [^Buf b c]
  (buf/write-byte! b (nbt/tag-id (tag c)))
  (write-payload b c))
