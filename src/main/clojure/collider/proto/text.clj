(ns collider.proto.text
  "Text components on the wire.
  A whole number argument goes as an int tag, a BigInt one as the
  long tag vanilla gives a long."
  (:require [clojure.string :as str]
            [collider.proto.buf :as buf])
  (:import (clojure.lang BigInt)
           (collider HashMapOrder)
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

(defn- hash-ordered [fields]
  (let [hs (int-array (map (fn [[_ ^String nm]] (.hashCode nm)) fields))]
    (mapv #(nth fields %) (HashMapOrder/of hs))))

(def ^:private ordered (memoize hash-ordered))

(defn plain?
  "Returns true when component c is bare text with no style
  or children."
  [c]
  (or (string? c) (and (= 1 (count c)) (string? (:text c)))))

(defn- tag ^long [v]
  (cond (string? v) 8
        (map? v) (if (plain? v) 8 10)
        (boolean? v) 1
        (instance? Byte v) 1
        (instance? Float v) 5
        (instance? Double v) 6
        (instance? UUID v) 11
        (instance? BigInt v) 4
        :else 3))

(defn- list-tag ^long [xs]
  (reduce (fn [^long t x]
            (let [u (tag x)]
              (cond (zero? t) u (= t u) t :else (reduced 10))))
          0 xs))

(declare write-payload)

(defn- write-uuid [^Buf b ^UUID u]
  (buf/write-int! b 4)
  (let [hi (.getMostSignificantBits u)
        lo (.getLeastSignificantBits u)]
    (doseq [v [(bit-shift-right hi 32) hi
               (bit-shift-right lo 32) lo]]
      (buf/write-int! b (unchecked-int v)))))

(defn- write-entry [^Buf b ^String nm v]
  (buf/write-byte! b (tag v))
  (buf/write-utf! b nm)
  (write-payload b v))

(defn- write-element [^Buf b ^long t v]
  (if (and (= 10 t) (not= 10 (tag v)))
    (do (write-entry b "" v) (buf/write-byte! b 0))
    (write-payload b v)))

(defn- write-list [^Buf b xs]
  (let [t (list-tag xs)]
    (buf/write-byte! b t)
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
              (do (buf/write-byte! b 9) (buf/write-utf! b nm)
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
    8 (buf/write-utf! b (if (string? v) v (:text v)))
    10 (write-map b v)
    1 (buf/write-byte! b (if (boolean? v) (if v 1 0) (long v)))
    5 (buf/write-float! b (double v))
    6 (buf/write-double! b (double v))
    11 (write-uuid b v)
    4 (buf/write-long! b (long v))
    3 (buf/write-int! b (long v))))

(defn write-component
  "Writes component c as the network NBT of a text.
  Bare text goes as a string tag and anything else as a compound."
  [^Buf b c]
  (buf/write-byte! b (tag c))
  (write-payload b c))
