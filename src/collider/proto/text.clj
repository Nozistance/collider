(ns collider.proto.text
  "Text components on the wire.
  A component is a string of plain text or a map: :text or
  :translate with :with and :fallback, :extra children and the
  style keys :color :shadow-color :bold :italic :underlined
  :strikethrough :obfuscated :click :hover :insertion :font."
  (:require [clojure.string :as str]
            [collider.proto.buf :as buf])
  (:import (collider.java Buf)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- bucket ^long [^String s ^long cap]
  (let [h (.hashCode s)]
    (bit-and (bit-xor h (unsigned-bit-shift-right h 16)) (dec cap))))

(defn- hash-order
  "Returns the entries [key name] in the order a hash table of
  capacity cap walks them, ties kept in the given order."
  [entries cap]
  (->> (map-indexed vector entries)
       (sort-by (fn [[i [_ s]]] [(bucket s cap) i]))
       (mapv second)))

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

(def ^:private small-order (hash-order component-keys 16))

(def ^:private large-order (hash-order component-keys 32))

(def ^:private event-order (hash-order event-keys 16))

(defn plain?
  "Tells whether component c is bare text with no style or children."
  [c]
  (or (string? c) (and (= 1 (count c)) (string? (:text c)))))

(defn- tag ^long [v]
  (cond (string? v) 8
        (map? v) (if (plain? v) 8 10)
        (boolean? v) 1
        (instance? Float v) 5
        (instance? Double v) 6
        (instance? UUID v) 11
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

(defn- write-fields [^Buf b m order]
  (run! (fn [[k nm]]
          (let [v (get m k)]
            (when (and (some? v) (not (and (vector? v) (empty? v))))
              (if (vector? v)
                (do (buf/write-byte! b 9) (buf/write-utf! b nm)
                    (write-list b v))
                (write-entry b nm (wire-value k v))))))
        order)
  (buf/write-byte! b 0))

(defn- write-map [^Buf b m]
  (cond (plain? m) (buf/write-utf! b (:text m))
        (contains? m :action) (write-fields b m event-order)
        (> (count m) 12) (write-fields b m large-order)
        :else (write-fields b m small-order)))

(defn- write-payload [^Buf b v]
  (case (tag v)
    8 (buf/write-utf! b (if (string? v) v (:text v)))
    10 (write-map b v)
    1 (buf/write-byte! b (if v 1 0))
    5 (buf/write-float! b (double v))
    6 (buf/write-double! b (double v))
    11 (write-uuid b v)
    3 (buf/write-int! b (long v))))

(defn write-component
  "Writes component c as the network NBT of a text: bare text as a
  string tag, anything else as a compound."
  [^Buf b c]
  (buf/write-byte! b (tag c))
  (write-payload b c))
