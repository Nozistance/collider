(ns collider.proto.codec
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as str]
            [collider.data :as data])
  (:import (collider.java Buf)
           (java.io ByteArrayOutputStream DataOutputStream EOFException InputStream OutputStream)
           (java.nio.charset StandardCharsets)
           (java.util UUID)
           (java.util.zip Deflater Inflater)))

(set! *warn-on-reflection* true)

(def protocol-version 776)
(def game-version "26.2")
(defn write-varint [^Buf buf v]
  (loop [v (bit-and (long v) 0xFFFFFFFF)]
    (if (zero? (bit-and v (bit-not 0x7F)))
      (.writeByte buf (unchecked-int v))
      (do (.writeByte buf (unchecked-int (bit-or (bit-and v 0x7F) 0x80)))
          (recur (unsigned-bit-shift-right v 7))))))

(defn read-varint ^long [^Buf buf]
  (loop [n 0 r 0]
    (let [b (long (.readByte buf))
          r (bit-or r (bit-shift-left (bit-and b 0x7F) (* 7 n)))]
      (if (zero? (bit-and b 0x80)) (long (unchecked-int r)) (recur (inc n) r)))))

(defn write-varlong [^Buf buf ^long v]
  (loop [v v]
    (if (zero? (bit-and v (bit-not 0x7F)))
      (.writeByte buf (int v))
      (do (.writeByte buf (int (bit-or (bit-and v 0x7F) 0x80)))
          (recur (unsigned-bit-shift-right v 7))))))

(defn write-string [^Buf buf ^String s]
  (let [bs (.getBytes s StandardCharsets/UTF_8)]
    (write-varint buf (alength bs))
    (.writeBytes buf bs)))

(defn read-string ^String [^Buf buf]
  (let [n (read-varint buf) bs (byte-array n)]
    (.readBytes buf bs)
    (String. bs StandardCharsets/UTF_8)))

(defn write-uuid [^Buf buf ^UUID u]
  (.writeLong buf (.getMostSignificantBits u))
  (.writeLong buf (.getLeastSignificantBits u)))

(defn read-uuid ^UUID [^Buf buf]
  (UUID. (.readLong buf) (.readLong buf)))

(defn write-id [^Buf buf k]
  (write-string buf
                (cond
                  (keyword? k) (str (or (namespace k) "minecraft") ":"
                                    (str/replace (name k) "-" "_"))
                  (str/includes? (str k) ":") (str k)
                  :else (str "minecraft:" k))))

(defn- write-nbt-string [^DataOutputStream d ^String name ^String v]
  (.writeByte d 8) (.writeUTF d name) (.writeUTF d v))

(declare write-translatable)

(defn- write-argument [^DataOutputStream d a]
  (if (map? a)
    (write-translatable d a)
    (do (write-nbt-string d "text" (str a)) (.writeByte d 0))))

(defn- write-translatable [^DataOutputStream d {:keys [translate with]}]
  (write-nbt-string d "translate" translate)
  (when (seq with)
    (.writeByte d 9) (.writeUTF d "with")
    (cond
      (every? number? with)
      (do (.writeByte d 3) (.writeInt d (count with))
          (doseq [a with] (.writeInt d (int a))))
      (every? string? with)
      (do (.writeByte d 8) (.writeInt d (count with))
          (doseq [a with] (.writeUTF d a)))
      :else
      (do (.writeByte d 10) (.writeInt d (count with))
          (doseq [a with] (write-argument d a)))))
  (.writeByte d 0))

(defn write-component [^Buf buf s]
  (let [bo (ByteArrayOutputStream.)]
    (with-open [d (DataOutputStream. bo)]
      (if (map? s)
        (do (.writeByte buf 10) (write-translatable d s))
        (do (.writeByte buf 8) (.writeUTF d ^String (str s)))))
    (.writeBytes buf (.toByteArray bo))))

(defn- nbt-type ^long [v]
  (cond (map? v) 10 (string? v) 8 (boolean? v) 1
        (instance? Byte v) 1 (instance? Short v) 2 (instance? Long v) 4
        (integer? v) 3 (vector? v) 9
        :else (throw (ex-info "no NBT type" {:value v}))))

(defn- write-nbt-payload [^DataOutputStream d v]
  (cond
    (map? v) (do (doseq [[k x] v :when (some? x)]
                   (.writeByte d (nbt-type x)) (.writeUTF d (name k)) (write-nbt-payload d x))
                 (.writeByte d 0))
    (string? v) (.writeUTF d ^String v)
    (boolean? v) (.writeByte d (if v 1 0))
    (instance? Byte v) (.writeByte d (int ^Byte v))
    (instance? Short v) (.writeShort d (int ^Short v))
    (instance? Long v) (.writeLong d (long v))
    (integer? v) (.writeInt d (int v))
    (vector? v) (do (.writeByte d (if (empty? v) 0 (nbt-type (first v))))
                    (.writeInt d (count v))
                    (doseq [x v] (write-nbt-payload d x)))))

(defn write-nbt [^Buf buf v]
  (let [bo (ByteArrayOutputStream.)]
    (with-open [d (DataOutputStream. bo)]
      (.writeByte d (nbt-type v))
      (write-nbt-payload d v))
    (.writeBytes buf (.toByteArray bo))))

(defn write-angle [^Buf buf ^double deg]
  (.writeByte buf (unchecked-int (Math/floor (/ (* deg 256.0) 360.0)))))

(defn write-vec3 [^Buf buf [x y z]]
  (.writeDouble buf (double x)) (.writeDouble buf (double y)) (.writeDouble buf (double z)))

(defn- lp-pack ^long [^double v]
  (Math/round (* (+ (* v 0.5) 0.5) 32766.0)))

(defn write-lp-vec3 [^Buf buf [x y z]]
  (let [x (double x) y (double y) z (double z)
        m (max (Math/abs x) (Math/abs y) (Math/abs z))]
    (if (< m 3.051944088384301E-5)
      (.writeByte buf 0)
      (let [scale (long (Math/ceil m))
            partial? (not= (bit-and scale 3) scale)
            markers (if partial? (bit-or (bit-and scale 3) 4) scale)
            buffer (bit-or markers
                           (bit-shift-left (lp-pack (/ x scale)) 3)
                           (bit-shift-left (lp-pack (/ y scale)) 18)
                           (bit-shift-left (lp-pack (/ z scale)) 33))]
        (.writeByte buf (unchecked-int buffer))
        (.writeByte buf (unchecked-int (bit-shift-right buffer 8)))
        (.writeInt buf (unchecked-int (bit-shift-right buffer 16)))
        (when partial? (write-varint buf (bit-shift-right scale 2)))))))

(defn write-block-pos [^Buf buf ^long x ^long y ^long z]
  (.writeLong buf (bit-or (bit-shift-left (bit-and x 0x3FFFFFF) 38)
                          (bit-shift-left (bit-and z 0x3FFFFFF) 12)
                          (bit-and y 0xFFF))))

(defn read-block-pos [^Buf buf]
  (let [v (.readLong buf)]
    [(bit-shift-right v 38)
     (bit-shift-right (bit-shift-left v 52) 52)
     (bit-shift-right (bit-shift-left v 26) 38)]))

(defn section-pos ^long [^long sx ^long sy ^long sz]
  (bit-or (bit-shift-left (bit-and sx 0x3FFFFF) 42)
          (bit-shift-left (bit-and sz 0x3FFFFF) 20)
          (bit-and sy 0xFFFFF)))

(defn write-list [^Buf buf xs f]
  (write-varint buf (count xs))
  (doseq [x xs] (f buf x)))

(defn write-holder-ref [^Buf buf ^long id]
  (write-varint buf (inc id)))

(defn write-item-stack [^Buf buf stack]
  (if (nil? stack)
    (write-varint buf 0)
    (do (write-varint buf (long (:count stack 1)))
        (write-varint buf (data/registry-id "item" (:item stack)))
        (write-varint buf 0)
        (write-varint buf 0))))

(def ^:private item-names
  (delay (into {} (map (fn [[k v]] [(long v) k])) (get @data/registries "item"))))

(defn read-item-stack [^Buf buf]
  (let [n (read-varint buf)]
    (when (pos? n)
      (let [item (read-varint buf)
            added (read-varint buf)
            removed (read-varint buf)]
        (when-not (and (zero? added) (zero? removed))
          (throw (ex-info "data components not read yet"
                          {:item item :added added :removed removed})))
        {:item (get @item-names item) :count n}))))

(defn read-hashed-stack [^Buf buf]
  (when (.readBoolean buf)
    (let [item (read-varint buf)
          n    (read-varint buf)]
      (dotimes [_ (read-varint buf)] (read-varint buf) (.readInt buf))
      (dotimes [_ (read-varint buf)] (read-varint buf))
      {:item (get @item-names item) :count n})))

(def ^:private data-types {:byte 0 :int 1 :float 3 :item 7 :boolean 8 :block-pos 10 :optional-block-pos 11 :block-state 14 :pose 20})
(defn write-entity-data [^Buf buf entries]
  (doseq [[idx type v] entries]
    (.writeByte buf (int idx))
    (write-varint buf (data-types type))
    (case type
      :byte (.writeByte buf (int v))
      :int (write-varint buf (long v))
      :float (.writeFloat buf (float v))
      :item (write-item-stack buf v)
      :boolean (.writeBoolean buf (boolean v))
      :block-pos (let [[x y z] v] (write-block-pos buf (long x) (long y) (long z)))
      :optional-block-pos (do (.writeBoolean buf (some? v))
                              (when v (let [[x y z] v] (write-block-pos buf (long x) (long y) (long z)))))
      :block-state (write-varint buf (long v))
      :pose (write-varint buf (long v))))
  (.writeByte buf 0xFF))

(defn offline-uuid ^UUID [^String name]
  (UUID/nameUUIDFromBytes (.getBytes (str "OfflinePlayer:" name) StandardCharsets/UTF_8)))

(def ^:private ^:const max-uncompressed 8388608)
(defn- read-varint-stream ^long [^InputStream in]
  (loop [n 0 acc 0]
    (let [b (.read in)]
      (when (neg? b) (throw (EOFException. "end of stream")))
      (let [acc (bit-or acc (bit-shift-left (bit-and b 0x7F) (* n 7)))]
        (cond
          (zero? (bit-and b 0x80)) acc
          (>= n 2) (throw (ex-info "frame length varint too long" {}))
          :else (recur (inc n) acc))))))

(def ^:private ^:const frame-keep 8192)
(defn read-frame! ^Buf [^InputStream in ^Buf buf]
  (let [len (read-varint-stream in)]
    (.clear buf frame-keep)
    (.readFrom buf in (int len))
    buf))

(defn decompress! ^Buf [^Buf buf ^long threshold ^Inflater inflater]
  (when-not (neg? threshold)
    (let [n (read-varint buf)]
      (when (pos? n)
        (when (< n threshold)
          (throw (ex-info "badly compressed packet" {:size n :threshold threshold})))
        (when (> n max-uncompressed)
          (throw (ex-info "badly compressed packet" {:size n :max max-uncompressed})))
        (let [src (byte-array (.readableBytes buf))
              dst (byte-array n)]
          (.readBytes buf src)
          (.setInput inflater src)
          (let [got (.inflate inflater dst)]
            (.reset inflater)
            (when (not= got n)
              (throw (ex-info "badly compressed packet" {:got got :expected n}))))
          (.clear buf)
          (.writeBytes buf dst)))))
  buf)

(defn write-frame! [^OutputStream out ^Buf payload ^Buf body ^Buf head threshold ^Deflater deflater ^bytes chunk]
  (.clear body)
  (.clear head)
  (if (neg? (long threshold))
    (.writeBytes body payload)
    (let [n (.readableBytes payload)]
      (if (< n (long threshold))
        (do (write-varint body 0) (.writeBytes body payload))
        (let [src (byte-array n)]
          (.readBytes payload src)
          (write-varint body n)
          (.setInput deflater src)
          (.finish deflater)
          (while (not (.finished deflater))
            (.writeBytes body chunk 0 (.deflate deflater chunk)))
          (.reset deflater)))))
  (write-varint head (.readableBytes body))
  (.writeTo head out)
  (.writeTo body out))
