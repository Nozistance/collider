(ns collider.proto.codec
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as str]
            [collider.data :as data])
  (:import (io.netty.buffer ByteBuf)
           (io.netty.channel ChannelHandlerContext)
           (io.netty.handler.codec ByteToMessageDecoder MessageToByteEncoder)
           (java.io ByteArrayOutputStream DataOutputStream)
           (java.nio.charset StandardCharsets)
           (java.util List UUID)))

(set! *warn-on-reflection* true)

(def protocol-version 776)
(def game-version "26.2")

(defn write-varint [^ByteBuf buf v]
  (loop [v (bit-and (long v) 0xFFFFFFFF)]
    (if (zero? (bit-and v (bit-not 0x7F)))
      (.writeByte buf (unchecked-int v))
      (do (.writeByte buf (unchecked-int (bit-or (bit-and v 0x7F) 0x80)))
          (recur (unsigned-bit-shift-right v 7))))))

(defn read-varint ^long [^ByteBuf buf]
  (loop [n 0 r 0]
    (let [b (long (.readByte buf))
          r (bit-or r (bit-shift-left (bit-and b 0x7F) (* 7 n)))]
      (if (zero? (bit-and b 0x80)) (long (unchecked-int r)) (recur (inc n) r)))))

(defn write-varlong [^ByteBuf buf ^long v]
  (loop [v v]
    (if (zero? (bit-and v (bit-not 0x7F)))
      (.writeByte buf (int v))
      (do (.writeByte buf (int (bit-or (bit-and v 0x7F) 0x80)))
          (recur (unsigned-bit-shift-right v 7))))))

(defn write-string [^ByteBuf buf ^String s]
  (let [bs (.getBytes s StandardCharsets/UTF_8)]
    (write-varint buf (alength bs))
    (.writeBytes buf bs)))

(defn read-string ^String [^ByteBuf buf]
  (let [n (read-varint buf) bs (byte-array n)]
    (.readBytes buf bs)
    (String. bs StandardCharsets/UTF_8)))

(defn write-uuid [^ByteBuf buf ^UUID u]
  (.writeLong buf (.getMostSignificantBits u))
  (.writeLong buf (.getLeastSignificantBits u)))

(defn read-uuid ^UUID [^ByteBuf buf]
  (UUID. (.readLong buf) (.readLong buf)))

(defn write-id [^ByteBuf buf k]
  (write-string buf
                (cond
                  (keyword? k) (str (or (namespace k) "minecraft") ":"
                                    (str/replace (name k) "-" "_"))
                  (str/includes? (str k) ":") (str k)
                  :else (str "minecraft:" k))))

(defn write-component [^ByteBuf buf s]
  (let [bo (ByteArrayOutputStream.)]
    (with-open [d (DataOutputStream. bo)]
      (.writeUTF d ^String (str s)))
    (.writeByte buf 8)
    (.writeBytes buf (.toByteArray bo))))

(defn write-angle [^ByteBuf buf ^double deg]
  (.writeByte buf (unchecked-int (Math/floor (/ (* deg 256.0) 360.0)))))

(defn write-vec3 [^ByteBuf buf [x y z]]
  (.writeDouble buf (double x)) (.writeDouble buf (double y)) (.writeDouble buf (double z)))

(defn- lp-pack ^long [^double v]
  (Math/round (* (+ (* v 0.5) 0.5) 32766.0)))

(defn write-lp-vec3 [^ByteBuf buf [x y z]]
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

(defn write-block-pos [^ByteBuf buf ^long x ^long y ^long z]
  (.writeLong buf (bit-or (bit-shift-left (bit-and x 0x3FFFFFF) 38)
                          (bit-shift-left (bit-and z 0x3FFFFFF) 12)
                          (bit-and y 0xFFF))))

(defn read-block-pos [^ByteBuf buf]
  (let [v (.readLong buf)]
    [(bit-shift-right v 38)
     (bit-shift-right (bit-shift-left v 52) 52)
     (bit-shift-right (bit-shift-left v 26) 38)]))

(defn section-pos ^long [^long sx ^long sy ^long sz]
  (bit-or (bit-shift-left (bit-and sx 0x3FFFFF) 42)
          (bit-shift-left (bit-and sz 0x3FFFFF) 20)
          (bit-and sy 0xFFFFF)))

(defn write-list [^ByteBuf buf xs f]
  (write-varint buf (count xs))
  (doseq [x xs] (f buf x)))

(defn write-holder-ref [^ByteBuf buf ^long id]
  (write-varint buf (inc id)))

(defn write-item-stack [^ByteBuf buf stack]
  (if (nil? stack)
    (write-varint buf 0)
    (do (write-varint buf (long (:count stack 1)))
        (write-varint buf (data/registry-id "item" (:item stack)))
        (write-varint buf 0)
        (write-varint buf 0))))

(def ^:private item-names
  (delay (into {} (map (fn [[k v]] [(long v) k])) (get @data/registries "item"))))

(defn read-item-stack [^ByteBuf buf]
  (let [n (read-varint buf)]
    (when (pos? n)
      (let [item (read-varint buf)
            added (read-varint buf)
            removed (read-varint buf)]
        (when-not (and (zero? added) (zero? removed))
          (throw (ex-info "data components not read yet"
                          {:item item :added added :removed removed})))
        {:item (get @item-names item) :count n}))))

(def ^:private data-types {:byte 0 :int 1 :float 3 :item 7 :boolean 8 :block-state 14})

(defn write-entity-data [^ByteBuf buf entries]
  (doseq [[idx type v] entries]
    (.writeByte buf (int idx))
    (write-varint buf (data-types type))
    (case type
      :byte (.writeByte buf (int v))
      :int (write-varint buf (long v))
      :float (.writeFloat buf (float v))
      :item (write-item-stack buf v)
      :boolean (.writeBoolean buf (boolean v))
      :block-state (write-varint buf (long v))))
  (.writeByte buf 0xFF))

(defn offline-uuid ^UUID [^String name]
  (UUID/nameUUIDFromBytes (.getBytes (str "OfflinePlayer:" name) StandardCharsets/UTF_8)))

(defn- read-frame-len ^long [^ByteBuf in]
  (loop [n 0 acc 0]
    (if-not (.isReadable in)
      -1
      (let [b   (long (.readByte in))
            acc (bit-or acc (bit-shift-left (bit-and b 0x7F) (* n 7)))]
        (cond
          (zero? (bit-and b 0x80)) acc
          (>= n 2) (throw (ex-info "frame length varint too long" {}))
          :else (recur (inc n) acc))))))

(defn frame-decoder []
  (proxy [ByteToMessageDecoder] []
    (decode [^ChannelHandlerContext _ctx ^ByteBuf in ^List out]
      (loop []
        (when (.isReadable in)
          (.markReaderIndex in)
          (let [len (read-frame-len in)]
            (if (or (neg? len) (< (.readableBytes in) len))
              (.resetReaderIndex in)
              (do (.add out (.readRetainedSlice in (int len)))
                  (recur)))))))))

(defn frame-encoder []
  (proxy [MessageToByteEncoder] []
    (encode [^ChannelHandlerContext _ctx ^ByteBuf msg ^ByteBuf out]
      (write-varint out (.readableBytes msg))
      (.writeBytes out msg))))
