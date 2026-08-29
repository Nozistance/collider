(ns collider.proto.types
  (:refer-clojure :exclude [read-string])
  (:import (io.netty.buffer ByteBuf)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn read-varint ^long [^ByteBuf buf]
  (loop [n 0 acc 0]
    (let [b   (long (.readByte buf))
          acc (bit-or acc (bit-shift-left (bit-and b 0x7F) (* n 7)))]
      (cond
        (zero? (bit-and b 0x80)) (unchecked-long (unchecked-int acc))
        (>= n 4) (throw (ex-info "VarInt longer than 5 bytes" {}))
        :else (recur (inc n) acc)))))

(defn write-varint [^ByteBuf buf v]
  (loop [v (bit-and (long v) 0xFFFFFFFF)]
    (if (zero? (bit-and v 0xFFFFFF80))
      (.writeByte buf (unchecked-int v))
      (do (.writeByte buf (unchecked-int (bit-or (bit-and v 0x7F) 0x80)))
          (recur (unsigned-bit-shift-right v 7))))))

(defn read-string ^String [^ByteBuf buf]
  (let [len (read-varint buf)]
    (when (or (neg? len) (> len 32767))
      (throw (ex-info "String length out of bounds" {:len len})))
    (.toString (.readCharSequence buf (int len) StandardCharsets/UTF_8))))

(defn write-string [^ByteBuf buf ^String s]
  (let [bs (.getBytes s StandardCharsets/UTF_8)]
    (write-varint buf (alength bs))
    (.writeBytes buf bs)))

(defn read-bytes ^bytes [^ByteBuf buf]
  (let [len (read-varint buf)
        bs  (byte-array len)]
    (.readBytes buf bs)
    bs))

(defn write-bytes [^ByteBuf buf ^bytes bs]
  (write-varint buf (alength bs))
  (.writeBytes buf bs))

(defn read-bool [^ByteBuf buf] (.readBoolean buf))
(defn write-bool [^ByteBuf buf v] (.writeBoolean buf (boolean v)))
(defn read-byte ^long [^ByteBuf buf] (long (.readByte buf)))
(defn write-byte [^ByteBuf buf v] (.writeByte buf (unchecked-int (long v))))
(defn read-ubyte ^long [^ByteBuf buf] (long (.readUnsignedByte buf)))
(defn write-ubyte [^ByteBuf buf v] (.writeByte buf (unchecked-int (long v))))
(defn read-short ^long [^ByteBuf buf] (long (.readShort buf)))
(defn write-short [^ByteBuf buf v] (.writeShort buf (unchecked-int (long v))))
(defn read-ushort ^long [^ByteBuf buf] (long (.readUnsignedShort buf)))
(defn write-ushort [^ByteBuf buf v] (.writeShort buf (unchecked-int (long v))))
(defn read-int ^long [^ByteBuf buf] (long (.readInt buf)))
(defn write-int [^ByteBuf buf v] (.writeInt buf (unchecked-int (long v))))
(defn read-long ^long [^ByteBuf buf] (.readLong buf))
(defn write-long [^ByteBuf buf v] (.writeLong buf (long v)))
(defn read-float ^double [^ByteBuf buf] (double (.readFloat buf)))
(defn write-float [^ByteBuf buf v] (.writeFloat buf (unchecked-float (double v))))
(defn read-double ^double [^ByteBuf buf] (.readDouble buf))
(defn write-double [^ByteBuf buf v] (.writeDouble buf (double v)))
(defn read-position [^ByteBuf buf]
  (let [v (.readLong buf)]
    [(bit-shift-right v 38)
     (bit-and (bit-shift-right v 26) 0xFFF)
     (bit-shift-right (bit-shift-left v 38) 38)]))

(defn write-position [^ByteBuf buf [x y z]]
  (.writeLong buf (bit-or (bit-shift-left (bit-and (long x) 0x3FFFFFF) 38)
                          (bit-shift-left (bit-and (long y) 0xFFF) 26)
                          (bit-and (long z) 0x3FFFFFF))))

(defn read-uuid ^UUID [^ByteBuf buf]
  (UUID. (.readLong buf) (.readLong buf)))

(defn write-uuid [^ByteBuf buf ^UUID u]
  (.writeLong buf (.getMostSignificantBits u))
  (.writeLong buf (.getLeastSignificantBits u)))

(defn write-slot [^ByteBuf buf stack]
  (if (nil? stack)
    (write-short buf -1)
    (do (write-short buf (:item stack))
        (write-byte buf (:count stack 1))
        (write-short buf (:damage stack 0))
        (write-byte buf 0))))

(def ^:private meta-type-id {:byte 0, :short 1, :int 2, :float 3, :string 4, :slot 5})
(defn read-metadata [^ByteBuf buf]
  (loop [acc []]
    (let [b (long (.readUnsignedByte buf))]
      (if (= b 0x7F)
        acc
        (let [t   (bit-shift-right b 5)
              idx (bit-and b 31)]
          (recur (conj acc (case (int t)
                             0 [idx :byte (long (.readByte buf))]
                             1 [idx :short (long (.readShort buf))]
                             2 [idx :int (long (.readInt buf))]
                             3 [idx :float (double (.readFloat buf))]
                             4 [idx :string (read-string buf)]
                             (throw (ex-info "Unsupported metadata type" {:type t :index idx}))))))))))

(defn write-metadata [^ByteBuf buf entries]
  (doseq [[idx type value] entries]
    (.writeByte buf (unchecked-int (bit-or (bit-shift-left (long (meta-type-id type)) 5)
                                           (bit-and (long idx) 31))))
    (case type
      :byte   (.writeByte buf (unchecked-int (long value)))
      :short  (.writeShort buf (unchecked-int (long value)))
      :int    (.writeInt buf (unchecked-int (long value)))
      :float  (.writeFloat buf (unchecked-float (double value)))
      :string (write-string buf value)
      :slot   (write-slot buf value)))
  (.writeByte buf 0x7F))

(defn read-angle ^double [^ByteBuf buf]
  (/ (* (long (.readByte buf)) 360.0) 256.0))

(defn write-angle [^ByteBuf buf deg]
  (.writeByte buf (unchecked-int (long (* (double deg) (/ 256.0 360.0))))))
