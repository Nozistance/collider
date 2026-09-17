(ns collider.proto.buf
  "Byte buffers of the wire format."
  (:import (collider.java Buf)))

(set! *warn-on-reflection* true)

(defn buf ^Buf [^long capacity]
  (Buf. (int capacity)))

(defn write-byte! [^Buf b ^long v]
  (.writeByte b (int v)))

(defn write-boolean! [^Buf b v]
  (.writeBoolean b (boolean v)))

(defn write-short! [^Buf b ^long v]
  (.writeShort b (int v)))

(defn write-int! [^Buf b ^long v]
  (.writeInt b (int v)))

(defn write-long! [^Buf b ^long v]
  (.writeLong b v))

(defn write-float! [^Buf b ^double v]
  (.writeFloat b (float v)))

(defn write-double! [^Buf b ^double v]
  (.writeDouble b v))

(defn write-bytes!
  ([^Buf b src]
   (if (instance? Buf src)
     (.writeBytes b ^Buf src)
     (.writeBytes b ^bytes src)))
  ([^Buf b ^bytes src ^long off ^long len]
   (.writeBytes b src (int off) (int len))))

(defn write-utf! [^Buf b ^String s]
  (.writeUtf b s))

(defn read-byte ^long [^Buf b]
  (long (.readByte b)))

(defn read-boolean [^Buf b]
  (.readBoolean b))

(defn read-unsigned-byte ^long [^Buf b]
  (long (.readUnsignedByte b)))

(defn read-short ^long [^Buf b]
  (long (.readShort b)))

(defn read-unsigned-short ^long [^Buf b]
  (long (.readUnsignedShort b)))

(defn read-int ^long [^Buf b]
  (long (.readInt b)))

(defn read-long ^long [^Buf b]
  (.readLong b))

(defn read-float ^double [^Buf b]
  (.readFloat b))

(defn read-double ^double [^Buf b]
  (.readDouble b))

(defn read-bytes! [^Buf b ^bytes dst]
  (.readBytes b dst))

(defn readable-bytes ^long [^Buf b]
  (long (.readableBytes b)))

(defn clear!
  "Resets read and write position; two-arg form also shrinks the
   backing array down to keep bytes when it grew larger."
  ([^Buf b]
   (.clear b))
  ([^Buf b ^long keep]
   (.clear b (int keep))))

(defn ensure!
  "Grows the buffer so n more bytes can be written without resize."
  [^Buf b ^long n]
  (.ensure b (int n)))

(defn adopt!
  "Replaces the backing array with src, read at 0 and write at len."
  [^Buf b ^bytes src ^long len]
  (.adopt b src (int len)))

(defn read-from! [^Buf b in ^long n]
  (.readFrom b in (int n)))

(defn write-to! [^Buf b out]
  (.writeTo b out))
