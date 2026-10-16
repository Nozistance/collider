(ns collider.proto.buf
  "Growable byte sequences of the wire format, big end first."
  (:import (collider.proto Buf)
           (java.io InputStream OutputStream)
           (java.util.zip Deflater Inflater)))

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
  "Writes the bytes of src, a byte array or a Buf. A Buf src ends with
  nothing unread."
  ([^Buf b src]
   (if (instance? Buf src)
     (.writeBytes b ^Buf src)
     (.writeBytes b ^bytes src)))
  ([^Buf b ^bytes src ^long off ^long len]
   (.writeBytes b src (int off) (int len))))

(defn write-utf!
  "Writes s as a two-byte length and its chars in the modified UTF-8
  of NBT. Throws past 65535 bytes."
  [^Buf b ^String s]
  (.writeModifiedUtf b s))

(defn read-byte ^long [^Buf b]
  (long (.readByte b)))

(defn read-utf
  "Returns the string in the modified UTF-8 of NBT at the read point.
  Throws on a malformed or cut char."
  ^String [^Buf b]
  (.readModifiedUtf b))

(defn read-boolean
  "Returns true for any byte but 0."
  [^Buf b]
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

(defn peek-bytes
  "Returns up to n unread bytes without moving the read point."
  ^bytes [^Buf b ^long n]
  (.peek b (int n)))

(defn clear!
  "Empties b. With keep, b gives back the room above keep bytes."
  ([^Buf b]
   (.clear b))
  ([^Buf b ^long keep]
   (.clear b (int keep))))

(defn ensure!
  "Makes room in b for n more bytes."
  [^Buf b ^long n]
  (.ensure b (int n)))

(defn adopt!
  "Makes the first len bytes of src the unread content of b. b takes
  src itself, not a copy."
  [^Buf b ^bytes src ^long len]
  (.adopt b src (int len)))

(defn read-from!
  "Reads exactly n bytes from in into b. Throws when in ends first."
  [^Buf b ^InputStream in ^long n]
  (.readFrom b in (int n)))

(defn write-to!
  "Writes the unread bytes of b to out and marks them read."
  [^Buf b ^OutputStream out]
  (.writeTo b out))

(defn inflate-input!
  "Gives the unread bytes of b to inflater. The read point stays."
  [^Buf b ^Inflater inflater]
  (.inflateInput b inflater))

(defn deflate-input!
  "Gives the unread bytes of b to deflater and marks them read."
  [^Buf b ^Deflater deflater]
  (.deflateInput b deflater))

(defn deflate!
  "Writes what deflater gives into the free room of b. That room does
  not grow."
  [^Buf b ^Deflater deflater]
  (.deflate b deflater))
