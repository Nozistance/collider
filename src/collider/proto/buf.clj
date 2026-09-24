(ns collider.proto.buf
  "Byte buffers of the wire format."
  (:import (collider.java Buf)
           (java.io ByteArrayInputStream InputStream)
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

(defn peek-bytes
  "Copies up to n unread bytes without moving the read point."
  ^bytes [^Buf b ^long n]
  (let [n (min n (long (.readableBytes b)))
        dst (byte-array n)]
    (System/arraycopy (.-a b) (.-r b) dst 0 n)
    dst))

(defn clear!
  "Resets the read and write position.
  The two-arg form also shrinks the backing array down to keep
  bytes when it grew larger."
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

(defn unread-stream
  "Returns a stream over the unread bytes. The read point stays."
  ^InputStream [^Buf b]
  (ByteArrayInputStream. (.-a b) (.-r b) (.readableBytes b)))

(defn leave-unread!
  "Moves the read point so only the last n written bytes stay unread."
  [^Buf b ^long n]
  (set! (.-r b) (int (- (.-w b) n))))

(defn inflate-input!
  "Gives the unread bytes to the inflater. The read point stays."
  [^Buf b ^Inflater i]
  (.setInput i (.-a b) (.-r b) (.readableBytes b)))

(defn deflate-input!
  "Gives the unread bytes to the deflater and marks them read."
  [^Buf b ^Deflater d]
  (.setInput d (.-a b) (.-r b) (.readableBytes b))
  (set! (.-r b) (.-w b)))

(defn deflate!
  "Writes what the deflater gives into the free room of the buffer."
  [^Buf b ^Deflater d]
  (let [a (.-a b)
        w (.-w b)
        k (.deflate d a w (- (alength a) w))]
    (set! (.-w b) (int (+ w k)))))
