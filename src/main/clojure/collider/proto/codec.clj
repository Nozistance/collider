(ns collider.proto.codec
  "Wire primitives of the protocol."
  (:refer-clojure :exclude [read-string])
  (:require [collider.data :as data]
            [collider.proto.buf :as buf])
  (:import (collider.proto Buf)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(def protocol-version 776)

(def game-version data/game)

(defn write-varint [^Buf buf v]
  (loop [v (bit-and (long v) 0xFFFFFFFF)]
    (if (zero? (bit-and v (bit-not 0x7F)))
      (buf/write-byte! buf (unchecked-int v))
      (do (buf/write-byte!
            buf (unchecked-int (bit-or (bit-and v 0x7F) 0x80)))
          (recur (unsigned-bit-shift-right v 7))))))

(def ^:private ^:const max-varint-size 5)

(defn read-varint ^long [^Buf buf]
  (loop [n 0 r 0]
    (let [b (long (buf/read-byte buf))
          r (bit-or r (bit-shift-left (bit-and b 0x7F) (* 7 n)))]
      (cond
        (zero? (bit-and b 0x80)) (long (unchecked-int r))
        (>= (inc n) max-varint-size)
        (throw (ex-info "VarInt too big" {:bytes (inc n)}))
        :else (recur (inc n) r)))))

(defn write-varlong [^Buf buf ^long v]
  (loop [v v]
    (if (zero? (bit-and v (bit-not 0x7F)))
      (buf/write-byte! buf (int v))
      (do (buf/write-byte! buf (int (bit-or (bit-and v 0x7F) 0x80)))
          (recur (unsigned-bit-shift-right v 7))))))

(def ^:private ^:const max-varlong-size 10)

(defn read-varlong ^long [^Buf buf]
  (loop [n 0 r 0]
    (let [b (long (buf/read-byte buf))
          r (bit-or r (bit-shift-left (bit-and b 0x7F) (* 7 n)))]
      (cond
        (zero? (bit-and b 0x80)) r
        (>= (inc n) max-varlong-size)
        (throw (ex-info "VarLong too big" {:bytes (inc n)}))
        :else (recur (inc n) r)))))

(defn write-string [^Buf buf ^String s]
  (let [bs (.getBytes s StandardCharsets/UTF_8)]
    (write-varint buf (alength bs))
    (buf/write-bytes! buf bs)))

(def ^:const max-string-length 32767)

(defn- read-utf8 ^String [^Buf buf ^long max]
  (let [n (read-varint buf)]
    (when (or (neg? n) (> n (* max 3)))
      (throw (ex-info "encoded string too long"
                      {:length n :max (* max 3)})))
    (let [bs (byte-array n)]
      (buf/read-bytes! buf bs)
      (String. bs StandardCharsets/UTF_8))))

(defn read-string
  "Returns the string at the read point.
  Throws when it has more than max characters."
  (^String [^Buf buf] (read-string buf max-string-length))
  (^String [^Buf buf max]
   (let [max (long max)
         ^String s (read-utf8 buf max)]
     (when (> (count s) max)
       (throw (ex-info "string too long"
                       {:length (count s) :max max})))
     s)))

(defn read-count
  "Returns the count at the read point.
  Throws when it is more than the unread bytes."
  ^long [^Buf buf]
  (let [n (read-varint buf)]
    (when (or (neg? n) (> n (buf/readable-bytes buf)))
      (throw (ex-info "count exceeds remaining bytes"
                      {:count n
                       :readable (buf/readable-bytes buf)})))
    n))

(defn write-uuid [^Buf buf ^UUID u]
  (buf/write-long! buf (.getMostSignificantBits u))
  (buf/write-long! buf (.getLeastSignificantBits u)))

(defn read-uuid ^UUID [^Buf buf]
  (UUID. (buf/read-long buf) (buf/read-long buf)))

(defn write-id
  "Writes a registry name, in the default namespace when it has none."
  [^Buf buf k]
  (write-string buf (if (keyword? k)
                      (data/wire k)
                      (data/full-id (str k)))))

(defn write-angle
  "Writes a rotation in degrees as one byte."
  [^Buf buf ^double deg]
  (buf/write-byte!
    buf (unchecked-int (Math/floor (/ (* deg 256.0) 360.0)))))

(defn read-angle
  "Returns the rotation at the read point in degrees."
  ^double [^Buf buf]
  (/ (* (buf/read-byte buf) 360.0) 256.0))

(defn write-vec3 [^Buf buf [x y z]]
  (buf/write-double! buf (double x))
  (buf/write-double! buf (double y))
  (buf/write-double! buf (double z)))

(defn read-vec3 [^Buf buf]
  [(buf/read-double buf) (buf/read-double buf) (buf/read-double buf)])

(defn write-fixed-vec3
  "Writes a position as three ints of eighths of a block."
  [^Buf buf [x y z]]
  (buf/write-int! buf (long (* 8.0 (double x))))
  (buf/write-int! buf (long (* 8.0 (double y))))
  (buf/write-int! buf (long (* 8.0 (double z)))))

(defn read-fixed-vec3 [^Buf buf]
  [(/ (buf/read-int buf) 8.0) (/ (buf/read-int buf) 8.0)
   (/ (buf/read-int buf) 8.0)])

(defn write-section-change
  "Writes one block change of a section update."
  [^Buf buf [at state]]
  (let [v (bit-or (bit-shift-left (long state) 12) (long at))]
    (write-varlong buf v)))

(defn read-section-change [^Buf buf]
  (let [v (read-varlong buf)]
    [(bit-and v 0xFFF) (bit-shift-right v 12)]))

(defn- lp-pack ^long [^double v]
  (Math/round (* (+ (* v 0.5) 0.5) 32766.0)))

(defn- lp-partial? [^long scale]
  (not= (bit-and scale 3) scale))

(defn- lp-bits
  ^long [^double x ^double y ^double z ^long scale]
  (let [markers (if (lp-partial? scale)
                  (bit-or (bit-and scale 3) 4)
                  scale)]
    (bit-or markers
            (bit-shift-left (lp-pack (/ x scale)) 3)
            (bit-shift-left (lp-pack (/ y scale)) 18)
            (bit-shift-left (lp-pack (/ z scale)) 33))))

(defn- write-lp-bits [^Buf buf ^long bits ^long scale]
  (buf/write-byte! buf (unchecked-int bits))
  (buf/write-byte! buf (unchecked-int (bit-shift-right bits 8)))
  (buf/write-int! buf (unchecked-int (bit-shift-right bits 16)))
  (when (lp-partial? scale)
    (write-varint buf (bit-shift-right scale 2))))

(def ^:private ^:const lp-zero
  "The size under which the wire sends a vector as zero."
  3.051944088384301E-5)

(defn write-lp-vec3
  "Writes a vector quantized to a direction and a whole scale."
  [^Buf buf [x y z]]
  (let [x (double x) y (double y) z (double z)
        m (max (Math/abs x) (Math/abs y) (Math/abs z))]
    (if (< m lp-zero)
      (buf/write-byte! buf 0)
      (let [scale (long (Math/ceil m))]
        (write-lp-bits buf (lp-bits x y z scale) scale)))))

(defn- lp-unpack ^double [^long v]
  (- (/ (* 2.0 (min (bit-and v 32767) 32766)) 32766.0) 1.0))

(defn- read-lp-bits ^long [^Buf buf ^long lowest]
  (let [middle (buf/read-unsigned-byte buf)
        high (bit-and (buf/read-int buf) 0xFFFFFFFF)]
    (bit-or (bit-shift-left high 16)
            (bit-shift-left middle 8)
            lowest)))

(defn- read-lp-scale ^long [^Buf buf ^long lowest]
  (let [scale (bit-and lowest 3)]
    (if (pos? (bit-and lowest 4))
      (let [n (bit-and (read-varint buf) 0xFFFFFFFF)]
        (bit-or scale (bit-shift-left n 2)))
      scale)))

(defn read-lp-vec3
  "Reads a vector quantized to a direction and a whole scale."
  [^Buf buf]
  (let [lowest (buf/read-unsigned-byte buf)]
    (if (zero? lowest)
      [0.0 0.0 0.0]
      (let [v (read-lp-bits buf lowest)
            scale (read-lp-scale buf lowest)]
        [(* scale (lp-unpack (bit-shift-right v 3)))
         (* scale (lp-unpack (bit-shift-right v 18)))
         (* scale (lp-unpack (bit-shift-right v 33)))]))))

(defn read-bits
  "Reads a bit set of n bytes, lowest byte first, as an integer."
  ^long [^Buf buf ^long n]
  (loop [i 0 v 0]
    (if (= i n)
      v
      (let [b (buf/read-unsigned-byte buf)]
        (recur (inc i) (bit-or v (bit-shift-left b (* 8 i))))))))

(defn write-bits
  "Writes the n low bytes of v, lowest byte first."
  [^Buf buf ^long v ^long n]
  (dotimes [i n]
    (buf/write-byte! buf (bit-and (bit-shift-right v (* 8 i)) 0xFF))))

(defn write-block-pos [^Buf buf ^long x ^long y ^long z]
  (let [v (bit-or (bit-shift-left (bit-and x 0x3FFFFFF) 38)
                  (bit-shift-left (bit-and z 0x3FFFFFF) 12)
                  (bit-and y 0xFFF))]
    (buf/write-long! buf v)))

(defn read-block-pos [^Buf buf]
  (let [v (buf/read-long buf)]
    [(bit-shift-right v 38)
     (bit-shift-right (bit-shift-left v 52) 52)
     (bit-shift-right (bit-shift-left v 26) 38)]))

(defn section-pos
  "Returns the section position as one long."
  ^long [^long sx ^long sy ^long sz]
  (bit-or (bit-shift-left (bit-and sx 0x3FFFFF) 42)
          (bit-shift-left (bit-and sz 0x3FFFFF) 20)
          (bit-and sy 0xFFFFF)))

(defn read-section-pos [^Buf buf]
  (let [v (buf/read-long buf)]
    [(bit-shift-right v 42)
     (bit-shift-right (bit-shift-left v 44) 44)
     (bit-shift-right (bit-shift-left v 22) 42)]))

(defn write-list
  "Writes the count of xs and each of them with f."
  [^Buf buf xs f]
  (write-varint buf (count xs))
  (doseq [x xs] (f buf x)))

(defn write-holder-ref
  "Writes a registry entry by reference to its id."
  [^Buf buf ^long id]
  (write-varint buf (inc id)))

(defn read-holder-ref
  "Returns the id of a registry entry given by reference."
  ^long [^Buf buf]
  (dec (read-varint buf)))

(defn read-id [^Buf buf]
  (let [s (read-string buf)
        k (data/kebab s)]
    (if (= (data/full-id s) (data/wire k)) k s)))

(defn- stat-registry [type]
  (case type
    :custom "custom_stat"
    :mined "block"
    (:killed :killed-by) "entity_type"
    "item"))

(defn write-stat
  "Writes a statistic as its type and its entry.
  The type names the registry of the entry."
  [^Buf buf k]
  (let [type (keyword (namespace k))
        reg (stat-registry type)]
    (write-varint buf (data/registry-id "stat_type" type))
    (write-varint buf (data/registry-id reg (keyword (name k))))))

(defn read-stat
  "Returns the statistic at the read point.
  Its type is the namespace of the keyword."
  [^Buf buf]
  (let [type (data/entry-name "stat_type" (read-varint buf))
        reg (stat-registry type)
        entry (data/entry-name reg (read-varint buf))]
    (keyword (name type) (name entry))))
