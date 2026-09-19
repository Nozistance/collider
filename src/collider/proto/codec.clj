(ns collider.proto.codec
  "Wire primitives of the protocol.
  Varints, NBT, item stacks and framing live here."
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.proto.buf :as buf])
  (:import (collider.java Buf)
           (java.io ByteArrayInputStream DataInputStream
                    EOFException InputStream OutputStream)
           (java.nio.charset StandardCharsets)
           (java.util UUID)
           (java.util.zip Deflater Inflater)))

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

(defn read-string
  "Returns the string at the read point. Throws when it is longer than
  max characters."
  (^String [^Buf buf] (read-string buf max-string-length))
  (^String [^Buf buf max]
   (let [max (long max)
         n (read-varint buf)]
     (when (or (neg? n) (> n (* max 3)))
       (throw (ex-info "encoded string too long"
                       {:length n :max (* max 3)})))
     (let [bs (byte-array n)]
       (buf/read-bytes! buf bs)
       (let [s (String. bs StandardCharsets/UTF_8)]
         (when (> (.length s) max)
           (let [info {:length (.length s) :max max}]
             (throw (ex-info "string too long" info))))
         s)))))

(defn read-count ^long [^Buf buf]
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

(defn write-id [^Buf buf k]
  (write-string buf
                (cond
                  (keyword? k) (data/wire k)
                  (str/includes? (str k) ":") (str k)
                  :else (str "minecraft:" k))))

(def ^:private ^:const tag-end 0)

(def ^:private ^:const tag-byte 1)

(def ^:private ^:const tag-short 2)

(def ^:private ^:const tag-int 3)

(def ^:private ^:const tag-long 4)

(def ^:private ^:const tag-float 5)

(def ^:private ^:const tag-double 6)

(def ^:private ^:const tag-byte-array 7)

(def ^:private ^:const tag-string 8)

(def ^:private ^:const tag-list 9)

(def ^:private ^:const tag-compound 10)

(def ^:private ^:const tag-int-array 11)

(def ^:private ^:const tag-long-array 12)

(def ^:private byte-array-class (Class/forName "[B"))

(def ^:private int-array-class (Class/forName "[I"))

(def ^:private long-array-class (Class/forName "[J"))

(defn- write-nbt-string [^Buf buf ^String name ^String v]
  (buf/write-byte! buf (int tag-string))
  (buf/write-utf! buf name)
  (buf/write-utf! buf v))

(declare write-translatable)

(defn- write-argument [^Buf buf a]
  (if (map? a)
    (write-translatable buf a)
    (do (write-nbt-string buf "text" (str a))
        (buf/write-byte! buf (int tag-end)))))

(defn- write-translatable [^Buf buf {:keys [translate with]}]
  (write-nbt-string buf "translate" translate)
  (when (seq with)
    (buf/write-byte! buf (int tag-list)) (buf/write-utf! buf "with")
    (cond
      (every? number? with)
      (do (buf/write-byte! buf (int tag-int))
          (buf/write-int! buf (count with))
          (doseq [a with] (buf/write-int! buf (int a))))
      (every? string? with)
      (do (buf/write-byte! buf (int tag-string))
          (buf/write-int! buf (count with))
          (doseq [a with] (buf/write-utf! buf a)))
      :else
      (do (buf/write-byte! buf (int tag-compound))
          (buf/write-int! buf (count with))
          (doseq [a with] (write-argument buf a)))))
  (buf/write-byte! buf (int tag-end)))

(defn write-component
  "Writes a piece of text a client shows, plain or translated."
  [^Buf buf s]
  (if (map? s)
    (do (buf/write-byte! buf (int tag-compound))
        (write-translatable buf s))
    (do (buf/write-byte! buf (int tag-string))
        (buf/write-utf! buf (str s)))))

(defn- nbt-type ^long [v]
  (cond (map? v) tag-compound
        (string? v) tag-string
        (boolean? v) tag-byte
        (instance? Byte v) tag-byte
        (instance? Short v) tag-short
        (instance? Long v) tag-long
        (instance? Float v) tag-float
        (instance? Double v) tag-double
        (.isInstance ^Class byte-array-class v) tag-byte-array
        (.isInstance ^Class int-array-class v) tag-int-array
        (.isInstance ^Class long-array-class v) tag-long-array
        (integer? v) tag-int
        (vector? v) tag-list
        :else (throw (ex-info "no NBT type" {:value v}))))

(defn- list-type ^long [v]
  (long (or (:nbt-type (meta v))
            (if (empty? v) tag-end (nbt-type (first v))))))

(defn- write-nbt-payload [^Buf buf v]
  (case (int (nbt-type v))
    10 (do (doseq [[k x] v :when (some? x)]
             (buf/write-byte! buf (int (nbt-type x)))
             (buf/write-utf! buf (name k))
             (write-nbt-payload buf x))
           (buf/write-byte! buf (int tag-end)))
    8 (buf/write-utf! buf ^String v)
    1 (buf/write-byte! buf (int (if (boolean? v) (if v 1 0) ^Byte v)))
    2 (buf/write-short! buf (int ^Short v))
    4 (buf/write-long! buf (long v))
    5 (buf/write-float! buf (float v))
    6 (buf/write-double! buf (double v))
    7 (do (buf/write-int! buf (alength ^bytes v))
          (buf/write-bytes! buf ^bytes v))
    11 (do (buf/write-int! buf (alength ^ints v))
           (dotimes [i (alength ^ints v)]
             (buf/write-int! buf (aget ^ints v i))))
    12 (do (buf/write-int! buf (alength ^longs v))
           (dotimes [i (alength ^longs v)]
             (buf/write-long! buf (aget ^longs v i))))
    3 (buf/write-int! buf (int v))
    9 (do (buf/write-byte! buf (int (list-type v)))
          (buf/write-int! buf (count v))
          (doseq [x v] (write-nbt-payload buf x)))))

(defn write-nbt [^Buf buf v]
  (if (nil? v)
    (buf/write-byte! buf (int tag-end))
    (do (buf/write-byte! buf (int (nbt-type v)))
        (write-nbt-payload buf v))))

(def ^:private ^:const nbt-quota 2097152)

(def ^:private ^:const nbt-max-depth 512)

(defn- account! [^longs acc ^long size]
  (when (neg? size)
    (throw (ex-info "negative NBT size" {:size size})))
  (let [used (+ (aget acc 0) size)]
    (when (> used nbt-quota)
      (throw (ex-info "NBT tag too big"
                      {:usage used :quota nbt-quota})))
    (aset acc 0 used)))

(defn- push-depth! [^longs acc]
  (when (>= (aget acc 1) nbt-max-depth)
    (throw (ex-info "NBT tag too complex"
                    {:max-depth nbt-max-depth})))
  (aset acc 1 (inc (aget acc 1))))

(defn- pop-depth! [^longs acc]
  (aset acc 1 (dec (aget acc 1))))

(defn- read-nbt-string
  ^String [^DataInputStream d ^longs acc ^long base]
  (account! acc base)
  (let [s (.readUTF d)]
    (account! acc (* 2 (.length s)))
    s))

(declare read-nbt-payload)

(defn- read-nbt-list [^DataInputStream d ^longs acc]
  (push-depth! acc)
  (try
    (account! acc 36)
    (let [et (long (.readByte d))
          n (long (.readInt d))]
      (account! acc (* 4 n))
      (with-meta (mapv (fn [_] (read-nbt-payload d acc et)) (range n))
                 {:nbt-type et}))
    (finally (pop-depth! acc))))

(defn- read-nbt-compound [^DataInputStream d ^longs acc]
  (push-depth! acc)
  (try
    (account! acc 48)
    (loop [entries []]
      (let [et (long (.readByte d))]
        (if (zero? et)
          (apply array-map (apply concat entries))
          (let [nm (read-nbt-string d acc 28)
                v (read-nbt-payload d acc et)]
            (account! acc 36)
            (recur (conj entries [(keyword nm) v]))))))
    (finally (pop-depth! acc))))

(defn- read-nbt-bytes [^DataInputStream d ^longs acc]
  (account! acc 24)
  (let [n (long (.readInt d))]
    (account! acc n)
    (let [b (byte-array n)] (.readFully d b) b)))

(defn- read-nbt-ints [^DataInputStream d ^longs acc]
  (account! acc 24)
  (let [n (long (.readInt d))]
    (account! acc (* 4 n))
    (let [a (int-array n)]
      (dotimes [i n] (aset a i (.readInt d)))
      a)))

(defn- read-nbt-longs [^DataInputStream d ^longs acc]
  (account! acc 24)
  (let [n (long (.readInt d))]
    (account! acc (* 8 n))
    (let [a (long-array n)]
      (dotimes [i n] (aset a i (.readLong d)))
      a)))

(defn- read-nbt-payload [^DataInputStream d ^longs acc ^long t]
  (case (int t)
    1 (do (account! acc 9) (Byte/valueOf (.readByte d)))
    2 (do (account! acc 10) (Short/valueOf (.readShort d)))
    3 (do (account! acc 12) (Integer/valueOf (.readInt d)))
    4 (do (account! acc 16) (Long/valueOf (.readLong d)))
    5 (do (account! acc 12) (Float/valueOf (.readFloat d)))
    6 (do (account! acc 16) (Double/valueOf (.readDouble d)))
    7 (read-nbt-bytes d acc)
    8 (read-nbt-string d acc 36)
    9 (read-nbt-list d acc)
    10 (read-nbt-compound d acc)
    11 (read-nbt-ints d acc)
    12 (read-nbt-longs d acc)
    (throw (ex-info "unknown NBT tag" {:tag t}))))

(defn read-nbt
  "Returns the NBT value at the read point, compound keys as keywords.
  Throws when it is too big or too deeply nested."
  [^Buf buf]
  (let [left (- (.w buf) (.r buf))
        in (ByteArrayInputStream. (.a buf) (.r buf) left)
        d (DataInputStream. in)
        acc (long-array 2)
        t (long (.readByte d))
        v (when-not (zero? t) (read-nbt-payload d acc t))]
    (set! (.r buf) (- (.w buf) (.available in)))
    v))

(defn write-angle [^Buf buf ^double deg]
  (buf/write-byte!
    buf (unchecked-int (Math/floor (/ (* deg 256.0) 360.0)))))

(defn read-angle
  "Returns in degrees the rotation one byte carries."
  ^double [^Buf buf]
  (/ (* (buf/read-byte buf) 360.0) 256.0))

(defn write-vec3 [^Buf buf [x y z]]
  (buf/write-double! buf (double x))
  (buf/write-double! buf (double y))
  (buf/write-double! buf (double z)))

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
  "Writes one block of a section update: where it sits in the section
  and its state, both in one varlong."
  [^Buf buf [at state]]
  (let [v (bit-or (bit-shift-left (long state) 12) (long at))]
    (write-varlong buf v)))

(defn read-section-change [^Buf buf]
  (let [v (read-varlong buf)]
    [(bit-and v 0xFFF) (bit-shift-right v 12)]))

(defn- lp-pack ^long [^double v]
  (Math/round (* (+ (* v 0.5) 0.5) 32766.0)))

(defn write-lp-vec3 [^Buf buf [x y z]]
  (let [x (double x) y (double y) z (double z)
        m (max (Math/abs x) (Math/abs y) (Math/abs z))]
    (if (< m 3.051944088384301E-5)
      (buf/write-byte! buf 0)
      (let [scale (long (Math/ceil m))
            partial? (not= (bit-and scale 3) scale)
            markers (if partial? (bit-or (bit-and scale 3) 4) scale)
            px (bit-shift-left (lp-pack (/ x scale)) 3)
            py (bit-shift-left (lp-pack (/ y scale)) 18)
            pz (bit-shift-left (lp-pack (/ z scale)) 33)
            buffer (bit-or markers px py pz)]
        (buf/write-byte! buf (unchecked-int buffer))
        (buf/write-byte!
          buf (unchecked-int (bit-shift-right buffer 8)))
        (buf/write-int!
          buf (unchecked-int (bit-shift-right buffer 16)))
        (when partial?
          (write-varint buf (bit-shift-right scale 2)))))))

(defn- lp-unpack ^double [^long v]
  (- (/ (* 2.0 (min (bit-and v 32767) 32766)) 32766.0) 1.0))

(defn read-lp-vec3
  "Reads a vector quantized to a direction and a whole scale."
  [^Buf buf]
  (let [lowest (buf/read-unsigned-byte buf)]
    (if (zero? lowest)
      [0.0 0.0 0.0]
      (let [middle (buf/read-unsigned-byte buf)
            high (bit-and (buf/read-int buf) 0xFFFFFFFF)
            v (bit-or (bit-shift-left high 16)
                      (bit-shift-left middle 8) lowest)
            more (fn [s]
                   (let [n (bit-and (read-varint buf) 0xFFFFFFFF)]
                     (bit-or s (bit-shift-left n 2))))
            wide? (pos? (bit-and lowest 4))
            scale (cond-> (bit-and lowest 3) wide? more)]
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

(defn write-bits [^Buf buf ^long v ^long n]
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

(defn section-pos ^long [^long sx ^long sy ^long sz]
  (bit-or (bit-shift-left (bit-and sx 0x3FFFFF) 42)
          (bit-shift-left (bit-and sz 0x3FFFFF) 20)
          (bit-and sy 0xFFFFF)))

(defn read-section-pos [^Buf buf]
  (let [v (buf/read-long buf)]
    [(bit-shift-right v 42)
     (bit-shift-right (bit-shift-left v 44) 44)
     (bit-shift-right (bit-shift-left v 22) 42)]))

(defn write-list [^Buf buf xs f]
  (write-varint buf (count xs))
  (doseq [x xs] (f buf x)))

(defn write-holder-ref [^Buf buf ^long id]
  (write-varint buf (inc id)))

(defn read-holder-ref ^long [^Buf buf]
  (dec (read-varint buf)))

(defn read-id [^Buf buf]
  (data/kebab (read-string buf)))

(defn- stat-registry [type]
  (case type
    :custom "custom_stat"
    :mined "block"
    (:killed :killed-by) "entity_type"
    "item"))

(defn write-stat
  "Writes a statistic: its type, then its entry in the registry the
  type names."
  [^Buf buf k]
  (let [type (keyword (namespace k))
        reg (stat-registry type)]
    (write-varint buf (data/registry-id "stat_type" type))
    (write-varint buf (data/registry-id reg (keyword (name k))))))

(defn read-stat [^Buf buf]
  (let [type (data/entry-name "stat_type" (read-varint buf))
        reg (stat-registry type)
        entry (data/entry-name reg (read-varint buf))]
    (keyword (name type) (name entry))))

(declare components read-patch write-patch)

(defn- codec [r w] {:r r :w w})

(def ^:private c-bool
  (codec (fn [^Buf b] (buf/read-boolean b))
         (fn [^Buf b v] (buf/write-boolean! b (boolean v)))))

(def ^:private c-varint
  (codec (fn [^Buf b] (read-varint b))
         (fn [^Buf b v] (write-varint b (long v)))))

(def ^:private c-int
  (codec (fn [^Buf b] (long (buf/read-int b)))
         (fn [^Buf b v] (buf/write-int! b (int v)))))

(def ^:private c-float
  (codec (fn [^Buf b] (buf/read-float b))
         (fn [^Buf b v] (buf/write-float! b (float v)))))

(def ^:private c-double
  (codec (fn [^Buf b] (buf/read-double b))
         (fn [^Buf b v] (buf/write-double! b (double v)))))

(def ^:private c-string
  (codec (fn [^Buf b] (read-string b))
         (fn [^Buf b v] (write-string b (str v)))))

(def ^:private c-ident (codec read-id (fn [^Buf b v] (write-id b v))))

(def ^:private c-uuid
  (codec (fn [^Buf b] (read-uuid b))
         (fn [^Buf b v] (write-uuid b v))))

(def ^:private c-nbt
  (codec (fn [^Buf b] (read-nbt b))
         (fn [^Buf b v] (write-nbt b v))))

(def ^:private c-text c-nbt)

(def ^:private c-unit (codec (fn [^Buf _] true) (fn [^Buf _ _] nil)))

(def ^:private c-block-pos
  (codec (fn [^Buf b] (read-block-pos b))
         (fn [^Buf b [x y z]]
           (write-block-pos b (long x) (long y) (long z)))))

(defn- c-opt [{:keys [r w]}]
  (codec (fn [^Buf b] (when (buf/read-boolean b) (r b)))
         (fn [^Buf b v]
           (buf/write-boolean! b (some? v))
           (when (some? v) (w b v)))))

(defn- c-list [{:keys [r w]}]
  (codec (fn [^Buf b]
           (let [n (read-count b)] (mapv (fn [_] (r b)) (range n))))
         (fn [^Buf b v]
           (write-varint b (count v))
           (doseq [x v] (w b x)))))

(defn- c-map [k v]
  (codec (fn [^Buf b]
           (let [n (read-count b)
                 pair (fn [_] [((:r k) b) ((:r v) b)])]
             (apply array-map (mapcat pair (range n)))))
         (fn [^Buf b m]
           (write-varint b (count m))
           (doseq [[a x] m] ((:w k) b a) ((:w v) b x)))))

(defn- c-either [l r]
  (codec (fn [^Buf b]
           (if (buf/read-boolean b)
             {:left ((:r l) b)}
             {:right ((:r r) b)}))
         (fn [^Buf b v]
           (if (contains? v :left)
             (do (buf/write-boolean! b true) ((:w l) b (:left v)))
             (do (buf/write-boolean! b false)
                 ((:w r) b (:right v)))))))

(defn- record-codec [& kvs]
  (let [fields (mapv vec (partition 2 kvs))
        ks (mapv first fields)]
    (codec (fn [^Buf b]
             (let [vs (mapv (fn [[_ c]] ((:r c) b)) fields)]
               (apply array-map (interleave ks vs))))
           (fn [^Buf b v]
             (doseq [[k c] fields] ((:w c) b (get v k)))))))

(defn- c-enum [names]
  (let [by-id (vec names)
        by-name (into {} (map-indexed (fn [i n] [n (long i)])) names)]
    (codec (fn [^Buf b] (let [i (read-varint b)] (get by-id i i)))
           (fn [^Buf b v]
             (write-varint
               b (long (if (keyword? v) (get by-name v) v)))))))

(defn- c-reg [registry]
  (codec (fn [^Buf b] (data/entry-name registry (read-varint b)))
         (fn [^Buf b v] (write-varint b (data/entry-id registry v)))))

(defn- c-holder [registry direct]
  (codec (fn [^Buf b]
           (let [i (read-varint b)]
             (if (zero? i)
               {:direct ((:r direct) b)}
               (data/entry-name registry (dec i)))))
         (fn [^Buf b v]
           (if (map? v)
             (do (write-varint b 0) ((:w direct) b (:direct v)))
             (write-varint b (inc (data/entry-id registry v)))))))

(defn- c-holder-set [registry]
  (codec (fn [^Buf b]
           (let [n (dec (read-count b))]
             (if (neg? n)
               {:tag (read-id b)}
               (mapv (fn [_]
                       (data/entry-name registry (read-varint b)))
                     (range n)))))
         (fn [^Buf b v]
           (if (map? v)
             (do (write-varint b 0) (write-id b (:tag v)))
             (do (write-varint b (inc (count v)))
                 (doseq [x v]
                   (write-varint b (data/entry-id registry x))))))))

(defn- c-filterable [inner]
  (record-codec :raw inner :filtered (c-opt inner)))

(def ^:private dye-colors
  [:white :orange :magenta :light-blue :yellow :lime :pink :gray
   :light-gray :cyan :purple :blue :brown :green :red :black])

(def ^:private c-dye (c-enum dye-colors))

(def ^:private c-sound
  (c-holder "sound_event"
            (record-codec :sound c-ident :range (c-opt c-float))))

(def ^:private c-effect-details
  (let [self (promise)
        nested (codec (fn [^Buf b] ((:r @self) b))
                      (fn [^Buf b v] ((:w @self) b v)))
        details (record-codec
                 :amplifier c-varint :duration c-varint
                 :ambient c-bool :show-particles c-bool
                 :show-icon c-bool :hidden (c-opt nested))]
    @(deliver self details)))

(def ^:private c-effect-instance
  (record-codec :effect (c-reg "mob_effect")
                :details c-effect-details))

(def ^:private c-apply-effects
  (record-codec :effects (c-list c-effect-instance)
                :probability c-float))

(def ^:private consume-effects
  {:apply-effects c-apply-effects
   :remove-effects (record-codec :effects (c-holder-set "mob_effect"))
   :clear-all-effects (record-codec)
   :teleport-randomly (record-codec :diameter c-float)
   :play-sound        (record-codec :sound c-sound)})

(def ^:private c-consume-effect
  (codec (fn [^Buf b]
           (let [i (read-varint b)
                 t (data/entry-name "consume_effect_type" i)]
             (assoc ((:r (get consume-effects t)) b) :type t)))
         (fn [^Buf b v]
           (write-varint
             b (data/entry-id "consume_effect_type" (:type v)))
           ((:w (get consume-effects (:type v))) b v))))

(def ^:private c-patch
  (codec (fn [^Buf b] (read-patch b))
         (fn [^Buf b v] (write-patch b v))))

(def ^:private c-template
  (record-codec :item (c-reg "item") :count c-varint
                :patch c-patch))

(def ^:private c-typed-component
  (codec (fn [^Buf b]
           (let [i (read-varint b)
                 t (data/entry-name "data_component_type" i)]
             [t ((:r (get components t)) b)]))
         (fn [^Buf b [t v]]
           (write-varint b (data/entry-id "data_component_type" t))
           ((:w (get components t)) b v))))

(def ^:private no-predicates
  "component predicates not supported")

(def ^:private c-no-predicates
  (codec (fn [^Buf b]
           (let [n (read-varint b)]
             (when (pos? n)
               (throw (ex-info no-predicates {:count n})))
             []))
         (fn [^Buf b v]
           (when (seq v) (throw (ex-info no-predicates {})))
           (write-varint b 0))))

(def ^:private c-state-range
  (record-codec :min (c-opt c-string) :max (c-opt c-string)))

(def ^:private c-state-matcher
  (c-either (record-codec :value c-string) c-state-range))

(def ^:private c-state-entry
  (record-codec :name c-string :matcher c-state-matcher))

(def ^:private c-block-predicate
  (record-codec :blocks (c-opt (c-holder-set "block"))
                :state (c-opt (c-list c-state-entry))
                :nbt (c-opt c-nbt)
                :exact (c-list c-typed-component)
                :partial c-no-predicates))

(def ^:private c-adventure
  (record-codec :predicates (c-list c-block-predicate)))

(def ^:private c-attribute-display
  (let [types {0 (record-codec) 1 (record-codec)
               2 (record-codec :text c-text)}
        rd (fn [^Buf b]
             (let [t (read-varint b)]
               (assoc ((:r (get types t)) b) :display t)))
        wr (fn [^Buf b v]
             (let [t (long (:display v 0))]
               (write-varint b t)
               ((:w (get types t)) b v)))]
    (codec rd wr)))

(def ^:private attribute-operations
  [:add-value :add-multiplied-base :add-multiplied-total])

(def ^:private attribute-slots
  [:any :mainhand :offhand :hand :feet :legs :chest :head :armor
   :body :saddle])

(def ^:private c-attribute-modifier
  (record-codec :id c-ident :amount c-double
                :operation (c-enum attribute-operations)))

(def ^:private c-attribute-entry
  (record-codec :attribute (c-reg "attribute")
                :modifier c-attribute-modifier
                :slot (c-enum attribute-slots)
                :display c-attribute-display))

(def ^:private c-tool-rule
  (record-codec :blocks (c-holder-set "block") :speed (c-opt c-float)
                :correct-for-drops (c-opt c-bool)))

(def ^:private c-damage-reduction
  (record-codec :horizontal-blocking-angle c-float
                :type (c-opt (c-holder-set "damage_type"))
                :base c-float :factor c-float))

(def ^:private c-kinetic-condition
  (record-codec :max-duration-ticks c-varint :min-speed c-float
                :min-relative-speed c-float))

(def ^:private firework-shapes
  [:small-ball :large-ball :star :creeper :burst])

(def ^:private c-firework-explosion
  (record-codec :shape (c-enum firework-shapes)
                :colors (c-list c-int) :fade-colors (c-list c-int)
                :trail c-bool :twinkle c-bool))

(def ^:private c-game-profile-properties
  (c-list (record-codec :name c-string :value c-string
                        :signature (c-opt c-string))))

(def ^:private c-named-profile
  (record-codec :id c-uuid :name c-string
                :properties c-game-profile-properties))

(def ^:private c-partial-profile
  (record-codec :name (c-opt c-string) :id (c-opt c-uuid)
                :properties c-game-profile-properties))

(def ^:private c-skin
  (record-codec :body (c-opt c-ident) :cape (c-opt c-ident)
                :elytra (c-opt c-ident)
                :model (c-opt (c-enum [:wide :slim]))))

(def ^:private c-profile
  (record-codec :profile (c-either c-named-profile c-partial-profile)
                :skin c-skin))

(def ^:private c-typed-entity-data
  (fn [type-codec] (record-codec :type type-codec :data c-nbt)))

(def ^:private c-instrument-data
  (record-codec :sound c-sound :use-duration c-float
                :range c-float :description c-text))

(def ^:private c-instrument (c-holder "instrument" c-instrument-data))

(def ^:private c-material-assets
  (record-codec :base c-string
                :overrides (c-map c-ident c-string)))

(def ^:private c-trim-material-data
  (record-codec :assets c-material-assets :description c-text))

(def ^:private c-trim-material
  (c-holder "trim_material" c-trim-material-data))

(def ^:private c-trim-pattern-data
  (record-codec :asset c-ident :description c-text :decal c-bool))

(def ^:private c-trim-pattern
  (c-holder "trim_pattern" c-trim-pattern-data))

(def ^:private c-banner-pattern-data
  (record-codec :asset c-ident :translation-key c-string))

(def ^:private c-banner-pattern
  (c-holder "banner_pattern" c-banner-pattern-data))

(def ^:private c-jukebox-song-data
  (record-codec :sound c-sound :description c-text
                :length c-float :comparator-output c-varint))

(def ^:private c-jukebox-song
  (c-holder "jukebox_song" c-jukebox-song-data))

(def ^:private c-painting-variant-data
  (record-codec :width c-varint :height c-varint :asset c-ident
                :title (c-opt c-text) :author (c-opt c-text)))

(def ^:private c-painting-variant
  (c-holder "painting_variant" c-painting-variant-data))

(def ^:private c-use-effects
  (record-codec :can-sprint c-bool :interact-vibrations c-bool
                :speed-multiplier c-float))

(def ^:private c-custom-model-data
  (record-codec :floats (c-list c-float) :flags (c-list c-bool)
                :strings (c-list c-string) :colors (c-list c-int)))

(def ^:private c-tooltip-display
  (record-codec :hide-tooltip c-bool
                :hidden (c-list (c-reg "data_component_type"))))

(def ^:private c-food
  (record-codec :nutrition c-varint :saturation c-float
                :can-always-eat c-bool))

(def ^:private consume-animations
  [:none :eat :drink :block :bow :trident :crossbow :spyglass
   :toot-horn :brush :bundle :spear])

(def ^:private c-consumable
  (record-codec :seconds c-float
                :animation (c-enum consume-animations)
                :sound c-sound :particles c-bool
                :on-consume (c-list c-consume-effect)))

(def ^:private c-tool
  (record-codec :rules (c-list c-tool-rule)
                :default-mining-speed c-float
                :damage-per-block c-varint
                :destroy-in-creative c-bool))

(def ^:private c-weapon
  (record-codec :damage-per-attack c-varint
                :disable-blocking-seconds c-float))

(def ^:private c-attack-range
  (record-codec :min-reach c-float :max-reach c-float
                :min-creative-reach c-float
                :max-creative-reach c-float
                :hitbox-margin c-float :mob-factor c-float))

(def ^:private equipment-slots
  [:mainhand :feet :legs :chest :head :offhand :body :saddle])

(def ^:private c-equippable
  (record-codec :slot (c-enum equipment-slots)
                :equip-sound c-sound
                :asset (c-opt c-ident)
                :camera-overlay (c-opt c-ident)
                :allowed-entities (c-opt (c-holder-set "entity_type"))
                :dispensable c-bool :swappable c-bool
                :damage-on-hurt c-bool :equip-on-interact c-bool
                :can-be-sheared c-bool :shearing-sound c-sound))

(def ^:private c-item-damage
  (record-codec :threshold c-float :base c-float :factor c-float))

(def ^:private c-blocks-attacks
  (record-codec :block-delay-seconds c-float
                :disable-cooldown-scale c-float
                :damage-reductions (c-list c-damage-reduction)
                :item-damage c-item-damage
                :bypassed-by (c-opt (c-holder-set "damage_type"))
                :block-sound (c-opt c-sound)
                :disable-sound (c-opt c-sound)))

(def ^:private c-piercing-weapon
  (record-codec :deals-knockback c-bool :dismounts c-bool
                :sound (c-opt c-sound) :hit-sound (c-opt c-sound)))

(def ^:private c-kinetic-weapon
  (record-codec :contact-cooldown-ticks c-varint
                :delay-ticks c-varint
                :dismount (c-opt c-kinetic-condition)
                :knockback (c-opt c-kinetic-condition)
                :damage (c-opt c-kinetic-condition)
                :forward-movement c-float :damage-multiplier c-float
                :sound (c-opt c-sound) :hit-sound (c-opt c-sound)))

(def ^:private c-swing-animation
  (record-codec :type (c-enum [:none :whack :stab])
                :duration c-varint))

(def ^:private c-potion-contents
  (record-codec :potion (c-opt (c-reg "potion"))
                :custom-color (c-opt c-int)
                :custom-effects (c-list c-effect-instance)
                :custom-name (c-opt c-string)))

(def ^:private c-stew-effect
  (record-codec :effect (c-reg "mob_effect") :duration c-varint))

(def ^:private c-written-book
  (record-codec :title (c-filterable c-string) :author c-string
                :generation c-varint
                :pages (c-list (c-filterable c-text))
                :resolved c-bool))

(def ^:private c-lodestone-target
  (record-codec :dimension c-ident :pos c-block-pos))

(def ^:private c-lodestone-tracker
  (record-codec :target (c-opt c-lodestone-target) :tracked c-bool))

(def ^:private c-fireworks
  (record-codec :flight-duration c-varint
                :explosions (c-list c-firework-explosion)))

(def ^:private c-bee
  (record-codec :data (c-typed-entity-data (c-reg "entity_type"))
                :ticks-in-hive c-varint
                :min-ticks-in-hive c-varint))

(def ^:private c-use-cooldown
  (record-codec :seconds c-float :group (c-opt c-ident)))

(def ^:private c-damage-resistant
  (record-codec :types (c-holder-set "damage_type")))

(def ^:private c-death-protection
  (record-codec :death-effects (c-list c-consume-effect)))

(def ^:private c-trim
  (record-codec :material c-trim-material :pattern c-trim-pattern))

(def ^:private c-block-entity-data
  (c-typed-entity-data (c-reg "block_entity_type")))

(def ^:private c-banner-patterns
  (c-list (record-codec :pattern c-banner-pattern :color c-dye)))

(def components
  {:custom-data                 c-nbt
   :max-stack-size c-varint
   :max-damage c-varint
   :damage c-varint
   :unbreakable c-unit
   :use-effects c-use-effects
   :custom-name c-text
   :minimum-attack-charge c-float
   :damage-type (c-reg "damage_type")
   :item-name c-text
   :item-model c-ident
   :lore (c-list c-text)
   :rarity (c-enum [:common :uncommon :rare :epic])
   :enchantments (c-map (c-reg "enchantment") c-varint)
   :can-place-on c-adventure
   :can-break c-adventure
   :attribute-modifiers (c-list c-attribute-entry)
   :custom-model-data c-custom-model-data
   :tooltip-display c-tooltip-display
   :repair-cost c-varint
   :creative-slot-lock c-unit
   :enchantment-glint-override c-bool
   :intangible-projectile c-nbt
   :food c-food
   :consumable c-consumable
   :use-remainder (record-codec :convert-into c-template)
   :use-cooldown c-use-cooldown
   :damage-resistant c-damage-resistant
   :tool c-tool
   :weapon c-weapon
   :attack-range c-attack-range
   :enchantable c-varint
   :equippable c-equippable
   :repairable (record-codec :items (c-holder-set "item"))
   :glider c-unit
   :tooltip-style c-ident
   :death-protection c-death-protection
   :blocks-attacks c-blocks-attacks
   :piercing-weapon c-piercing-weapon
   :kinetic-weapon c-kinetic-weapon
   :swing-animation c-swing-animation
   :additional-trade-cost c-varint
   :stored-enchantments (c-map (c-reg "enchantment") c-varint)
   :dye c-dye
   :dyed-color c-int
   :map-color c-int
   :map-id c-varint
   :map-decorations c-nbt
   :map-post-processing (c-enum [:lock :scale])
   :charged-projectiles (c-list c-template)
   :bundle-contents (c-list c-template)
   :potion-contents c-potion-contents
   :potion-duration-scale c-float
   :suspicious-stew-effects (c-list c-stew-effect)
   :writable-book-content (c-list (c-filterable c-string))
   :written-book-content c-written-book
   :trim c-trim
   :debug-stick-state c-nbt
   :entity-data (c-typed-entity-data (c-reg "entity_type"))
   :bucket-entity-data c-nbt
   :block-entity-data c-block-entity-data
   :instrument c-instrument
   :provides-trim-material c-trim-material
   :ominous-bottle-amplifier c-varint
   :jukebox-playable (record-codec :song c-jukebox-song)
   :provides-banner-patterns (c-holder-set "banner_pattern")
   :recipes c-nbt
   :lodestone-tracker c-lodestone-tracker
   :firework-explosion c-firework-explosion
   :fireworks c-fireworks
   :profile c-profile
   :note-block-sound c-ident
   :banner-patterns c-banner-patterns
   :base-color c-dye
   :pot-decorations (c-list (c-reg "item"))
   :container (c-list (c-opt c-template))
   :block-state (c-map c-string c-string)
   :bees (c-list c-bee)
   :sulfur-cube-content (record-codec :absorbed c-template)
   :lock c-nbt
   :container-loot c-nbt
   :break-sound c-sound
   :villager/variant (c-reg "villager_type")
   :wolf/variant (c-reg "wolf_variant")
   :wolf/sound-variant (c-reg "wolf_sound_variant")
   :wolf/collar c-dye
   :fox/variant c-varint
   :salmon/size c-varint
   :parrot/variant c-varint
   :tropical-fish/pattern c-varint
   :tropical-fish/base-color c-dye
   :tropical-fish/pattern-color c-dye
   :mooshroom/variant c-varint
   :rabbit/variant c-varint
   :pig/variant (c-reg "pig_variant")
   :pig/sound-variant (c-reg "pig_sound_variant")
   :cow/variant (c-reg "cow_variant")
   :cow/sound-variant (c-reg "cow_sound_variant")
   :chicken/variant (c-reg "chicken_variant")
   :chicken/sound-variant (c-reg "chicken_sound_variant")
   :zombie-nautilus/variant (c-reg "zombie_nautilus_variant")
   :frog/variant (c-reg "frog_variant")
   :horse/variant c-varint
   :painting/variant c-painting-variant
   :llama/variant c-varint
   :axolotl/variant c-varint
   :cat/variant (c-reg "cat_variant")
   :cat/sound-variant (c-reg "cat_sound_variant")
   :cat/collar c-dye
   :sheep/color c-dye
   :shulker/color c-dye})

(defn- component-codec [kw]
  (or (get components kw)
      (throw (ex-info "no codec for data component"
                      {:component kw}))))

(defn- read-component
  "Reads one component; delimited? means a byte length precedes the
  value, as in the untrusted stack of set-creative-mode-slot."
  [^Buf buf delimited?]
  (let [k (data/entry-name "data_component_type" (read-varint buf))]
    (when delimited? (read-varint buf))
    [k ((:r (component-codec k)) buf)]))

(defn- read-removed [^Buf buf]
  (data/entry-name "data_component_type" (read-varint buf)))

(defn read-patch
  ([^Buf buf] (read-patch buf false))
  ([^Buf buf delimited?]
   (let [added (read-count buf)
         removed (read-count buf)]
     (if (and (zero? added) (zero? removed))
       nil
       (let [one (fn [_] (read-component buf delimited?))
             cs (mapv one (range added))
             rs (mapv (fn [_] (read-removed buf)) (range removed))
             m (apply array-map (apply concat cs))]
         (cond-> {}
                 (seq cs) (assoc :components m)
                 (seq rs) (assoc :removed (set rs))))))))

(defn write-patch [^Buf buf patch]
  (let [cs (:components patch)
        id-of #(data/entry-id "data_component_type" %)
        rs (sort-by id-of (:removed patch))]
    (write-varint buf (count cs))
    (write-varint buf (count rs))
    (doseq [[k v] cs]
      (write-varint buf (data/entry-id "data_component_type" k))
      ((:w (component-codec k)) buf v))
    (doseq [k rs] (write-varint buf (id-of k)))))

(defn write-item-stack [^Buf buf stack]
  (if (nil? stack)
    (write-varint buf 0)
    (do (write-varint buf (long (:count stack 1)))
        (write-varint buf (data/registry-id "item" (:item stack)))
        (write-patch buf stack))))

(defn read-item-stack
  "Reads an optional stack. The client's creative stack is the
  untrusted codec: every component value is length-prefixed."
  ([^Buf buf] (read-item-stack buf false))
  ([^Buf buf delimited?]
   (let [n (read-varint buf)]
     (when (pos? n)
       (let [item (data/entry-name "item" (read-varint buf))]
         (merge {:item item :count n}
                (read-patch buf delimited?)))))))

(defn read-hashed-stack
  "Returns the stack the client claims is in a slot.
  An empty slot gives nil. Component values arrive as hashes, so the
  result only says whether the stack has any."
  [^Buf buf]
  (when (buf/read-boolean buf)
    (let [item (read-varint buf)
          n (read-varint buf)
          added (read-count buf)]
      (dotimes [_ added] (read-varint buf) (buf/read-int buf))
      (let [removed (read-count buf)]
        (dotimes [_ removed] (read-varint buf))
        (cond-> {:item (data/entry-name "item" item) :count n}
                (or (pos? (long added)) (pos? (long removed)))
                (assoc :components? true))))))

(def data-types
  "Entity data type -> its place in the serializer order."
  {:byte 0 :int 1 :float 3 :item 7 :boolean 8 :block-pos 10
   :optional-block-pos 11 :block-state 14 :particle 16 :pose 20
   :cow-variant 23 :cow-sound-variant 24})

(defn- write-data-pos [^Buf buf v]
  (let [[x y z] v]
    (write-block-pos buf (long x) (long y) (long z))))

(defn- write-data-value [^Buf buf type v]
  (case type
    :byte (buf/write-byte! buf (int v))
    :int (write-varint buf (long v))
    :float (buf/write-float! buf (float v))
    :item (write-item-stack buf v)
    :boolean (buf/write-boolean! buf (boolean v))
    :block-pos (write-data-pos buf v)
    :optional-block-pos
    (do (buf/write-boolean! buf (some? v))
        (when v (write-data-pos buf v)))
    :block-state (write-varint buf (long v))
    :particle (let [[t c] v]
                (write-varint buf (long t))
                (buf/write-int! buf (int c)))
    :pose (write-varint buf (long v))
    (:cow-variant :cow-sound-variant) (write-varint buf (long v))))

(defn write-entity-data [^Buf buf entries]
  (doseq [[idx type v] entries]
    (buf/write-byte! buf (int idx))
    (write-varint buf (data-types type))
    (write-data-value buf type v))
  (buf/write-byte! buf 0xFF))

(def ^:private data-type-names
  (into {} (map (fn [[k v]] [(long v) k])) data-types))

(defn- read-optional-pos [^Buf buf]
  (when (buf/read-boolean buf) (read-block-pos buf)))

(defn- read-data-value [^Buf buf type]
  (case type
    :byte (buf/read-byte buf)
    :int (read-varint buf)
    :float (buf/read-float buf)
    :item (read-item-stack buf)
    :boolean (buf/read-boolean buf)
    :block-pos (read-block-pos buf)
    :optional-block-pos (read-optional-pos buf)
    :particle [(read-varint buf) (buf/read-int buf)]
    (:block-state :pose :cow-variant :cow-sound-variant)
    (read-varint buf)))

(def ^:private ^:const entity-data-end 255)

(defn read-entity-data
  "Reads the tracked fields of an entity up to the end marker."
  [^Buf buf]
  (loop [out []]
    (let [idx (buf/read-unsigned-byte buf)]
      (if (= entity-data-end idx)
        out
        (let [type (data-type-names (read-varint buf))
              v (read-data-value buf type)]
          (recur (conj out [idx type v])))))))

(defn offline-uuid ^UUID [^String name]
  (let [s (str "OfflinePlayer:" name)]
    (UUID/nameUUIDFromBytes (.getBytes s StandardCharsets/UTF_8))))

(def ^:private ^:const max-uncompressed 8388608)

(defn- read-varint-stream ^long [^InputStream in]
  (loop [n 0 acc 0]
    (let [b (.read in)]
      (when (neg? b) (throw (EOFException. "end of stream")))
      (let [acc (bit-or acc
                        (bit-shift-left (bit-and b 0x7F) (* n 7)))]
        (cond
          (zero? (bit-and b 0x80)) acc
          (>= n 2) (throw (ex-info "frame length varint too long" {}))
          :else (recur (inc n) acc))))))

(def ^:private ^:const frame-keep 8192)

(defn read-frame! ^Buf [^InputStream in ^Buf buf]
  (let [len (read-varint-stream in)]
    (buf/clear! buf frame-keep)
    (buf/read-from! buf in len)
    buf))

(defn- bad-frame [info]
  (ex-info "badly compressed packet" info))

(defn- inflate! [^Buf buf ^Inflater inflater ^long n]
  (let [dst (byte-array n)]
    (.setInput inflater (.a buf) (.r buf) (buf/readable-bytes buf))
    (let [got (try (.inflate inflater dst)
                   (finally (.reset inflater)))]
      (when (not= got n)
        (throw (bad-frame {:got got :expected n}))))
    (buf/adopt! buf dst n)))

(defn decompress! ^Buf [^Buf buf ^long threshold ^Inflater inflater]
  (when-not (neg? threshold)
    (let [n (read-varint buf)]
      (when (pos? n)
        (when (< n threshold)
          (throw (bad-frame {:size n :threshold threshold})))
        (when (> n max-uncompressed)
          (throw (bad-frame {:size n :max max-uncompressed})))
        (inflate! buf inflater n))))
  buf)

(def ^:private ^:const deflate-step 8192)

(defn- deflate-into! [^Buf body ^Deflater deflater]
  (loop []
    (buf/ensure! body deflate-step)
    (let [a (.a body)
          k (.deflate deflater a (.w body) (- (alength a) (.w body)))]
      (set! (.w body) (+ (.w body) k))
      (when-not (.finished deflater) (recur)))))

(defn write-frame!
  "Writes the payload to the stream as one packet. body and head are
  scratch buffers."
  [^OutputStream out ^Buf payload ^Buf body ^Buf head threshold
   ^Deflater deflater ^bytes chunk]
  (buf/clear! body)
  (buf/clear! head)
  (if (neg? (long threshold))
    (buf/write-bytes! body payload)
    (let [n (buf/readable-bytes payload)]
      (if (< n (long threshold))
        (do (write-varint body 0) (buf/write-bytes! body payload))
        (do (write-varint body n)
            (.setInput deflater (.a payload) (.r payload) n)
            (set! (.r payload) (.w payload))
            (.finish deflater)
            (deflate-into! body deflater)
            (.reset deflater)))))
  (write-varint head (buf/readable-bytes body))
  (buf/write-to! head out)
  (buf/write-to! body out))
