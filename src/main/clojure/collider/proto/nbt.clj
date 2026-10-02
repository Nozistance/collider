(ns collider.proto.nbt
  "Binary NBT values on the wire."
  (:require [collider.proto.buf :as buf])
  (:import (collider.proto Buf)))

(set! *warn-on-reflection* true)

(def ^:private tags
  [:end :byte :short :int :long :float :double :byte-array :string
   :list :compound :int-array :long-array])

(def ^:private tag-ids (zipmap tags (range)))

(defn- tag-id ^long [kind]
  (long (tag-ids kind)))

(defn- tag-kind [^long t]
  (when (< -1 t (count tags)) (nth tags t)))

(defn- nbt-type [v]
  (cond (map? v) :compound
        (string? v) :string
        (boolean? v) :byte
        (instance? Byte v) :byte
        (instance? Short v) :short
        (instance? Long v) :long
        (instance? Float v) :float
        (instance? Double v) :double
        (instance? byte/1 v) :byte-array
        (instance? int/1 v) :int-array
        (instance? long/1 v) :long-array
        (integer? v) :int
        (vector? v) :list
        :else (throw (ex-info "no NBT type" {:value v}))))

(defn- list-type ^long [v]
  (long (or (:nbt-type (meta v))
            (if (empty? v) 0 (tag-id (nbt-type (first v)))))))

(declare write-payload)

(defn- write-compound [^Buf buf m]
  (doseq [[k x] m :when (some? x)]
    (let [kind (nbt-type x)]
      (buf/write-byte! buf (tag-id kind))
      (buf/write-utf! buf (name k))
      (write-payload buf kind x)))
  (buf/write-byte! buf 0))

(defn- write-ints [^Buf buf ^ints v]
  (buf/write-int! buf (alength v))
  (dotimes [i (alength v)]
    (buf/write-int! buf (aget v i))))

(defn- write-longs [^Buf buf ^longs v]
  (buf/write-int! buf (alength v))
  (dotimes [i (alength v)]
    (buf/write-long! buf (aget v i))))

(defn- write-list [^Buf buf v]
  (buf/write-byte! buf (list-type v))
  (buf/write-int! buf (count v))
  (doseq [x v] (write-payload buf (nbt-type x) x)))

(defn- write-byte-value [^Buf buf v]
  (buf/write-byte! buf (int (if (boolean? v) (if v 1 0) ^Byte v))))

(defn- write-payload [^Buf buf kind v]
  (case kind
    :compound (write-compound buf v)
    :string (buf/write-utf! buf ^String v)
    :byte (write-byte-value buf v)
    :short (buf/write-short! buf (int ^Short v))
    :long (buf/write-long! buf (long v))
    :float (buf/write-float! buf (float v))
    :double (buf/write-double! buf (double v))
    :byte-array (do (buf/write-int! buf (alength ^bytes v))
                    (buf/write-bytes! buf ^bytes v))
    :int-array (write-ints buf v)
    :long-array (write-longs buf v)
    :int (buf/write-int! buf (int v))
    :list (write-list buf v)))

(defn write-nbt
  "Writes the NBT value v, nil as an empty tag."
  [^Buf buf v]
  (if (nil? v)
    (buf/write-byte! buf 0)
    (let [kind (nbt-type v)]
      (buf/write-byte! buf (tag-id kind))
      (write-payload buf kind v))))

(def ^:private ^:const quota 2097152)

(def ^:private ^:const max-depth 512)

(defn- account! [^longs acc ^long size]
  (when (neg? size)
    (throw (ex-info "negative NBT size" {:size size})))
  (let [used (+ (aget acc 0) size)]
    (when (> used quota)
      (throw (ex-info "NBT tag too big"
                      {:usage used :quota quota})))
    (aset acc 0 used)))

(defn- push-depth! [^longs acc]
  (when (>= (aget acc 1) max-depth)
    (throw (ex-info "NBT tag too complex" {:max-depth max-depth})))
  (aset acc 1 (inc (aget acc 1))))

(defn- pop-depth! [^longs acc]
  (aset acc 1 (dec (aget acc 1))))

(defn- read-string* ^String [^Buf buf ^longs acc ^long base]
  (account! acc base)
  (let [s (buf/read-utf buf)]
    (account! acc (* 2 (count s)))
    s))

(declare read-payload)

(defn- read-list [^Buf buf ^longs acc]
  (push-depth! acc)
  (try
    (account! acc 36)
    (let [et (buf/read-byte buf)
          n (buf/read-int buf)]
      (account! acc (* 4 n))
      (with-meta (mapv (fn [_] (read-payload buf acc et)) (range n))
                 {:nbt-type et}))
    (finally (pop-depth! acc))))

(defn- read-entries [^Buf buf ^longs acc]
  (loop [entries []]
    (let [et (buf/read-byte buf)]
      (if (zero? et)
        (apply array-map (apply concat entries))
        (let [nm (read-string* buf acc 28)
              v (read-payload buf acc et)]
          (account! acc 36)
          (recur (conj entries [(keyword nm) v])))))))

(defn- read-compound [^Buf buf ^longs acc]
  (push-depth! acc)
  (try
    (account! acc 48)
    (read-entries buf acc)
    (finally (pop-depth! acc))))

(defn- read-bytes [^Buf buf ^longs acc]
  (account! acc 24)
  (let [n (buf/read-int buf)]
    (account! acc n)
    (let [b (byte-array n)] (buf/read-bytes! buf b) b)))

(defn- read-ints [^Buf buf ^longs acc]
  (account! acc 24)
  (let [n (buf/read-int buf)]
    (account! acc (* 4 n))
    (let [a (int-array n)]
      (dotimes [i n] (aset a i (int (buf/read-int buf))))
      a)))

(defn- read-longs [^Buf buf ^longs acc]
  (account! acc 24)
  (let [n (buf/read-int buf)]
    (account! acc (* 8 n))
    (let [a (long-array n)]
      (dotimes [i n] (aset a i (buf/read-long buf)))
      a)))

(defn- read-number [^Buf buf ^longs acc kind]
  (case kind
    :byte (do (account! acc 9)
              (Byte/valueOf (byte (buf/read-byte buf))))
    :short (do (account! acc 10)
               (Short/valueOf (short (buf/read-short buf))))
    :int (do (account! acc 12)
             (Integer/valueOf (int (buf/read-int buf))))
    :long (do (account! acc 16) (Long/valueOf (buf/read-long buf)))
    :float (do (account! acc 12)
               (Float/valueOf (float (buf/read-float buf))))
    :double (do (account! acc 16)
                (Double/valueOf (buf/read-double buf)))))

(defn- read-payload [^Buf buf ^longs acc ^long t]
  (case (tag-kind t)
    (:byte :short :int :long :float :double)
    (read-number buf acc (tag-kind t))
    :byte-array (read-bytes buf acc)
    :string (read-string* buf acc 36)
    :list (read-list buf acc)
    :compound (read-compound buf acc)
    :int-array (read-ints buf acc)
    :long-array (read-longs buf acc)
    (throw (ex-info "unknown NBT tag" {:tag t}))))

(defn read-nbt
  "Returns the NBT value at the read point, compound keys as keywords.
  Throws when it is too big or too deeply nested."
  [^Buf buf]
  (let [acc (long-array 2)
        t (buf/read-byte buf)]
    (when-not (zero? t) (read-payload buf acc t))))
