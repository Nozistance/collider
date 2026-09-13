(ns collider.proto.chunk
  (:require [collider.data :as data]
            [collider.proto.codec :as c]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.world.chunk Section)
           (collider.java Buf)
           (java.util Arrays)))

(set! *warn-on-reflection* true)

(def ^:const light-sections 26)
(defn- pack-longs ^longs [^long bits ^ints values]
  (let [per (quot 64 bits)
        len (alength values)
        n (long (Math/ceil (/ (double len) per)))
        out (long-array n)]
    (dotimes [i len]
      (let [at (quot (long i) per)
            off (* (rem (long i) per) bits)]
        (aset out at (bit-or (aget out at)
                             (bit-shift-left (long (aget values i)) off)))))
    out))

(defn- ceillog2 ^long [^long n]
  (- 32 (Integer/numberOfLeadingZeros (int (dec (max 1 n))))))

(def ^:private global-block-bits (ceillog2 data/block-state-count))
(def ^:private global-biome-bits (ceillog2 (count (get data/datapack "worldgen/biome"))))

(defn- state-flag? [^long st ^long bit]
  (pos? (bit-and (long (get data/flags st 0)) bit)))

(defn- fluid? [^long st]
  (or (block/liquid? st) (block/waterlogged? st)))

(defn- motion-blocking? [^long st]
  (or (state-flag? st 1) (fluid? st)))

(defn- state-table ^booleans [pred]
  (let [a (boolean-array data/block-state-count)]
    (dotimes [i data/block-state-count]
      (aset a i (boolean (pred i))))
    a))

(def ^:private surface-arr (state-table (fn [^long st] (not= :air (block/type-of st)))))
(def ^:private motion-arr (state-table motion-blocking?))
(def ^:private no-leaves-arr
  (state-table (fn [^long st] (and (motion-blocking? st) (not (block/leaves? st))))))
(def ^:private fluid-arr (state-table fluid?))

(def ^:private ^:const palette-slots 1024)
(def ^:private ^:const palette-max 256)
(def ^:private ^:const block-bits-floor 4)
(def ^:private plains (data/datapack-id "worldgen/biome" :plains))

(defn- pack-into! [^Buf buf ^long bits ^ints values ^long len]
  (let [per (quot 64 bits)]
    (loop [i 0]
      (when (< i len)
        (let [end (min len (+ i per))]
          (loop [j i acc 0 off 0]
            (if (< j end)
              (recur (inc j) (bit-or acc (bit-shift-left (long (aget values j)) off)) (+ off bits))
              (.writeLong buf acc)))
          (recur (+ i per)))))))

(defn- write-global! [^Buf buf ^shorts bs ^long bits]
  (let [per (quot 64 bits)]
    (.writeByte buf (int bits))
    (loop [i 0]
      (when (< i 4096)
        (let [end (min 4096 (+ i per))]
          (loop [j i acc 0 off 0]
            (if (< j end)
              (recur (inc j)
                     (bit-or acc (bit-shift-left (bit-and (long (aget bs j)) 0xFFFF) off))
                     (+ off bits))
              (.writeLong buf acc)))
          (recur (+ i per)))))))

(def ^:private ^:const palette-at palette-slots)
(def ^:private ^:const counts-at (+ palette-slots palette-max 1))
(def ^:private ^:const scratch-size (+ palette-slots (* 2 (inc palette-max))))

(defn- palette-index ^long [^ints scratch ^long id ^long n]
  (loop [h (bit-and id (dec palette-slots))]
    (let [v (long (aget scratch (int h)))]
      (cond
        (neg? v) (do (aset scratch (int h) (int n))
                     (aset scratch (int (+ palette-at n)) (int id))
                     (- -1 n))
        (= id (long (aget scratch (int (+ palette-at v))))) v
        :else (recur (bit-and (inc h) (dec palette-slots)))))))

(defn- section-palette ^long [^shorts bs ^ints idx ^ints scratch]
  (Arrays/fill scratch 0 palette-slots (int -1))
  (Arrays/fill scratch counts-at scratch-size (int 0))
  (loop [i 0 n 0]
    (if (= i 4096)
      n
      (let [v (palette-index scratch (bit-and (long (aget bs i)) 0xFFFF) n)
            at (int (if (neg? v) (- -1 v) v))
            n (if (neg? v) (inc n) n)]
        (aset idx i at)
        (aset scratch (+ counts-at at) (unchecked-inc-int (aget scratch (+ counts-at at))))
        (if (> n palette-max) -1 (recur (inc i) n))))))

(defn- write-palette! [^Buf buf ^ints idx ^ints scratch ^long n]
  (let [bits (max block-bits-floor (ceillog2 n))]
    (.writeByte buf (int bits))
    (c/write-varint buf n)
    (dotimes [i n] (c/write-varint buf (long (aget scratch (+ palette-at (int i))))))
    (pack-into! buf bits idx 4096)))

(defn- tallied ^long [^ints scratch ^long n ^booleans pred]
  (loop [i 0 acc 0]
    (if (= i n)
      acc
      (let [id (long (aget scratch (+ palette-at (int i))))]
        (recur (inc i) (if (and (< id (alength pred)) (aget pred (int id)))
                         (+ acc (long (aget scratch (+ counts-at (int i)))))
                         acc))))))

(defn- air-tally ^long [^ints scratch ^long n]
  (loop [i 0]
    (cond
      (= i n) 0
      (= (long (aget scratch (+ palette-at i))) (long block/air))
      (long (aget scratch (+ counts-at i)))
      :else (recur (inc i)))))

(defn- raw-tally ^longs [^shorts bs]
  (let [out (long-array 2)
        ^booleans fluids fluid-arr]
    (dotimes [i 4096]
      (let [id (bit-and (long (aget bs i)) 0xFFFF)]
        (when-not (= id (long block/air)) (aset out 0 (inc (aget out 0))))
        (when (and (< id (alength fluids)) (aget fluids (int id)))
          (aset out 1 (inc (aget out 1))))))
    out))

(defn- write-biomes! [^Buf buf]
  (.writeByte buf 0)
  (c/write-varint buf (long plains)))

(defn- write-section! [^Buf buf ^Section s ^ints idx ^ints scratch]
  (let [^shorts bs (.blocks s)
        n (section-palette bs idx scratch)]
    (if (neg? n)
      (let [^longs t (raw-tally bs)]
        (.writeShort buf (int (aget t 0)))
        (.writeShort buf (int (aget t 1)))
        (write-global! buf bs global-block-bits))
      (do (.writeShort buf (int (- 4096 (air-tally scratch n))))
          (.writeShort buf (int (tallied scratch n fluid-arr)))
          (if (= n 1)
            (do (.writeByte buf 0) (c/write-varint buf (long (aget scratch palette-at))))
            (write-palette! buf idx scratch n))))
    (write-biomes! buf)))

(defn- write-empty-section! [^Buf buf]
  (.writeShort buf 0)
  (.writeShort buf 0)
  (.writeByte buf 0) (c/write-varint buf block/air)
  (write-biomes! buf))

(def ^:private full-light (byte-array 2048 (unchecked-byte 0xFF)))
(defn- light-mask ^long [pred]
  (loop [i 0 m 0]
    (if (= i light-sections) m (recur (inc i) (if (pred i) (bit-or m (bit-shift-left 1 i)) m)))))

(defn- our-section [chunk ^long si]
  (when (< -1 si chunk/section-count) (get (:sections chunk) si)))

(def ^:private height-bits (ceillog2 (+ 2 (- chunk/max-y chunk/min-y))))

(defn- section-hit ^long [^Section s ^booleans pred ^long c]
  (let [^shorts bs (.blocks s)
        n (alength pred)]
    (loop [y 15]
      (if (neg? y)
        -1
        (let [id (bit-and (long (aget bs (+ (* y 256) c))) 0xFFFF)]
          (if (and (< id n) (aget pred (int id))) y (recur (dec y))))))))

(defn- column-height ^long [chunk ^booleans pred ^long c]
  (loop [si (dec (long chunk/section-count))]
    (if (neg? si)
      0
      (let [^Section s (our-section chunk si)
            y (if (nil? s) -1 (section-hit s pred c))]
        (if (neg? y) (recur (dec si)) (+ (* si 16) y 1))))))

(defn- heightmap-longs ^longs [chunk ^booleans pred]
  (let [out (int-array 256)]
    (dotimes [c 256] (aset out c (int (column-height chunk pred c))))
    (pack-longs height-bits out)))

(def ^:private client-heightmaps [[1 surface-arr] [4 motion-arr] [5 no-leaves-arr]])

(defn- write-heightmaps! [^Buf buf chunk]
  (c/write-varint buf (count client-heightmaps))
  (doseq [[id pred] client-heightmaps]
    (c/write-varint buf (long id))
    (let [^longs ls (heightmap-longs chunk pred)]
      (c/write-varint buf (alength ls))
      (dotimes [i (alength ls)] (.writeLong buf (aget ls i))))))

(defn- write-block-entities! [^Buf buf entries]
  (c/write-varint buf (count entries))
  (doseq [[[x y z] {:keys [type nbt]}] entries]
    (.writeByte buf (int (bit-or (bit-shift-left (bit-and (long x) 15) 4) (bit-and (long z) 15))))
    (.writeShort buf (int y))
    (c/write-varint buf (long type))
    (c/write-nbt buf nbt)))

(defn- write-sections! [^Buf buf chunk]
  (let [body (Buf. 4096)
        idx (int-array 4096)
        scratch (int-array scratch-size)]
    (dotimes [wi chunk/section-count]
      (if-let [^Section s (our-section chunk wi)]
        (write-section! body s idx scratch)
        (write-empty-section! body)))
    (c/write-varint buf (.readableBytes body))
    (.writeBytes buf body)))

(defn write-chunk!
  ([buf cx cz chunk] (write-chunk! buf cx cz chunk nil))
  ([^Buf buf cx cz chunk block-entities]
   (.writeInt buf (int (long cx)))
   (.writeInt buf (int (long cz)))
   (write-heightmaps! buf chunk)
   (write-sections! buf chunk)
   (write-block-entities! buf block-entities)
   (let [blk? (fn [li] (some? (our-section chunk (dec (long li)))))
         top (long (reduce (fn [^long acc ^long si]
                             (if (some? (our-section chunk si)) si acc))
                           -1 (range chunk/section-count)))
         sky? (fn [li] (or (blk? li) (and (>= top 0) (= (dec (long li)) (inc top)))))
         sky-mask (light-mask sky?)
         blk-mask (light-mask blk?)]
     (c/write-varint buf 1) (.writeLong buf sky-mask)
     (c/write-varint buf 1) (.writeLong buf blk-mask)
     (c/write-varint buf 0)
     (c/write-varint buf 0)
     (c/write-varint buf (Long/bitCount sky-mask))
     (dotimes [li light-sections]
       (when (sky? li)
         (c/write-varint buf 2048)
         (if-let [^Section s (our-section chunk (dec li))]
           (.writeBytes buf ^bytes (.sky-light s))
           (.writeBytes buf ^bytes full-light))))
     (c/write-varint buf (Long/bitCount blk-mask))
     (dotimes [li light-sections]
       (when (blk? li)
         (c/write-varint buf 2048)
         (.writeBytes buf ^bytes (.block-light ^Section (our-section chunk (dec li)))))))))
