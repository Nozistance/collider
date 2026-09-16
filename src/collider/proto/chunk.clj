(ns collider.proto.chunk
  "Chunks as the client receives them."
  (:require [collider.data :as data]
            [collider.proto.codec :as c]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.java Buf Chunk Section)))

(set! *warn-on-reflection* true)

(def ^:const light-sections 26)
(defn- pack-longs
  "Returns the values packed one after another, bits wide each."
  ^longs [^long bits ^ints values]
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

(defn- ceillog2
  "Returns how many bits it takes to number n things."
  ^long [^long n]
  (- 32 (Integer/numberOfLeadingZeros (int (dec (max 1 n))))))

(defn- state-flag?
  "Returns true if a block state carries the given flag."
  [^long st ^long bit]
  (pos? (bit-and (long (get (data/flags) st 0)) bit)))

(defn- fluid?
  "Returns true if a block state holds fluid."
  [^long st]
  (or (block/liquid? st) (block/waterlogged? st)))

(defn- motion-blocking?
  "Returns true if a block state stops something falling onto it."
  [^long st]
  (or (state-flag? st 1) (fluid? st)))

(defn- state-table
  "Returns pred as a table over every block state."
  ^booleans [pred]
  (let [a (boolean-array (data/block-state-count))]
    (dotimes [i (data/block-state-count)]
      (aset a i (boolean (pred i))))
    a))

(def ^:private ^:table surface-arr (delay (state-table (fn [^long st] (not= :air (block/type-of st))))))
(def ^:private ^:table motion-arr (delay (state-table motion-blocking?)))
(def ^:private ^:table no-leaves-arr
  (delay (state-table (fn [^long st] (and (motion-blocking? st) (not (block/leaves? st)))))))
(def ^:private ^:table fluid-arr (delay (state-table fluid?)))

(def ^:private ^:table plains (delay (data/datapack-id "worldgen/biome" :plains)))

(defn- write-section! [^Buf buf ^Section s]
  (.write s buf ^booleans @fluid-arr (int @plains)))

(defn- write-biomes!
  "Writes the biomes of a section."
  [^Buf buf]
  (.writeByte buf 0)
  (c/write-varint buf (long @plains)))

(defn- write-empty-section!
  "Writes a section of nothing but air."
  [^Buf buf]
  (.writeShort buf 0)
  (.writeShort buf 0)
  (.writeByte buf 0) (c/write-varint buf block/air)
  (write-biomes! buf))

(defn- light-mask
  "Returns the mask of the light sections pred accepts."
  ^long [pred]
  (loop [i 0 m 0]
    (if (= i light-sections) m (recur (inc i) (if (pred i) (bit-or m (bit-shift-left 1 i)) m)))))

(defn- our-section
  "Returns the section of chunk at si, or nil when nothing is there."
  [^Chunk chunk ^long si]
  (.section chunk (int si)))

(def ^:private height-bits (ceillog2 (+ 2 (- chunk/max-y chunk/min-y))))

(defn- heightmap-longs
  "Returns one heightmap of a chunk, by pred, ready to write."
  ^longs [chunk ^booleans pred]
  (let [out (int-array 256)]
    (doseq [si (range (dec (long chunk/section-count)) -1 -1)
            :let [s (our-section chunk si)]
            :when s]
      (.heights ^Section s pred out (int (* 16 (long si)))))
    (pack-longs height-bits out)))

(def ^:private ^:table client-heightmaps
  (delay [[1 @surface-arr] [4 @motion-arr] [5 @no-leaves-arr]]))

(defn- write-heightmaps!
  "Writes the heightmaps of a chunk."
  [^Buf buf chunk]
  (c/write-varint buf (count @client-heightmaps))
  (doseq [[id pred] @client-heightmaps]
    (c/write-varint buf (long id))
    (let [^longs ls (heightmap-longs chunk pred)]
      (c/write-varint buf (alength ls))
      (dotimes [i (alength ls)] (.writeLong buf (aget ls i))))))

(defn- write-block-entities!
  "Writes the block entities of a chunk."
  [^Buf buf entries]
  (c/write-varint buf (count entries))
  (doseq [[[x y z] {:keys [type nbt]}] entries]
    (.writeByte buf (int (bit-or (bit-shift-left (bit-and (long x) 15) 4) (bit-and (long z) 15))))
    (.writeShort buf (int y))
    (c/write-varint buf (long type))
    (c/write-nbt buf nbt)))

(defn- write-sections!
  "Writes every section of a chunk."
  [^Buf buf chunk]
  (let [body (Buf. 4096)]
    (dotimes [wi chunk/section-count]
      (if-let [^Section s (our-section chunk wi)]
        (write-section! body s)
        (write-empty-section! body)))
    (c/write-varint buf (.readableBytes body))
    (.writeBytes buf body)))

(defn- blk-pred
  "Returns a test for which light sections carry block light."
  [chunk] (fn [li] (some? (our-section chunk (dec (long li))))))

(defn- top-section
  "Returns the highest section of a chunk that holds anything, or -1."
  ^long [chunk]
  (long (reduce (fn [^long acc ^long si] (if (some? (our-section chunk si)) si acc))
                -1 (range chunk/section-count))))

(defn- sky-pred
  "Returns a test for which light sections carry sky light."
  [chunk blk?]
  (let [top (top-section chunk)]
    (fn [li] (or (blk? li) (and (>= top 0) (= (dec (long li)) (inc top)))))))

(defn- lit? [^Section s channel]
  (if (= channel :sky) (.hasSkyLight s) (.hasBlockLight s)))

(defn- light-of [chunk pred ^long li channel]
  (when (pred li)
    (if-let [s (our-section chunk (dec li))]
      (if (lit? s channel) s :empty)
      :full)))

(defn- light-masks [chunk pred channel]
  (let [of #(light-of chunk pred % channel)]
    [(light-mask #(not (contains? #{nil :empty} (of %))))
     (light-mask #(= :empty (of %)))]))

(defn- write-light-masks! [^Buf buf [sky sky-empty] [blk blk-empty]]
  (doseq [m [sky blk sky-empty blk-empty]]
    (if (zero? (long m))
      (c/write-varint buf 0)
      (do (c/write-varint buf 1) (.writeLong buf (long m))))))

(defn- write-light! [^Buf buf chunk pred channel mask]
  (c/write-varint buf (Long/bitCount (long mask)))
  (dotimes [li light-sections]
    (when (bit-test (long mask) li)
      (c/write-varint buf 2048)
      (let [l (light-of chunk pred li channel)]
        (cond
          (= l :full) (Section/writeFullLight buf)
          (= channel :sky) (.writeSkyLight ^Section l buf)
          :else (.writeBlockLight ^Section l buf))))))

(defn write-chunk!
  "Writes the chunk at cx cz with its block entities: sections, heightmaps
   and light."
  ([buf cx cz chunk] (write-chunk! buf cx cz chunk nil))
  ([^Buf buf cx cz chunk block-entities]
   (.writeInt buf (int (long cx)))
   (.writeInt buf (int (long cz)))
   (write-heightmaps! buf chunk)
   (write-sections! buf chunk)
   (write-block-entities! buf block-entities)
   (let [blk? (blk-pred chunk)
         sky? (sky-pred chunk blk?)
         sky (light-masks chunk sky? :sky)
         blk (light-masks chunk blk? :block)]
     (write-light-masks! buf sky blk)
     (write-light! buf chunk sky? :sky (first sky))
     (write-light! buf chunk blk? :block (first blk)))))
