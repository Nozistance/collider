(ns collider.proto.chunk
  (:require [collider.data :as data]
            [collider.proto.codec :as c]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.world.chunk Section)
           (collider.java Buf)))

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

(defn- write-container! [^Buf buf ^ints ids ^long linear-bits]
  (let [distinct (vec (distinct (seq ids)))]
    (if (= 1 (count distinct))
      (do (.writeByte buf 0)
          (c/write-varint buf (long (first distinct))))
      (let [bits (max linear-bits
                      (long (Math/ceil (/ (Math/log (count distinct)) (Math/log 2)))))
            index (into {} (map-indexed (fn [i v] [v i])) distinct)
            packed (int-array (map #(index %) (seq ids)))]
        (.writeByte buf (int bits))
        (c/write-varint buf (count distinct))
        (doseq [v distinct] (c/write-varint buf (long v)))
        (doseq [^long l (pack-longs bits packed)] (.writeLong buf l))))))

(def ^:private plains (delay (data/datapack-id "worldgen/biome" :plains)))
(defn- section-ids ^ints [^Section s]
  (let [^shorts bs (.blocks s)
        out (int-array 4096)]
    (dotimes [i 4096]
      (aset out i (int (bit-and (long (aget bs i)) 0xFFFF))))
    out))

(defn- write-section! [^Buf buf ^Section s]
  (let [ids (section-ids s)
        [n fluids] (loop [i 0 n 0 f 0]
                     (if (= i 4096)
                       [n f]
                       (let [id (aget ids i)]
                         (recur (inc i)
                                (if (zero? id) n (inc n))
                                (if (or (block/liquid? id) (block/waterlogged? id)) (inc f) f)))))]
    (.writeShort buf (int n))
    (.writeShort buf (int fluids))
    (write-container! buf ids 4)
    (write-container! buf (int-array 64 (int @plains)) 1)))

(defn- write-empty-section! [^Buf buf]
  (.writeShort buf 0)
  (.writeShort buf 0)
  (.writeByte buf 0) (c/write-varint buf block/air)
  (.writeByte buf 0) (c/write-varint buf (long @plains)))

(def ^:private full-light (byte-array 2048 (unchecked-byte 0xFF)))
(defn- light-mask ^long [pred]
  (loop [i 0 m 0]
    (if (= i light-sections) m (recur (inc i) (if (pred i) (bit-or m (bit-shift-left 1 i)) m)))))

(defn- our-section [chunk ^long si]
  (when (< -1 si chunk/section-count) (get (:sections chunk) si)))

(defn- write-block-entities! [^Buf buf entries]
  (c/write-varint buf (count entries))
  (doseq [[[x y z] {:keys [type nbt]}] entries]
    (.writeByte buf (int (bit-or (bit-shift-left (bit-and (long x) 15) 4) (bit-and (long z) 15))))
    (.writeShort buf (int y))
    (c/write-varint buf (long type))
    (c/write-nbt buf nbt)))

(defn write-chunk!
  ([buf cx cz chunk] (write-chunk! buf cx cz chunk nil))
  ([^Buf buf cx cz chunk block-entities]
   (.writeInt buf (int (long cx)))
   (.writeInt buf (int (long cz)))
   (c/write-varint buf 0)
   (let [body (Buf. 4096)]
     (dotimes [wi chunk/section-count]
       (if-let [^Section s (our-section chunk wi)]
         (write-section! body s)
         (write-empty-section! body)))
     (c/write-varint buf (.readableBytes body))
     (.writeBytes buf body))
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
