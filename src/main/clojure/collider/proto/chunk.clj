(ns collider.proto.chunk
  "Chunks as the client receives them."
  (:require [collider.data :as data]
            [collider.data.state :as states]
            [collider.proto.buf :as buf]
            [collider.proto.codec :as c]
            [collider.proto.nbt :as nbt]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.biome :as biome])
  (:import (collider.world Section)))

(set! *warn-on-reflection* true)

(defn- pack-longs ^longs [^long bits ^ints values]
  (let [per (quot 64 bits)
        len (alength values)
        n (long (Math/ceil (/ (double len) per)))
        out (long-array n)]
    (dotimes [i len]
      (let [at (quot (long i) per)
            off (* (rem (long i) per) bits)
            v (bit-shift-left (long (aget values i)) off)]
        (aset out at (bit-or (aget out at) v))))
    out))

(defn- ceillog2 ^long [^long n]
  (- 32 (Integer/numberOfLeadingZeros (int (dec (max 1 n))))))

(def ^:private ^:const motion-blocking-flag 1)

(defn- state-flag? [^long st ^long bit]
  (pos? (bit-and (long (get (states/flags) st 0)) bit)))

(defn- fluid? [^long st]
  (or (block/liquid? st) (block/waterlogged? st)))

(defn- motion-blocking? [^long st]
  (or (state-flag? st motion-blocking-flag) (fluid? st)))

(defn- state-table ^booleans [pred]
  (let [a (boolean-array (data/block-state-count))]
    (dotimes [i (data/block-state-count)]
      (aset a i (boolean (pred i))))
    a))

(defn- non-air? [^long st] (not= :air (block/type-of st)))

(defn- no-leaves? [^long st]
  (and (motion-blocking? st) (not (block/leaves? st))))

(def ^:private ^:table non-air-arr (delay (state-table non-air?)))

(def ^:private ^:table motion-arr
  (delay (state-table motion-blocking?)))

(def ^:private ^:table no-leaves-arr
  (delay (state-table no-leaves?)))

(def ^:private ^:table fluid-arr (delay (state-table fluid?)))

(def ^:private dark (byte-array 2048))

(defn- write-palette! [buf ^Section s]
  (let [n (.paletteSize s)]
    (when-not (zero? n)
      (when-not (zero? (.bits s)) (c/write-varint buf n))
      (dotimes [k n] (c/write-varint buf (.paletteId s k))))))

(defn- write-section! [buf ^Section s ^long biome]
  (let [t (.tally s ^booleans @fluid-arr)]
    (buf/write-short! buf (unsigned-bit-shift-right t 16))
    (buf/write-short! buf (bit-and t 0xFFFF))
    (buf/write-byte! buf (.bits s))
    (write-palette! buf s)
    (dotimes [c (.wordCount s)] (buf/write-long! buf (.word s c)))
    (buf/write-byte! buf 0)
    (c/write-varint buf biome)))

(defn- write-biomes! [buf ^long biome]
  (buf/write-byte! buf 0)
  (c/write-varint buf biome))

(defn- write-empty-section! [buf ^long biome]
  (let [block-count 0 fluid-count 0 single-value-bits 0]
    (buf/write-short! buf block-count)
    (buf/write-short! buf fluid-count)
    (buf/write-byte! buf single-value-bits)
    (c/write-varint buf block/air)
    (write-biomes! buf biome)))

(defn- window [lv]
  (let [lo (chunk/level-min-y lv)]
    {:lo (chunk/section-index lo)
     :n (quot (- (inc (chunk/level-max-y lv)) lo) 16)
     :sky? (:sky? lv true)
     :biome (biome/id (:dim lv))}))

(defn- heightmap-longs ^longs [chunk {:keys [^long lo ^long n]} pred]
  (let [out (int-array 256)]
    (doseq [si (range (+ lo n -1) (dec lo) -1)
            :let [s (chunk/chunk-section chunk si)]
            :when s]
      (chunk/heights! s pred out (* 16 (- (long si) lo))))
    (pack-longs (ceillog2 (inc (* 16 n))) out)))

(def ^:private ^:const world-surface-id 1)

(def ^:private ^:const motion-blocking-id 4)

(def ^:private ^:const no-leaves-id 5)

(def ^:private ^:table client-heightmaps
  (delay [[world-surface-id @non-air-arr]
          [motion-blocking-id @motion-arr]
          [no-leaves-id @no-leaves-arr]]))

(defn- write-heightmaps! [buf chunk win]
  (c/write-varint buf (count @client-heightmaps))
  (doseq [[id pred] @client-heightmaps]
    (c/write-varint buf (long id))
    (let [^longs ls (heightmap-longs chunk win pred)]
      (c/write-varint buf (alength ls))
      (dotimes [i (alength ls)] (buf/write-long! buf (aget ls i))))))

(defn- packed-xz ^long [x z]
  (bit-or (bit-shift-left (bit-and (long x) 15) 4)
          (bit-and (long z) 15)))

(defn- write-block-entities! [buf entries]
  (c/write-varint buf (count entries))
  (doseq [[[x y z] {:keys [type nbt]}] entries]
    (buf/write-byte! buf (int (packed-xz x z)))
    (buf/write-short! buf (int y))
    (c/write-varint buf (long type))
    (nbt/write-nbt buf nbt)))

(defn- write-sections! [buf chunk {:keys [^long lo ^long n biome]}]
  (let [body (buf/buf 4096)
        biome (long biome)]
    (dotimes [i n]
      (if-let [s (chunk/chunk-section chunk (+ lo i))]
        (write-section! body s biome)
        (write-empty-section! body biome)))
    (c/write-varint buf (buf/readable-bytes body))
    (buf/write-bytes! buf body)))

(defn- filled-bits ^long [chunk ^long lo ^long n ^booleans solid]
  (loop [i 0 m 0]
    (if (= i n)
      m
      (let [s (chunk/chunk-section chunk (+ lo i))
            hit? (and (some? s) (chunk/holds? s solid))]
        (recur (inc i)
               (if hit? (bit-or m (bit-shift-left 1 (inc i))) m))))))

(defn- around-bits
  ^long [cs ^long cx ^long cz {:keys [lo n]}]
  (let [solid @non-air-arr]
    (loop [k 0 m 0]
      (if (= k 9)
        m
        (let [x (+ cx (dec (quot k 3)))
              z (+ cz (dec (rem k 3)))
              c (when (and cs (not= k 4)) (chunk/chunk-at cs x z))]
          (recur (inc k)
                 (if c (bit-or m (filled-bits c lo n solid)) m)))))))

(defn- spread ^long [^long m]
  (bit-or m (bit-shift-left m 1) (unsigned-bit-shift-right m 1)))

(defn- dark-bottom? [s]
  (loop [idx 0]
    (cond
      (= idx 256) true
      (pos? (chunk/sky-light s idx)) false
      :else (recur (inc idx)))))

(defn- dark-inherited? [chunk ^long si]
  (if-let [s (chunk/first-above chunk si)]
    (or (not (chunk/sky-lit? s)) (dark-bottom? s))
    false))

(defn- sky-empty? [chunk ^long si]
  (if-let [s (chunk/chunk-section chunk si)]
    (not (chunk/sky-lit? s))
    (dark-inherited? chunk si)))

(defn- block-empty? [chunk ^long si]
  (if-let [s (chunk/chunk-section chunk si)]
    (not (chunk/block-lit? s))
    true))

(defn- empty-mask ^long [chunk ^long lo ^long layers dark?]
  (loop [li 0 m 0]
    (if (= li 64)
      m
      (recur (inc li)
             (if (and (bit-test layers li)
                      (dark? chunk (+ lo li -1)))
               (bit-or m (bit-shift-left 1 li))
               m)))))

(defn- write-mask! [buf ^long m]
  (if (zero? m)
    (c/write-varint buf 0)
    (do (c/write-varint buf 1) (buf/write-long! buf m))))

(defn- write-masks! [buf ^long sky ^long blk]
  (write-mask! buf sky)
  (write-mask! buf blk))

(defn- write-sky-layer! [buf chunk ^long si]
  (let [s (or (chunk/chunk-section chunk si)
                (chunk/new-section chunk si))]
    (buf/write-bytes! buf (or (.skyLightBytes ^Section s) dark))))

(defn- write-block-layer! [buf chunk ^long si]
  (let [^Section s (chunk/chunk-section chunk si)]
    (buf/write-bytes! buf (or (.blockLightBytes s) dark))))

(defn- write-light-layers! [buf chunk lo m write-layer!]
  (let [lo (long lo) m (long m)]
    (c/write-varint buf (Long/bitCount m))
    (dotimes [li 64]
      (when (bit-test m li)
        (c/write-varint buf 2048)
        (write-layer! buf chunk (+ lo li -1))))))

(defn- write-lights!
  [buf lv chunk cx cz {:keys [^long lo ^long n sky?] :as win}]
  (let [own (filled-bits chunk lo n @non-air-arr)
        near (around-bits (:chunks lv) cx cz win)
        layers (spread (bit-or own near))
        sky-empty (empty-mask chunk lo (if sky? layers 0) sky-empty?)
        blk-empty (empty-mask chunk lo layers block-empty?)
        sky-lit (bit-and-not (if sky? layers 0) sky-empty)
        blk-lit (bit-and-not layers blk-empty)]
    (write-masks! buf sky-lit blk-lit)
    (write-masks! buf sky-empty blk-empty)
    (write-light-layers! buf chunk lo sky-lit write-sky-layer!)
    (write-light-layers! buf chunk lo blk-lit write-block-layer!)))

(defn write-chunk!
  "Writes chunk at cx cz as level lv shows it to a client."
  ([buf cx cz chunk] (write-chunk! buf cx cz chunk nil nil))
  ([buf cx cz chunk block-entities]
   (write-chunk! buf cx cz chunk block-entities nil))
  ([buf cx cz chunk block-entities lv]
   (let [win (window lv)]
     (buf/write-int! buf cx)
     (buf/write-int! buf cz)
     (write-heightmaps! buf chunk win)
     (write-sections! buf chunk win)
     (write-block-entities! buf block-entities)
     (write-lights! buf lv chunk cx cz win))))
