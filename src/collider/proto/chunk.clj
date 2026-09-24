(ns collider.proto.chunk
  "Chunks as the client receives them."
  (:require [collider.data :as data]
            [collider.proto.buf :as buf]
            [collider.proto.codec :as c]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

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

(defn- state-flag? [^long st ^long bit]
  (pos? (bit-and (long (get (data/flags) st 0)) bit)))

(defn- fluid? [^long st]
  (or (block/liquid? st) (block/waterlogged? st)))

(defn- motion-blocking? [^long st]
  (or (state-flag? st 1) (fluid? st)))

(defn- state-table ^booleans [pred]
  (let [a (boolean-array (data/block-state-count))]
    (dotimes [i (data/block-state-count)]
      (aset a i (boolean (pred i))))
    a))

(defn- surface? [^long st] (not= :air (block/type-of st)))

(defn- no-leaves? [^long st]
  (and (motion-blocking? st) (not (block/leaves? st))))

(def ^:private ^:table surface-arr (delay (state-table surface?)))

(def ^:private ^:table motion-arr
  (delay (state-table motion-blocking?)))

(def ^:private ^:table no-leaves-arr
  (delay (state-table no-leaves?)))

(def ^:private ^:table fluid-arr (delay (state-table fluid?)))

(def ^:private ^:table plains
  (delay (data/datapack-id "worldgen/biome" :plains)))

(defn- write-section! [buf s]
  (chunk/write-section! s buf @fluid-arr @plains))

(defn- write-biomes! [buf]
  (buf/write-byte! buf 0)
  (c/write-varint buf (long @plains)))

(defn- write-empty-section! [buf]
  (buf/write-short! buf 0)
  (buf/write-short! buf 0)
  (buf/write-byte! buf 0) (c/write-varint buf block/air)
  (write-biomes! buf))

(defn- window
  "Returns [first section, section count, sky?] of the level lv."
  [lv]
  (let [lo (chunk/level-min-y lv)]
    [(chunk/section-index lo)
     (quot (- (inc (chunk/level-max-y lv)) lo) 16)
     (:sky? lv true)]))

(defn- light-mask ^long [^long n pred]
  (loop [i 0 m 0]
    (if (= i (+ n 2))
      m
      (recur (inc i)
             (if (pred i) (bit-or m (bit-shift-left 1 i)) m)))))

(defn- our-section [chunk ^long si]
  (chunk/chunk-section chunk si))

(defn- heightmap-longs ^longs [chunk [^long lo ^long n] pred]
  (let [out (int-array 256)]
    (doseq [si (range (+ lo n -1) (dec lo) -1)
            :let [s (our-section chunk si)]
            :when s]
      (chunk/heights! s pred out (* 16 (- (long si) lo))))
    (pack-longs (ceillog2 (inc (* 16 n))) out)))

(def ^:private ^:table client-heightmaps
  (delay [[1 @surface-arr] [4 @motion-arr] [5 @no-leaves-arr]]))

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
    (c/write-nbt buf nbt)))

(defn- write-sections! [buf chunk [^long lo ^long n]]
  (let [body (buf/buf 4096)]
    (dotimes [i n]
      (if-let [s (our-section chunk (+ lo i))]
        (write-section! body s)
        (write-empty-section! body)))
    (c/write-varint buf (buf/readable-bytes body))
    (buf/write-bytes! buf body)))

(defn- at-light [chunk ^long lo ^long li]
  (our-section chunk (+ lo li -1)))

(defn- blk-pred [chunk [^long lo]]
  (fn [li] (some? (at-light chunk lo li))))

(defn- top-section ^long [chunk [^long lo ^long n]]
  (long (reduce (fn [^long acc ^long i]
                  (if (some? (our-section chunk (+ lo i))) i acc))
                -1 (range n))))

(defn- sky-pred [chunk [_ _ sky? :as win] blk?]
  (let [top (top-section chunk win)]
    (fn [li]
      (and sky?
           (or (blk? li)
               (and (>= top 0) (= (dec (long li)) (inc top))))))))

(defn- lit? [s channel]
  (if (= channel :sky) (chunk/sky-lit? s) (chunk/block-lit? s)))

(defn- light-of [chunk [lo] pred li channel]
  (when (pred li)
    (if-let [s (at-light chunk lo li)]
      (if (lit? s channel) s :empty)
      :full)))

(defn- light-masks [chunk [_ n :as win] pred channel]
  (let [of #(light-of chunk win pred % channel)]
    [(light-mask n #(not (contains? #{nil :empty} (of %))))
     (light-mask n #(= :empty (of %)))]))

(defn- write-light-masks! [buf [sky sky-empty] [blk blk-empty]]
  (doseq [m [sky blk sky-empty blk-empty]]
    (if (zero? (long m))
      (c/write-varint buf 0)
      (do (c/write-varint buf 1) (buf/write-long! buf (long m))))))

(defn- write-light! [buf chunk [_ n :as win] pred channel mask]
  (c/write-varint buf (Long/bitCount (long mask)))
  (dotimes [li (+ (long n) 2)]
    (when (bit-test (long mask) li)
      (c/write-varint buf 2048)
      (let [l (light-of chunk win pred li channel)]
        (cond
          (= l :full) (chunk/write-full-light! buf)
          (= channel :sky) (chunk/write-sky-light! l buf)
          :else (chunk/write-block-light! l buf))))))

(defn- write-lights! [buf chunk win]
  (let [blk? (blk-pred chunk win)
        sky? (sky-pred chunk win blk?)
        sky (light-masks chunk win sky? :sky)
        blk (light-masks chunk win blk? :block)]
    (write-light-masks! buf sky blk)
    (write-light! buf chunk win sky? :sky (first sky))
    (write-light! buf chunk win blk? :block (first blk))))

(defn write-chunk!
  "Writes chunk at cx cz as the level lv shows it.
  It holds the sections, heightmaps and light inside the height
  of lv."
  ([buf cx cz chunk] (write-chunk! buf cx cz chunk nil nil))
  ([buf cx cz chunk block-entities]
   (write-chunk! buf cx cz chunk block-entities nil))
  ([buf cx cz chunk block-entities lv]
   (let [win (window lv)]
     (buf/write-int! buf (int (long cx)))
     (buf/write-int! buf (int (long cz)))
     (write-heightmaps! buf chunk win)
     (write-sections! buf chunk win)
     (write-block-entities! buf block-entities)
     (write-lights! buf chunk win))))
