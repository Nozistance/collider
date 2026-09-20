(ns collider.world.chunk
  "Chunks: block states and light, and chunk and block ids."
  (:require [collider.vec :as v])
  (:import (collider.java Buf Chunk ChunkIndex Section)
           (java.io DataInput DataOutput)
           (java.util Arrays HashMap)))

(set! *warn-on-reflection* true)

(def ^:const min-y -64)

(def ^:const max-y 319)

(def ^:const section-count 24)

(def ^:const section-offset 4)

(defn in-range? [^long y] (<= min-y y max-y))

(defn section-index ^long [^long y] (+ (bit-shift-right y 4) section-offset))

(def ^Section empty-section Section/EMPTY)

(defn section ^Section [^shorts blocks ^bytes block-light ^bytes sky-light]
  (Section/of blocks block-light sky-light))

(defn nibble-get ^long [^bytes arr ^long idx]
  (let [b (long (aget arr (bit-shift-right idx 1)))]
    (if (zero? (bit-and idx 1))
      (bit-and b 0xF)
      (bit-and (bit-shift-right b 4) 0xF))))

(defn nibble-set! [^bytes arr ^long idx ^long v]
  (let [bi (bit-shift-right idx 1)
        b (long (aget arr bi))]
    (aset arr bi
          (unchecked-byte
            (if (zero? (bit-and idx 1))
              (bit-or (bit-and b 0xF0) v)
              (bit-or (bit-and b 0x0F) (bit-shift-left v 4)))))))

(def ^ChunkIndex no-chunks ChunkIndex/EMPTY)

(def empty-chunk Chunk/EMPTY)

(defn chunk-of ^Chunk [sections]
  (Chunk/of (object-array sections)))

(defn first-above ^Section [^Chunk chunk ^long si]
  (.firstAbove chunk (int si)))

(defn nil-sky
  "Returns the sky light a new section at si inherits at lx lz.
  The light comes from above."
  ^long [chunk ^long si ^long lx ^long lz]
  (if-let [^Section s (first-above chunk si)]
    (.skyLight s (int (+ (* lz 16) lx)))
    15))

(defn new-section
  "Returns an empty section at si with inherited sky light."
  ^Section [^Chunk chunk ^long si]
  (.fresh chunk (int si)))

(defn nil-sky-array
  "Returns a fresh array of the sky light a new section inherits.
  The section is at si and the light comes from above."
  ^bytes [chunk ^long si]
  (.skyLightCopy ^Section (new-section chunk si)))

(defn chunk-at ^Chunk [^ChunkIndex chunks ^long cx ^long cz]
  (Chunk/at chunks (int cx) (int cz)))

(defn section-at
  "Returns the section holding block x y z, nil where none exists."
  ^Section [^ChunkIndex chunks ^long x ^long y ^long z]
  (Chunk/sectionAt chunks (unchecked-int x) (unchecked-int y)
                   (unchecked-int z)))

(defn chunk-section ^Section [^Chunk chunk ^long si]
  (.section chunk (int si)))

(defn with-section ^Chunk [^Chunk chunk ^long si ^Section s]
  (.with chunk (int si) s))

(defn section-block
  "Returns the block state at index idx of s.
  The order is y, then z, then x."
  ^long [^Section s ^long idx]
  (.block s (int idx)))

(defn sky-light ^long [^Section s ^long idx] (.skyLight s (int idx)))

(defn block-light ^long [^Section s ^long idx] (.blockLight s (int idx)))

(defn sky-light-copy ^bytes [^Section s] (.skyLightCopy s))

(defn block-light-copy ^bytes [^Section s] (.blockLightCopy s))

(defn with-sky-light ^Section [^Section s ^bytes a] (.withSkyLight s a))

(defn with-block-light ^Section [^Section s ^bytes a] (.withBlockLight s a))

(defn sky-lit? [^Section s] (.hasSkyLight s))

(defn block-lit? [^Section s] (.hasBlockLight s))

(defn heights!
  "Fills the unset entries of the 256 heightmap columns in out.
  Each entry gets the height above base of the topmost block of
  s that pred marks."
  [^Section s ^booleans pred ^ints out ^long base]
  (.heights s pred out (int base)))

(defn write-section!
  "Writes s to buf in wire form.
  It counts the states fluid marks and gives every block
  the biome."
  [^Section s ^Buf buf ^booleans fluid ^long biome]
  (.write s buf fluid (int biome)))

(defn write-sky-light! [^Section s ^Buf buf] (.writeSkyLight s buf))

(defn write-block-light! [^Section s ^Buf buf] (.writeBlockLight s buf))

(defn write-full-light! [^Buf buf] (Section/writeFullLight buf))

(defn save-chunk! [^Chunk chunk ^DataOutput out] (.save chunk out))

(defn load-chunk ^Chunk [^DataInput in] (Chunk/load in))

(defn set-block ^Chunk [^Chunk chunk lx y lz state]
  (let [y (long y)
        si (int (section-index y))
        idx (+ (* (bit-and y 15) 256) (* (long lz) 16) (long lx))
        s (or (.section chunk si) (.fresh chunk si))]
    (.with chunk si (.with s (int idx) (int state)))))

(defn get-block
  "Returns the block state at local lx y lz.
  Air comes back where no section exists."
  ^long [^Chunk chunk lx y lz]
  (if chunk
    (.block chunk (int lx) (int y) (int lz))
    0))

(defn block-pos->id
  (^long [[x y z]] (block-pos->id x y z))
  (^long [x y z]
   (let [x (long x) y (long y) z (long z)]
     (bit-or (bit-shift-left (bit-and x 0x3FFFFFF) 38)
             (bit-shift-left (bit-and y 0xFFF) 26)
             (bit-and z 0x3FFFFFF)))))

(defn id->block-pos [^long id]
  [(bit-shift-right id 38)
   (bit-shift-right (bit-shift-left id 26) 52)
   (bit-shift-right (bit-shift-left id 38) 38)])

(defn pos->id ^long [cx cz]
  (bit-or (bit-shift-left (bit-and (long cx) 0xFFFFFFFF) 32)
          (bit-and (long cz) 0xFFFFFFFF)))

(defn id->pos [chunk-id]
  [(long (unchecked-int (bit-shift-right (long chunk-id) 32)))
   (long (unchecked-int (bit-and (long chunk-id) 0xFFFFFFFF)))])

(defn around-ids [^long cx ^long cz ^long r]
  (for [dx (range (- r) (inc r))
        dz (range (- r) (inc r))]
    (pos->id (+ cx dx) (+ cz dz))))

(defn tracked?
  "Tells whether a chunk dx dz away is within view distance v.
  The two rings nearest the axes count as distance zero, so the
  set reaches v+1 along them and is cut at the corners."
  [^long v ^long dx ^long dz]
  (let [ax (max 0 (- (Math/abs dx) 2))
        az (max 0 (- (Math/abs dz) 2))]
    (< (+ (* ax ax) (* az az)) (* v v))))

(defn tracked-ids
  "Returns the ids of the chunks a viewer in cx cz with view
  distance v keeps."
  [^long cx ^long cz ^long v]
  (for [dx (range (- -1 v) (+ v 2))
        dz (range (- -1 v) (+ v 2))
        :when (tracked? v dx dz)]
    (pos->id (+ cx dx) (+ cz dz))))

(defn block-id-chunk
  "Returns the id of the chunk that holds packed block id bid."
  ^long [^long bid]
  (pos->id (bit-shift-right bid 42) (bit-shift-right (bit-shift-left bid 38) 42)))

(defn block-chunk
  "Returns the id of the chunk that holds the block at x y z."
  ^long [[x _ z]]
  (pos->id (bit-shift-right (long x) 4) (bit-shift-right (long z) 4)))

(defn pos-chunk
  "Returns the id of the chunk that holds the point pos."
  ^long [pos]
  (pos->id (bit-shift-right (long (Math/floor (v/x pos))) 4)
           (bit-shift-right (long (Math/floor (v/z pos))) 4)))

(defn chunks-get-block
  "Returns the block state at x y z, air where the chunk is absent."
  (^long [chunks [x y z]]
   (Chunk/blockAt chunks (unchecked-int x) (unchecked-int y) (unchecked-int z)))
  ([chunks x y z]
   (Chunk/blockAt chunks (unchecked-int x) (unchecked-int y) (unchecked-int z))))

(definline block-state [chunks x y z]
  `(long (Chunk/blockAt ~chunks (unchecked-int ~x) (unchecked-int ~y) (unchecked-int ~z))))

(defn at
  "Returns the block state at p, air outside the world height."
  ^long [chunks [_ y _ :as p]]
  (if (in-range? y) (chunks-get-block chunks p) 0))

(defn at-void
  "Returns the block state at p, -1 outside the world height."
  ^long [chunks [_ y _ :as p]]
  (if (in-range? y) (chunks-get-block chunks p) -1))

(definterface Edits
  (add [^long i ^long state])
  (applyTo [^collider.java.Section s]))

(deftype ^:private Batch [^:unsynchronized-mutable ^ints idx
                          ^:unsynchronized-mutable ^ints states
                          ^:unsynchronized-mutable ^long n]
  Edits
  (add [_ i state]
    (when (= n (alength idx))
      (set! idx (Arrays/copyOf idx (int (* 2 n))))
      (set! states (Arrays/copyOf states (int (* 2 n)))))
    (aset idx n (int i))
    (aset states n (int state))
    (set! n (inc n)))
  (applyTo [_ s]
    (.apply s idx states (int n))))

(defn- add-edit! [^Edits e ^long i ^long state] (.add e i state))

(defn- edited ^Section [^Edits e ^Section s] (.applyTo e s))

(defn- edits-of ^Edits [^HashMap cache cp si]
  (let [k [cp si]]
    (or (.get cache k)
        (let [e (Batch. (int-array 8) (int-array 8) 0)]
          (.put cache k e)
          e))))

(defn- merge-section ^Chunk [^Chunk c [[_ si] edits]]
  (let [si (int si)
        s (or (.section c si) (.fresh c si))]
    (.with c si (edited edits s))))

(defn- apply-change! [^HashMap cache change]
  (let [[[x y z :as p] state] change
        x (long x) y (long y) z (long z)
        cp (block-chunk p)
        idx (+ (* (bit-and y 15) 256) (* (bit-and z 15) 16)
               (bit-and x 15))]
    (add-edit! (edits-of cache cp (section-index y))
               idx (long state))))

(defn- cache-order [^HashMap cache]
  (sort-by (fn [[[cp si] _]] [(long cp) (- (long si))]) (into {} cache)))

(defn with-chunks
  "Returns chunks with each group of entries reduced by f.
  An entry is [[cp k] x] and its group is reduced into the chunk
  at cp. A group for an absent chunk is dropped."
  ^ChunkIndex [^ChunkIndex chunks f groups]
  (let [gs (filterv (fn [g] (some? (.get chunks (long (ffirst (first g)))))) groups)
        ids (long-array (count gs))
        cs (object-array (count gs))]
    (dotimes [i (count gs)]
      (let [g (gs i)
            cp (long (ffirst (first g)))]
        (aset ids i cp)
        (aset cs i (reduce f (.get chunks cp) g))))
    (.withAll chunks ids cs)))

(defn chunks-set-blocks
  "Returns chunks with the [pos state] changes applied.
  Changes in absent chunks are dropped."
  [chunks changes]
  (if (empty? changes)
    chunks
    (let [cache (HashMap.)]
      (doseq [change changes] (apply-change! cache change))
      (with-chunks chunks merge-section
        (partition-by ffirst (cache-order cache))))))
