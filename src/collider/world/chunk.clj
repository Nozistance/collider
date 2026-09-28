(ns collider.world.chunk
  "Chunks of block states and light, with chunk and block ids."
  (:require [collider.vec :as v])
  (:import (collider.proto Buf)
           (collider.world Batch Chunk ChunkIndex Section)
           (java.io DataInput DataOutput)
           (java.util HashMap)))

(set! *warn-on-reflection* true)

(def ^:const min-y -64)

(def ^:const max-y 319)

(def ^:const section-count 24)

(def ^:const section-offset 4)

(defn in-range?
  "Returns true when block y is inside the world height."
  [^long y] (and (<= min-y y) (<= y max-y)))

(defn level-min-y
  "Returns the lowest block y of level lv."
  ^long [lv] (long (:min-y lv min-y)))

(defn level-max-y
  "Returns the highest block y of level lv."
  ^long [lv] (long (:max-y lv max-y)))

(defn void-y
  "Returns the y below which level lv takes what falls out."
  ^double [lv] (- (double (level-min-y lv)) 64.0))

(defn in-level?
  "Returns true when block y is inside the height of level lv."
  [lv ^long y]
  (and (<= (level-min-y lv) y) (<= y (level-max-y lv))))

(defn section-index
  "Returns the index of the section holding block y."
  ^long [^long y]
  (+ (bit-shift-right y 4) section-offset))

(def ^Section empty-section Section/EMPTY)

(defn section
  "Returns a section of block states and block and sky light."
  ^Section [^shorts blocks ^bytes bl ^bytes sl]
  (Section/of blocks bl sl))

(defn nibble-set!
  "Sets the light level at idx of the light levels arr to v."
  [^bytes arr ^long idx ^long v]
  (Section/setNibble arr (int idx) (int v)))

(def ^ChunkIndex no-chunks ChunkIndex/EMPTY)

(defn editable
  "Returns chunks opened for a window of edits.
  Edits of the result copy each index node once and then write it in
  place, until frozen. Only the index returned last is valid to read."
  ^ChunkIndex [^ChunkIndex chunks] (.editable chunks))

(defn editing?
  "Returns true when chunks are open for a window of edits."
  [^ChunkIndex chunks] (.editing chunks))

(defn frozen
  "Returns chunks closed as an immutable value again."
  ^ChunkIndex [^ChunkIndex chunks] (.frozen chunks))

(def empty-chunk Chunk/EMPTY)

(defn chunk-of
  "Returns a chunk of sections, the lowest first."
  ^Chunk [sections]
  (Chunk/of (object-array sections)))

(defn first-above
  "Returns the nearest section above index si of chunk, or nil."
  ^Section [^Chunk chunk ^long si]
  (.firstAbove chunk (int si)))

(defn new-section
  "Returns an empty section at si with inherited sky light."
  ^Section [^Chunk chunk ^long si]
  (.fresh chunk (int si)))

(defn chunk-at
  "Returns the chunk at cx cz, or an empty chunk when it is absent."
  ^Chunk [^ChunkIndex chunks ^long cx ^long cz]
  (Chunk/at chunks (int cx) (int cz)))

(defn chunk-section
  "Returns the section at index si of chunk, nil where none exists."
  ^Section [^Chunk chunk ^long si]
  (.section chunk (int si)))

(defn with-section
  "Returns chunk with section s at index si."
  ^Chunk [^Chunk chunk ^long si ^Section s]
  (.with chunk (int si) s))

(defn section-block
  "Returns the block state at index idx of s. The index runs over
  x fastest and over y slowest."
  ^long [^Section s ^long idx]
  (.block s (int idx)))

(defn sky-light
  "Returns the sky light level at index idx of s."
  ^long [^Section s ^long idx] (.skyLight s (int idx)))

(defn with-sky-light
  "Returns s with the sky light levels a."
  ^Section [^Section s ^bytes a] (.withSkyLight s a))

(defn with-block-light
  "Returns s with the block light levels a."
  ^Section [^Section s ^bytes a] (.withBlockLight s a))

(defn sky-lit?
  "Returns true when s holds its own sky light."
  [^Section s] (.hasSkyLight s))

(defn block-lit?
  "Returns true when s holds its own block light."
  [^Section s] (.hasBlockLight s))

(defn holds?
  "Returns true when s holds a state that the table pred marks."
  [^Section s ^booleans pred] (.holds s pred))

(defn heights!
  "Fills the unset heightmap columns in out.
  Each gets the height above base of the top block of s that
  pred marks."
  [^Section s ^booleans pred ^ints out ^long base]
  (.heights s pred out (int base)))

(defn write-section!
  "Writes s to buf in wire form. It counts the states that fluid
  marks and gives every block the biome."
  [^Section s ^Buf buf ^booleans fluid ^long biome]
  (.write s buf fluid (int biome)))

(defn write-sky-light!
  "Writes the sky light of s to buf."
  [^Section s ^Buf buf] (.writeSkyLight s buf))

(defn write-block-light!
  "Writes the block light of s to buf."
  [^Section s ^Buf buf] (.writeBlockLight s buf))

(defn write-full-light!
  "Writes a section of full light to buf."
  [^Buf buf] (Section/writeFullLight buf))

(defn save-chunk!
  "Writes chunk to out."
  [^Chunk chunk ^DataOutput out] (.save chunk out))

(defn load-chunk
  "Returns the chunk read from in."
  ^Chunk [^DataInput in] (Chunk/load in))

(defn set-block
  "Returns chunk with the block at local lx y lz set to state."
  ^Chunk [^Chunk chunk lx y lz state]
  (let [y (long y)
        si (int (section-index y))
        idx (+ (* (bit-and y 15) 256) (* (long lz) 16) (long lx))
        s (or (.section chunk si) (.fresh chunk si))]
    (.with chunk si (.with s (int idx) (int state)))))

(defn get-block
  "Returns the block state at local lx y lz. Air comes back where
  no section exists, outside the world height and for no chunk."
  ^long [^Chunk chunk lx y lz]
  (if chunk
    (.block chunk (int lx) (int y) (int lz))
    0))

(defn block-pos->id
  "Returns the block position as one long id."
  (^long [[x y z]] (block-pos->id x y z))
  (^long [x y z]
   (let [x (long x) y (long y) z (long z)]
     (bit-or (bit-shift-left (bit-and x 0x3FFFFFF) 38)
             (bit-shift-left (bit-and y 0xFFF) 26)
             (bit-and z 0x3FFFFFF)))))

(defn id->block-pos
  "Returns the block position that id holds."
  [^long id]
  [(bit-shift-right id 38)
   (bit-shift-right (bit-shift-left id 26) 52)
   (bit-shift-right (bit-shift-left id 38) 38)])

(defn pos->id
  "Returns the chunk coordinates cx cz as one long id."
  ^long [cx cz]
  (bit-or (bit-shift-left (bit-and (long cx) 0xFFFFFFFF) 32)
          (bit-and (long cz) 0xFFFFFFFF)))

(defn id->pos
  "Returns the chunk coordinates that chunk-id holds."
  [chunk-id]
  [(long (unchecked-int (bit-shift-right (long chunk-id) 32)))
   (long (unchecked-int (bit-and (long chunk-id) 0xFFFFFFFF)))])

(defn shape
  "Returns a token shared by every version of chunks with the same
  set of chunk ids, whatever their blocks."
  [^ChunkIndex chunks]
  (.shape chunks))

(defn around-ids
  "Returns the ids of the chunks in the square of radius r around
  chunk cx cz."
  [^long cx ^long cz ^long r]
  (for [dx (range (- r) (inc r))
        dz (range (- r) (inc r))]
    (pos->id (+ cx dx) (+ cz dz))))

(defn tracked?
  "Returns true when a chunk dx dz away is within view distance v.
  The two rings nearest the axes count as distance zero. So the view
  reaches v+1 along the axes and is cut at the corners."
  [^long v ^long dx ^long dz]
  (let [ax (max 0 (- (Math/abs dx) 2))
        az (max 0 (- (Math/abs dz) 2))]
    (< (+ (* ax ax) (* az az)) (* v v))))

(defn tracked-ids
  "Returns the ids of the chunks that a viewer in cx cz keeps at view
  distance v."
  [^long cx ^long cz ^long v]
  (for [dx (range (- -1 v) (+ v 2))
        dz (range (- -1 v) (+ v 2))
        :when (tracked? v dx dz)]
    (pos->id (+ cx dx) (+ cz dz))))

(defn block-id-chunk
  "Returns the id of the chunk that holds the block with id bid."
  ^long [^long bid]
  (pos->id (bit-shift-right bid 42)
           (bit-shift-right (bit-shift-left bid 38) 42)))

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
   (Chunk/blockAt chunks (unchecked-int x) (unchecked-int y)
                  (unchecked-int z)))
  ([chunks x y z]
   (Chunk/blockAt chunks (unchecked-int x) (unchecked-int y)
                  (unchecked-int z))))

(definline block-state
  "Returns the block state at x y z, air where the chunk is absent."
  [chunks x y z]
  `(long (Chunk/blockAt
           ~chunks (unchecked-int ~x) (unchecked-int ~y)
           (unchecked-int ~z))))

(defn at
  "Returns the block state at p, air outside the world height."
  ^long [chunks [_ y _ :as p]]
  (if (in-range? y) (chunks-get-block chunks p) 0))

(defn at-void
  "Returns the block state at p, -1 outside the world height."
  ^long [chunks [_ y _ :as p]]
  (if (in-range? y) (chunks-get-block chunks p) -1))

(defn- add-edit! [^Batch e ^long i ^long state] (.add e i state))

(defn- edits-of ^Batch [^HashMap cache cp si]
  (let [k [cp si]]
    (or (.get cache k)
        (let [e (Batch.)]
          (.put cache k e)
          e))))

(defn- apply-change! [^HashMap cache change]
  (let [[[x y z :as p] state] change
        x (long x) y (long y) z (long z)
        cp (block-chunk p)
        idx (+ (* (bit-and y 15) 256) (* (bit-and z 15) 16)
               (bit-and x 15))]
    (add-edit! (edits-of cache cp (section-index y))
               idx (long state))))

(defn- cache-order [^HashMap cache]
  (let [order (fn [[[cp si] _]] [(long cp) (- (long si))])]
    (sort-by order (into {} cache))))

(defn chunks-set-block
  "Returns chunks with the block at p set to state.
  A change in an absent chunk is dropped. Equals chunks-set-blocks
  of the one change without its batch machinery. In a window of edits
  the chunk and its section are written in place once owned."
  ^ChunkIndex [^ChunkIndex chunks [x y z] state]
  (.withBlock chunks (unchecked-int x) (unchecked-int y)
              (unchecked-int z) (unchecked-int state)))

(defn changed
  "Returns the changes [pos st] that alter a block of chunks inside
  the height of level lv, as [pos old st] in order. A change sees the
  ones before it."
  [chunks lv changes]
  (Batch/changed chunks changes (level-min-y lv) (level-max-y lv)))

(defn by-chunk
  "Returns the changes [pos old st] as a map from chunk id to the
  [pos st] of that chunk, in order."
  [changes]
  (Batch/byChunk changes))

(defn chunks-set-blocks
  "Returns chunks with the [pos state] changes applied, or the
  [pos old state] changes when at is 2. Changes in absent chunks are
  dropped."
  ([chunks changes] (chunks-set-blocks chunks changes 1))
  ([chunks changes at]
   (if (empty? changes)
     chunks
     (Batch/setBlocks chunks changes (int at)))))
