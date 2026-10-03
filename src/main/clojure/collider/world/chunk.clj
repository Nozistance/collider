(ns collider.world.chunk
  "Chunks of block states and light, with chunk and block ids."
  (:require [collider.cell :as cell]
            [collider.vec :as v])
  (:import (collider.world Batch Chunk ChunkIndex Section)
           (java.io DataInput DataOutput)))

(set! *warn-on-reflection* true)

(def ^:const min-y -64)

(def ^:const max-y 319)

(def ^:const world-border
  "How far from the centre the world border stands, in blocks."
  29999984)

(def ^:const section-count 24)

(def ^:const section-offset 4)

(defn in-range?
  "Returns true when block y is inside the world height."
  [^long y] (and (<= min-y y) (<= y max-y)))

(defn level-min-y
  ^long [lv] (long (:min-y lv min-y)))

(defn level-max-y
  ^long [lv] (long (:max-y lv max-y)))

(defn void-y
  "Returns the y below which level lv takes what falls out."
  ^double [lv] (- (double (level-min-y lv)) 64.0))

(defn in-level?
  [lv ^long y]
  (and (<= (level-min-y lv) y) (<= y (level-max-y lv))))

(defn section-index
  ^long [^long y]
  (+ (bit-shift-right y 4) section-offset))

(def ^Section empty-section Section/EMPTY)

(defn section
  ^Section [^shorts blocks ^bytes bl ^bytes sl]
  (Section/of blocks bl sl))

(defn nibble-set!
  "Sets the light level at idx of arr to v, from 0 to 15."
  [^bytes arr ^long idx ^long v]
  (Section/setNibble arr (int idx) (int v)))

(def ^ChunkIndex no-chunks ChunkIndex/EMPTY)

(defn editable
  "Returns chunks opened for a window of edits. Only the index
  returned last is valid to read."
  ^ChunkIndex [^ChunkIndex chunks] (.editable chunks))

(defn editing?
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
  ^Section [^Chunk chunk ^long si]
  (.section chunk (int si)))

(defn holds?
  "Returns true when section s may hold a state true in pred."
  [^Section s ^booleans pred]
  (.holds s pred))

(defn with-section
  ^Chunk [^Chunk chunk ^long si ^Section s]
  (.with chunk (int si) s))

(defn section-block
  "Returns the block state at index idx of s. The index runs over
  x fastest and over y slowest."
  ^long [^Section s ^long idx]
  (.block s (int idx)))

(defn sky-light
  ^long [^Section s ^long idx] (.skyLight s (int idx)))

(defn with-sky-light
  ^Section [^Section s ^bytes a] (.withSkyLight s a))

(defn with-block-light
  ^Section [^Section s ^bytes a] (.withBlockLight s a))

(defn sky-lit?
  "Returns true when s holds its own sky light."
  [^Section s] (.hasSkyLight s))

(defn block-lit?
  "Returns true when s holds its own block light."
  [^Section s] (.hasBlockLight s))

(defn heights!
  "Fills the unset heightmap columns in out with the height above
  base of the top block of s that pred marks."
  [^Section s ^booleans pred ^ints out ^long base]
  (.heights s pred out (int base)))

(defn save-chunk!
  [^Chunk chunk ^DataOutput out] (.save chunk out))

(defn load-chunk
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

(defn pos->id
  "Returns the chunk coordinates cx cz as one long id."
  ^long [^long cx ^long cz]
  (bit-or (bit-shift-left (bit-and cx 0xFFFFFFFF) 32)
          (bit-and cz 0xFFFFFFFF)))

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

(defn block->chunk
  ^long [^long x]
  (bit-shift-right x 4))

(defn block-id-chunk
  ^long [^long bid]
  (pos->id (bit-shift-right (cell/x bid) 4)
           (bit-shift-right (cell/z bid) 4)))

(defn block-chunk
  ^long [[x _ z]]
  (pos->id (bit-shift-right (long x) 4) (bit-shift-right (long z) 4)))

(defn pos-chunk
  "Returns the id of the chunk that holds the point pos."
  ^long [pos]
  (pos->id (bit-shift-right (long (Math/floor (v/x pos))) 4)
           (bit-shift-right (long (Math/floor (v/z pos))) 4)))

(definline block-state
  "Returns the block state at x y z, air where the chunk is absent."
  [chunks x y z]
  `(long (Chunk/blockAt
           ~chunks (unchecked-int ~x) (unchecked-int ~y)
           (unchecked-int ~z))))

(defn at
  "Returns the block state at p, air outside the world height and
  where the chunk is absent."
  ^long [chunks [x y z]]
  (block-state chunks x y z))

(defn at-void
  "Returns the block state at p, -1 outside the world height."
  ^long [chunks [x y z]]
  (if (in-range? y) (block-state chunks x y z) -1))

(defn chunks-set-block
  "Returns chunks with the block at p set to state. A change in an
  absent chunk is dropped."
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

(defn- set-all [chunks changes ^long state-at]
  (if (empty? changes)
    chunks
    (Batch/setBlocks chunks changes (int state-at))))

(defn chunks-set-blocks
  "Returns chunks with the [pos state] changes applied. Changes in
  absent chunks are dropped."
  [chunks changes]
  (set-all chunks changes 1))

(defn chunks-set-writes
  "Returns chunks with the [pos old state] writes applied. Writes in
  absent chunks are dropped."
  [chunks writes]
  (set-all chunks writes 2))
