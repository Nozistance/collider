(ns collider.world.chunk
  "Chunks: block states and light, and chunk and block ids."
  (:import (collider.java Section)
           (java.util Arrays HashMap)))

(set! *warn-on-reflection* true)

(def ^:const min-y -64)
(def ^:const max-y 319)
(def ^:const section-count 24)
(def ^:const section-offset 4)
(defn in-range?
  "Returns true when y is inside the world height."
  [^long y] (<= min-y y max-y))
(defn section-index
  "Returns the index of the section holding y."
  ^long [^long y] (+ (bit-shift-right y 4) section-offset))

(def ^Section empty-section Section/EMPTY)

(defn section
  "Returns a section of the block states and light arrays given."
  ^Section [^shorts blocks ^bytes block-light ^bytes sky-light]
  (Section/of blocks block-light sky-light))

(defn nibble-get
  "Returns the 4-bit value at idx of a nibble array."
  ^long [^bytes arr ^long idx]
  (let [b (long (aget arr (bit-shift-right idx 1)))]
    (if (zero? (bit-and idx 1))
      (bit-and b 0xF)
      (bit-and (bit-shift-right b 4) 0xF))))

(defn nibble-set!
  "Sets the 4-bit value at idx of a nibble array in place."
  [^bytes arr ^long idx ^long v]
  (let [bi (bit-shift-right idx 1)
        b (long (aget arr bi))]
    (aset arr bi
          (unchecked-byte
            (if (zero? (bit-and idx 1))
              (bit-or (bit-and b 0xF0) v)
              (bit-or (bit-and b 0x0F) (bit-shift-left v 4)))))))

(defn first-above
  "Returns the first present section above si, or nil."
  ^Section [chunk ^long si]
  (loop [i (inc si)]
    (when (< i section-count)
      (if-let [s (get (:sections chunk) i)] s (recur (inc i))))))

(defn nil-sky
  "Returns the sky light a new section at si inherits from above at lx lz."
  ^long [chunk ^long si ^long lx ^long lz]
  (if-let [^Section s (first-above chunk si)]
    (.skyLight s (int (+ (* lz 16) lx)))
    15))

(defn new-section
  "Returns an empty section at si with inherited sky light."
  ^Section [chunk ^long si]
  (if-let [^Section s (first-above chunk si)]
    (.below s)
    Section/EMPTY))

(defn nil-sky-array
  "Returns the sky light a new section at si inherits from above, as a
   fresh array."
  ^bytes [chunk ^long si]
  (.skyLightCopy ^Section (new-section chunk si)))

(defn set-block
  "Returns chunk with the block at local lx y lz set to state."
  [chunk lx y lz state]
  (let [y (long y)
        si (section-index y)
        idx (+ (* (bit-and y 15) 256) (* (long lz) 16) (long lx))
        ^Section s (or (get (:sections chunk) si)
                       (new-section chunk si))]
    (assoc-in chunk [:sections si] (.with s (int idx) (int state)))))

(defn get-block
  "Returns the block state at local lx y lz, air where no section exists."
  ^long [chunk lx y lz]
  (let [y (long y)
        si (section-index y)]
    (if-let [s (get (:sections chunk) si)]
      (.block ^Section s (int (+ (* (bit-and y 15) 256)
                                 (* (long lz) 16) (long lx))))
      0)))

(defn block-pos->id
  "Returns the id of a block position."
  (^long [[x y z]] (block-pos->id x y z))
  (^long [x y z]
   (let [x (long x) y (long y) z (long z)]
     (bit-or (bit-shift-left (bit-and x 0x3FFFFFF) 38)
             (bit-shift-left (bit-and y 0xFFF) 26)
             (bit-and z 0x3FFFFFF)))))

(defn id->block-pos
  "Returns the block position of an id."
  [^long id]
  [(bit-shift-right id 38)
   (bit-shift-right (bit-shift-left id 26) 52)
   (bit-shift-right (bit-shift-left id 38) 38)])

(defn pos->id
  "Returns the id of the chunk at cx cz."
  ^long [cx cz]
  (bit-or (bit-shift-left (bit-and (long cx) 0xFFFFFFFF) 32)
          (bit-and (long cz) 0xFFFFFFFF)))

(defn id->pos
  "Returns [cx cz] of a chunk id."
  [chunk-id]
  [(long (unchecked-int (bit-shift-right (long chunk-id) 32)))
   (long (unchecked-int (bit-and (long chunk-id) 0xFFFFFFFF)))])

(defn around-ids
  "Returns the chunk ids within r chunks of cx cz."
  [^long cx ^long cz ^long r]
  (for [dx (range (- r) (inc r))
        dz (range (- r) (inc r))]
    (pos->id (+ cx dx) (+ cz dz))))

(defn block-chunk
  "Returns the chunk id of a block position."
  ^long [[x _ z]]
  (pos->id (bit-shift-right (long x) 4) (bit-shift-right (long z) 4)))

(defn chunks-get-block
  "Returns the block state at x y z, reading template where the chunk is
   absent."
  (^long [chunks template [x y z]]
   (chunks-get-block chunks template x y z))
  ([chunks template x y z]
   (let [x (long x) y (long y) z (long z)]
     (get-block (get chunks (pos->id (bit-shift-right x 4) (bit-shift-right z 4)) template)
                (bit-and x 15) y (bit-and z 15)))))

(definline block-state [chunks template x y z]
  `(let [x# (long ~x) y# (long ~y) z# (long ~z)]
     (get-block (get ~chunks (pos->id (bit-shift-right x# 4) (bit-shift-right z# 4)) ~template)
                (bit-and x# 15) y# (bit-and z# 15))))

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

(defn- edits-of ^Edits [^HashMap cache cp si]
  (let [k [cp si]]
    (or (.get cache k)
        (let [e (Batch. (int-array 8) (int-array 8) 0)]
          (.put cache k e)
          e))))

(defn- merge-section [template chs [[cp si] edits]]
  (let [c (get chs cp template)
        s (or (get (:sections c) si) (new-section c si))]
    (assoc chs cp (assoc-in c [:sections si]
                            (.applyTo ^Edits edits s)))))

(defn- apply-change! [^HashMap cache change]
  (let [[[x y z] state] change
        x (long x) y (long y) z (long z)
        cp (pos->id (bit-shift-right x 4) (bit-shift-right z 4))
        idx (+ (* (bit-and y 15) 256) (* (bit-and z 15) 16)
               (bit-and x 15))]
    (.add (edits-of cache cp (section-index y)) idx (long state))))

(defn- cache-order [^HashMap cache]
  (sort-by (fn [[[cp si] _]] [(long cp) (- (long si))]) (into {} cache)))

(defn chunks-set-blocks
  "Returns chunks with the [pos state] changes applied; an absent chunk starts
   from template."
  [chunks template changes]
  (if (empty? changes)
    chunks
    (let [cache (HashMap.)]
      (doseq [change changes] (apply-change! cache change))
      (reduce (partial merge-section template) chunks (cache-order cache)))))
