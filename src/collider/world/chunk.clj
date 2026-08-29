(ns collider.world.chunk
  (:import (java.util Arrays HashMap)))

(set! *warn-on-reflection* true)

(deftype Section [^shorts blocks ^bytes block-light ^bytes sky-light])
(defn full-light ^bytes []
  (doto (byte-array 2048) (Arrays/fill (unchecked-byte 0xFF))))

(def ^Section empty-section
  (Section. (short-array 4096) (byte-array 2048) (full-light)))

(defn nibble-get
  ^long [^bytes arr ^long idx]
  (let [b (long (aget arr (bit-shift-right idx 1)))]
    (if (zero? (bit-and idx 1))
      (bit-and b 0xF)
      (bit-and (bit-shift-right b 4) 0xF))))

(defn nibble-set! [^bytes arr ^long idx ^long v]
  (let [bi (bit-shift-right idx 1)
        b  (long (aget arr bi))]
    (aset arr bi
          (unchecked-byte
           (if (zero? (bit-and idx 1))
             (bit-or (bit-and b 0xF0) v)
             (bit-or (bit-and b 0x0F) (bit-shift-left v 4)))))))

(defn section-set-block ^Section [^Section s ^long idx ^long state]
  (let [b (aclone ^shorts (.blocks s))]
    (aset b idx (short state))
    (Section. b (.block-light s) (.sky-light s))))

(defn set-block [chunk lx y lz state]
  (let [y   (long y)
        si  (bit-shift-right y 4)
        idx (+ (* (bit-and y 15) 256) (* (long lz) 16) (long lx))
        s   (or (get (:sections chunk) si) empty-section)]
    (assoc-in chunk [:sections si] (section-set-block s idx state))))

(defn get-block
  ^long [chunk lx y lz]
  (let [y  (long y)
        si (bit-shift-right y 4)]
    (if-let [s (get (:sections chunk) si)]
      (bit-and (long (aget ^shorts (.blocks ^Section s)
                           (+ (* (bit-and y 15) 256) (* (long lz) 16) (long lx))))
               0xFFFF)
      0)))

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

(defn pos->id
  ^long [cx cz]
  (bit-or (bit-shift-left (bit-and (long cx) 0xFFFFFFFF) 32)
          (bit-and (long cz) 0xFFFFFFFF)))

(defn id->pos [chunk-id]

  [(long (unchecked-int (bit-shift-right (long chunk-id) 32)))
   (long (unchecked-int (bit-and (long chunk-id) 0xFFFFFFFF)))])

(defn around-ids [^long cx ^long cz ^long r]
  (for [dx (range (- r) (inc r))
        dz (range (- r) (inc r))]
    (pos->id (+ cx dx) (+ cz dz))))

(defn block-chunk
  ^long [[x _ z]]
  (pos->id (bit-shift-right (long x) 4) (bit-shift-right (long z) 4)))

(defn chunks-get-block
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

(defn chunks-set-blocks [chunks template changes]
  (if (empty? changes)
    chunks
    (let [cache (HashMap.)]
      (doseq [[[x y z] state] changes]
        (let [x  (long x) y (long y) z (long z)
              cp (pos->id (bit-shift-right x 4) (bit-shift-right z 4))
              si (bit-shift-right y 4)
              k  [cp si]
              ^shorts arr
              (or (.get cache k)
                  (let [c (get chunks cp template)
                        ^Section s (or (get (:sections c) si) empty-section)
                        a (aclone ^shorts (.blocks s))]
                    (.put cache k a)
                    a))]
          (aset arr (+ (* (bit-and y 15) 256) (* (bit-and z 15) 16) (bit-and x 15))
                (short state))))
      (reduce
       (fn [chs [[cp si] arr]]
         (let [c (get chs cp template)
               ^Section s (or (get (:sections c) si) empty-section)]
           (assoc chs cp
                  (assoc-in c [:sections si]
                            (->Section arr (.block-light s) (.sky-light s))))))
       chunks
       (sort-by key (into {} cache))))))

(defn encode-sections [chunk sis]
  (let [sections (into [] (keep (fn [si] (when-let [s (get (:sections chunk) si)] [si s]))) (sort sis))
        n        (count sections)
        data     (byte-array (* n 12288))
        bl-base  (* n 8192)
        sl-base  (+ bl-base (* n 2048))]
    (doseq [[i [_si s]] (map-indexed vector sections)]
      (let [^Section s s
            ^shorts bs (.blocks s)
            off        (* (long i) 8192)]
        (dotimes [j 4096]
          (let [v (bit-and (long (aget bs j)) 0xFFFF)]
            (aset data (+ off (* 2 j)) (unchecked-byte (bit-and v 0xFF)))
            (aset data (+ off (* 2 j) 1) (unchecked-byte (unsigned-bit-shift-right v 8)))))
        (System/arraycopy (.block-light s) 0 data (+ bl-base (* (long i) 2048)) 2048)
        (System/arraycopy (.sky-light s) 0 data (+ sl-base (* (long i) 2048)) 2048)))
    [(reduce (fn [m [si _]] (bit-or (long m) (bit-shift-left 1 (long si)))) 0 sections)
     data]))

(defn encode-column [chunk]
  (let [sections   (into [] (keep-indexed (fn [si s] (when s [si s]))) (:sections chunk))
        n          (count sections)
        data       (byte-array (+ (* n 12288) 256))
        bl-base    (* n 8192)
        sl-base    (+ bl-base (* n 2048))
        biome-base (+ sl-base (* n 2048))]
    (doseq [[i [_si s]] (map-indexed vector sections)]
      (let [^Section s s
            ^shorts bs (.blocks s)
            off        (* (long i) 8192)]
        (dotimes [j 4096]
          (let [v (bit-and (long (aget bs j)) 0xFFFF)]
            (aset data (+ off (* 2 j)) (unchecked-byte (bit-and v 0xFF)))
            (aset data (+ off (* 2 j) 1) (unchecked-byte (unsigned-bit-shift-right v 8)))))
        (System/arraycopy (.block-light s) 0 data (+ bl-base (* (long i) 2048)) 2048)
        (System/arraycopy (.sky-light s) 0 data (+ sl-base (* (long i) 2048)) 2048)))
    (Arrays/fill data (int biome-base) (alength data) (byte 1))
    [(reduce (fn [m [si _]] (bit-or (long m) (bit-shift-left 1 (long si)))) 0 sections)
     data]))
