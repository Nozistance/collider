(ns collider.world.light

  (:require [collider.world.chunk :as chunk])
  (:import (collider.world.chunk Section)
           (java.util ArrayDeque Arrays HashMap)))

(set! *warn-on-reflection* true)

(def emission
  {10 15, 11 15, 50 14, 51 15, 62 13, 76 7, 89 15, 91 15, 124 15, 138 15, 169 15})

(def ^:private opacity-table
  {0 0, 6 0, 8 3, 9 3, 18 1, 20 0, 26 0, 27 0, 28 0, 30 0, 31 0, 32 0,
   37 0, 38 0, 39 0, 40 0, 50 0, 51 0, 54 0, 55 0, 63 0, 64 0, 65 0, 66 0,
   68 0, 69 0, 70 0, 71 0, 72 0, 75 0, 76 0, 77 0, 79 3, 83 0, 85 0, 90 0,
   95 0, 96 0, 101 0, 102 0, 106 0, 107 0, 111 0, 117 0, 131 0, 132 0,
   143 0, 145 0, 154 0, 160 0, 161 1, 165 0, 166 0, 167 0, 175 0, 183 0,
   184 0, 185 0, 186 0, 187 0})

(def ^:private ^ints opacity-arr
  (let [a (int-array 256)] (Arrays/fill a (int 255))
    (doseq [[id op] opacity-table] (aset a (long id) (int op))) a))
(def ^:private ^ints emission-arr
  (let [a (int-array 256)] (doseq [[id em] emission] (aset a (long id) (int em))) a))

(defn- opacity ^long [^long id] (if (< -1 id 256) (aget ^ints opacity-arr id) 255))
(defn- emits ^long [^long id] (if (< -1 id 256) (aget ^ints emission-arr id) 0))
(def ^:private ^:const SL 1)

(def ^:private DX (long-array [1 -1 0 0 0 0]))
(def ^:private DY (long-array [0 0 1 -1 0 0]))
(def ^:private DZ (long-array [0 0 0 0 1 -1]))
(def ^:private ^:const OFF 8388608)
(defn- pack ^long [^long x ^long y ^long z ^long l]
  (bit-or (bit-shift-left (+ x OFF) 38) (bit-shift-left (+ z OFF) 14) (bit-shift-left (inc y) 4) l))
(defn- px ^long [^long e] (- (bit-shift-right e 38) OFF))
(defn- pz ^long [^long e] (- (bit-and (bit-shift-right e 14) 0xFFFFFF) OFF))
(defn- py ^long [^long e] (dec (bit-and (bit-shift-right e 4) 0x3FF)))
(defn- pl ^long [^long e] (bit-and e 0xF))
(defn- l-idx ^long [^long x ^long y ^long z]
  (+ (* (bit-and y 15) 256) (* (bit-and z 15) 16) (bit-and x 15)))

(defn- section ^Section [chunks template x y z]
  (let [x (long x) y (long y) z (long z)]
    (get (:sections (get chunks (chunk/pos->id (bit-shift-right x 4) (bit-shift-right z 4)) template))
         (bit-shift-right y 4))))

(defn- block-id-at [chunks template x y z]
  (let [y (long y)]
    (if (or (neg? y) (> y 255))
      0
      (bit-shift-right (long (chunk/chunks-get-block chunks template x y z)) 4))))

(defn- light-key ^long [^long x ^long y ^long z ^long ch]
  (bit-or (bit-shift-left (chunk/pos->id (bit-shift-right x 4) (bit-shift-right z 4)) 5)
          (bit-shift-left (bit-shift-right y 4) 1)
          ch))

(defn- get-l [^HashMap cache chunks template ch x y z]
  (let [ch (long ch) x (long x) y (long y) z (long z)]
    (if (or (neg? y) (> y 255))
      (if (and (= ch SL) (> y 255)) 15 0)
      (if-let [^bytes arr (.get cache (light-key x y z ch))]
        (chunk/nibble-get arr (l-idx x y z))
        (if-let [s (section chunks template x y z)]
          (chunk/nibble-get (if (= ch SL) (.sky-light s) (.block-light s)) (l-idx x y z))
          (if (= ch SL) 15 0))))))

(defn- set-l! [^HashMap cache chunks template ch x y z v]
  (let [ch (long ch) x (long x) y (long y) z (long z) v (long v)]
    (when (<= 0 y 255)
      (let [k (light-key x y z ch)]
        (if-let [^bytes arr (.get cache k)]
          (do (chunk/nibble-set! arr (l-idx x y z) v) true)
          (when-let [s (section chunks template x y z)]
            (let [^bytes src (if (= ch SL) (.sky-light s) (.block-light s))
                  ^bytes arr (aclone src)]
              (.put cache k arr)
              (chunk/nibble-set! arr (l-idx x y z) v)
              true)))))))

(defn- unlight! [^HashMap cache chunks template ch ^ArrayDeque rq ^ArrayDeque pq]
  (let [ch (long ch)]
    (loop []
      (when-let [e (.poll rq)]
        (let [e (long e) x (px e) y (py e) z (pz e) l (pl e)]
          (dotimes [d 6]
            (let [dy (aget ^longs DY d)
                  nx (+ x (aget ^longs DX d)) ny (+ y dy) nz (+ z (aget ^longs DZ d))
                  ln (long (get-l cache chunks template ch nx ny nz))]
              (when (pos? ln)
                (if (or (< ln l)
                        (and (= ch SL) (= l 15) (= ln 15) (= dy -1)))
                  (when (set-l! cache chunks template ch nx ny nz 0)
                    (.add rq (pack nx ny nz ln))
                    (let [em (emits (long (block-id-at chunks template nx ny nz)))]
                      (when (pos? em)
                        (set-l! cache chunks template ch nx ny nz em)
                        (.add pq (pack nx ny nz em)))))
                  (.add pq (pack nx ny nz ln)))))))
        (recur)))))

(defn- propagate! [^HashMap cache chunks template ch ^ArrayDeque pq]
  (let [ch (long ch)]
    (loop []
      (when-let [e (.poll pq)]
        (let [e (long e) x (px e) y (py e) z (pz e) l (pl e)]
          (when (= l (long (get-l cache chunks template ch x y z)))
            (dotimes [d 6]
              (let [dy (aget ^longs DY d)
                    nx (+ x (aget ^longs DX d)) ny (+ y dy) nz (+ z (aget ^longs DZ d))]
                (when (<= 0 ny 255)
                  (let [op   (opacity (long (block-id-at chunks template nx ny nz)))
                        cand (if (and (= ch SL) (= l 15) (= dy -1) (zero? op))
                               15
                               (- l (max 1 op)))]
                    (when (and (pos? cand)
                               (> cand (long (get-l cache chunks template ch nx ny nz)))
                               (set-l! cache chunks template ch nx ny nz cand))
                      (.add pq (pack nx ny nz cand)))))))))
        (recur)))))

(defn- seed-neighbors! [^HashMap cache chunks template ch ^ArrayDeque pq x y z]
  (let [x (long x) y (long y) z (long z)]
    (dotimes [d 6]
      (let [nx (+ x (aget ^longs DX d)) ny (+ y (aget ^longs DY d)) nz (+ z (aget ^longs DZ d))
            ln (long (get-l cache chunks template ch nx ny nz))]
        (when (pos? ln)
          (.add pq (pack nx ny nz ln)))))))

(defn- sky-visible? [chunks template x y z]
  (let [x (long x) y (long y) z (long z)
        cp (chunk/pos->id (bit-shift-right x 4) (bit-shift-right z 4))
        c    (get chunks cp template)
        secs (:sections c)
        top  (long (loop [si 15] (cond (neg? si) 0 (nth secs si) si :else (recur (dec si)))))
        ymax (+ (* 16 top) 15)]
    (loop [yy (inc y)]
      (cond
        (> yy ymax) true
        (pos? (opacity (bit-shift-right (chunk/get-block c (bit-and x 15) yy (bit-and z 15)) 4))) false
        :else (recur (inc yy))))))

(defn- rebuild [chunks template ^HashMap cache]
  (reduce
   (fn [chs k]
     (let [k (long k) ^bytes arr (.get cache k)
           cp (bit-shift-right k 5) si (bit-and (bit-shift-right k 1) 15) ch (bit-and k 1)
           c  (get chs cp template)
           ^Section s (get (:sections c) si)]
       (if (nil? s)
         chs
         (assoc chs cp
                (assoc-in c [:sections si]
                          (if (= ch SL)
                            (chunk/->Section (.blocks s) (.block-light s) arr)
                            (chunk/->Section (.blocks s) arr (.sky-light s))))))))
   chunks
   (sort (keys cache))))

(defn- channel-pass! [^HashMap cache chunks template ch cells]
  (let [ch (long ch)
        rq (ArrayDeque.)
        pq (ArrayDeque.)]
    (doseq [[x y z _] cells]
      (let [x (long x) y (long y) z (long z)
            cur (long (get-l cache chunks template ch x y z))]
        (when (pos? cur)
          (set-l! cache chunks template ch x y z 0)
          (.add rq (pack x y z cur)))))
    (unlight! cache chunks template ch rq pq)
    (doseq [[x y z source] cells]
      (let [x (long x) y (long y) z (long z) source (long source)]
        (when (and (pos? source)
                   (> source (long (get-l cache chunks template ch x y z))))
          (set-l! cache chunks template ch x y z source)
          (.add pq (pack x y z source)))
        (seed-neighbors! cache chunks template ch pq x y z)))
    (propagate! cache chunks template ch pq)))

(defn blocks-light? [^long state]
  (> (opacity (bit-shift-right state 4)) 2))

(defn light-at [chunks template x y z]
  (if (or (neg? (long y)) (> (long y) 255))
    (if (> (long y) 255) 15 0)
    (if-let [s (section chunks template x y z)]
      (max (chunk/nibble-get (.sky-light ^Section s) (l-idx x y z))
           (chunk/nibble-get (.block-light ^Section s) (l-idx x y z)))
      15)))

(defn relight-batch [chunks template changes]
  (let [[bcells scells]
        (reduce
         (fn [[b s] [pos old new]]
           (let [[x y z] pos
                 old-id (bit-shift-right (long old) 4)
                 new-id (bit-shift-right (long new) 4)
                 op?    (not= (opacity old-id) (opacity new-id))
                 em-new (emits new-id)]
             (if (or op? (not= (emits old-id) em-new))
               [(conj b [x y z em-new])
                (if op?
                  (conj s [x y z (if (and (zero? (opacity new-id))
                                          (sky-visible? chunks template x y z))
                                   15 0)])
                  s)]
               [b s])))
         [[] []] changes)]
    (if (and (empty? bcells) (empty? scells))
      chunks
      (let [cache (HashMap.)]
        (when (seq bcells) (channel-pass! cache chunks template 0 bcells))
        (when (seq scells) (channel-pass! cache chunks template SL scells))
        (if (.isEmpty cache)
          chunks
          (rebuild chunks template cache))))))

(defn relight [chunks template pos old-state new-state]
  (relight-batch chunks template [[pos old-state new-state]]))
