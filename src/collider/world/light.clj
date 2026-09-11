(ns collider.world.light
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.weather :as weather])
  (:import (collider.world.chunk Section)
           (java.util ArrayDeque HashMap)))

(set! *warn-on-reflection* true)

(def ^:private ^:const SL 1)
(def ^:private DX (long-array [0 0 0 0 -1 1]))
(def ^:private DY (long-array [-1 1 0 0 0 0]))
(def ^:private DZ (long-array [0 0 -1 1 0 0]))
(def ^:private ^:const DOWN 0)
(def ^:private ^:const OFF 8388608)

(defn- pack ^long [^long x ^long y ^long z ^long l]
  (bit-or (bit-shift-left (+ x OFF) 38) (bit-shift-left (+ z OFF) 14)
          (bit-shift-left (inc (- y chunk/min-y)) 4) l))
(defn- px ^long [^long e] (- (bit-shift-right e 38) OFF))
(defn- pz ^long [^long e] (- (bit-and (bit-shift-right e 14) 0xFFFFFF) OFF))
(defn- py ^long [^long e] (+ chunk/min-y (dec (bit-and (bit-shift-right e 4) 0x3FF))))
(defn- pl ^long [^long e] (bit-and e 0xF))
(defn- l-idx ^long [^long x ^long y ^long z]
  (+ (* (bit-and y 15) 256) (* (bit-and z 15) 16) (bit-and x 15)))

(defn- chunk-at [chunks template x z]
  (get chunks (chunk/pos->id (bit-shift-right (long x) 4) (bit-shift-right (long z) 4)) template))

(defn- section ^Section [chunks template x y z]
  (let [x (long x) y (long y) z (long z)]
    (get (:sections (chunk-at chunks template x z)) (chunk/section-index y))))

(defn- absent-sky [chunks template x y z]
  (let [x (long x) y (long y) z (long z)]
    (chunk/nil-sky (chunk-at chunks template x z) (chunk/section-index y)
                   (bit-and x 15) (bit-and z 15))))

(defn- block-id-at [chunks template x y z]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (long (chunk/chunks-get-block chunks template x y z))
      0)))

(defn- light-key ^long [^long x ^long y ^long z ^long ch]
  (bit-or (bit-shift-left (chunk/pos->id (bit-shift-right x 4) (bit-shift-right z 4)) 6)
          (bit-shift-left (chunk/section-index y) 1)
          ch))

(defn- stored-l [chunks template ch x y z]
  (if (not (chunk/in-range? y))
    (if (and (= ch SL) (> y chunk/max-y)) 15 0)
    (if-let [s (section chunks template x y z)]
      (chunk/nibble-get (if (= ch SL) (.sky-light s) (.block-light s)) (l-idx x y z))
      (if (= ch SL) (absent-sky chunks template x y z) 0))))

(defn- get-l [^HashMap cache chunks template ch x y z]
  (let [ch (long ch) x (long x) y (long y) z (long z)]
    (if (not (chunk/in-range? y))
      (if (and (= ch SL) (> y chunk/max-y)) 15 0)
      (if-let [^bytes arr (.get cache (light-key x y z ch))]
        (chunk/nibble-get arr (l-idx x y z))
        (stored-l chunks template ch x y z)))))

(defn- set-l! [^HashMap cache chunks template ch x y z v]
  (let [ch (long ch) x (long x) y (long y) z (long z) v (long v)]
    (when (chunk/in-range? y)
      (let [k (light-key x y z ch)]
        (if-let [^bytes arr (.get cache k)]
          (do (chunk/nibble-set! arr (l-idx x y z) v) true)
          (let [s (section chunks template x y z)
                ^bytes src (if s
                             (if (= ch SL) (.sky-light ^Section s) (.block-light ^Section s))
                             (if (= ch SL)
                               (chunk/nil-sky-array (chunk-at chunks template x z) (chunk/section-index y))
                               (byte-array 2048)))
                ^bytes arr (aclone src)]
            (.put cache k arr)
            (chunk/nibble-set! arr (l-idx x y z) v)
            true))))))

(defn- edge-occluded? [^long top ^long bottom]
  (or (pos? (block/dampening bottom)) (block/shape-occludes? top bottom DOWN)))

(defn- sky-source-y ^long [chunks template ^long x ^long z]
  (loop [y chunk/max-y top 0]
    (if (< y chunk/min-y)
      chunk/min-y
      (let [b (long (block-id-at chunks template x y z))]
        (if (edge-occluded? top b) (inc y) (recur (dec y) b))))))

(defn- unlight! [^HashMap cache chunks template ch ^ArrayDeque rq ^ArrayDeque pq]
  (let [ch (long ch)]
    (loop []
      (when-let [e (.poll rq)]
        (let [e (long e) x (px e) y (py e) z (pz e) l (pl e)]
          (dotimes [d 6]
            (let [nx (+ x (aget ^longs DX d)) ny (+ y (aget ^longs DY d)) nz (+ z (aget ^longs DZ d))
                  ln (long (get-l cache chunks template ch nx ny nz))]
              (when (pos? ln)
                (if (< ln l)
                  (when (set-l! cache chunks template ch nx ny nz 0)
                    (.add rq (pack nx ny nz ln))
                    (let [em (block/emits (long (block-id-at chunks template nx ny nz)))]
                      (when (and (pos? em) (= ch 0))
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
            (let [from (long (block-id-at chunks template x y z))]
              (dotimes [d 6]
                (let [nx (+ x (aget ^longs DX d)) ny (+ y (aget ^longs DY d)) nz (+ z (aget ^longs DZ d))]
                  (when (chunk/in-range? ny)
                    (let [to   (long (block-id-at chunks template nx ny nz))
                          cand (- l (block/opacity to))]
                      (when (and (pos? cand)
                                 (> cand (long (get-l cache chunks template ch nx ny nz)))
                                 (not (block/shape-occludes? from to d))
                                 (set-l! cache chunks template ch nx ny nz cand))
                        (.add pq (pack nx ny nz cand))))))))))
        (recur)))))

(defn- seed-neighbors! [^HashMap cache chunks template ch ^ArrayDeque pq x y z]
  (let [x (long x) y (long y) z (long z)]
    (dotimes [d 6]
      (let [nx (+ x (aget ^longs DX d)) ny (+ y (aget ^longs DY d)) nz (+ z (aget ^longs DZ d))
            ln (long (get-l cache chunks template ch nx ny nz))]
        (when (pos? ln)
          (.add pq (pack nx ny nz ln)))))))

(defn- rebuild [chunks template ^HashMap cache]
  (reduce
   (fn [chs k]
     (let [k (long k) ^bytes arr (.get cache k)
           cp (bit-shift-right k 6) si (bit-and (bit-shift-right k 1) 31) ch (bit-and k 1)
           c  (get chs cp template)
           ^Section s (or (get (:sections c) si) (chunk/new-section c si))]
       (assoc chs cp
              (assoc-in c [:sections si]
                        (if (= ch SL)
                          (chunk/->Section (.blocks s) (.block-light s) arr)
                          (chunk/->Section (.blocks s) arr (.sky-light s)))))))
   chunks
   (reverse (sort (keys cache)))))

(defn- channel-pass! [^HashMap cache chunks template ch cells]
  (let [ch (long ch)
        rq (ArrayDeque.)
        pq (ArrayDeque.)]
    (doseq [[x y z _] cells]
      (let [cur (long (get-l cache chunks template ch x y z))]
        (when (pos? cur)
          (set-l! cache chunks template ch x y z 0)
          (.add rq (pack x y z cur)))))
    (unlight! cache chunks template ch rq pq)
    (doseq [[x y z source] cells]
      (let [x (long x) y (long y) z (long z) source (long source)]
        (when (and (pos? source) (> source (long (get-l cache chunks template ch x y z))))
          (set-l! cache chunks template ch x y z source)
          (.add pq (pack x y z source)))
        (seed-neighbors! cache chunks template ch pq x y z)))
    (propagate! cache chunks template ch pq)))

(defn light-at [chunks template x y z]
  (if (not (chunk/in-range? y))
    (if (> (long y) chunk/max-y) 15 0)
    (if-let [s (section chunks template x y z)]
      (max (chunk/nibble-get (.sky-light ^Section s) (l-idx x y z))
           (chunk/nibble-get (.block-light ^Section s) (l-idx x y z)))
      (absent-sky chunks template x y z))))

(defn block-light-at [chunks template x y z]
  (if (not (chunk/in-range? y))
    0
    (if-let [s (section chunks template x y z)]
      (chunk/nibble-get (.block-light ^Section s) (l-idx x y z))
      0)))

(defn sky-light-at [chunks template x y z]
  (if (not (chunk/in-range? y))
    (if (> (long y) chunk/max-y) 15 0)
    (if-let [s (section chunks template x y z)]
      (chunk/nibble-get (.sky-light ^Section s) (l-idx x y z))
      (absent-sky chunks template x y z))))

(def ^:private ^:const day-period 24000)

(def ^:private sky-level-segments
  (let [ks [133 11867 13670 22330]
        vs [(float 1.0) (float 1.0) (float 0.26666668) (float 0.26666668)]
        n  (dec (count ks))]
    (vec (concat [[(- (long (ks n)) day-period) (vs n) (ks 0) (vs 0)]]
                 (for [i (range n)] [(ks i) (vs i) (ks (inc i)) (vs (inc i))])
                 [[(ks n) (vs n) (+ (long (ks 0)) day-period) (vs 0)]]))))

(defn- sky-level-segment [^long t]
  (or (first (filter (fn [seg] (< t (long (seg 2)))) sky-level-segments))
      (peek sky-level-segments)))

(defn- sky-level-factor ^double [^long time]
  (let [t   (mod time day-period)
        seg (sky-level-segment t)
        v0  (float (seg 1))
        v1  (float (seg 3))
        a   (float (/ (float (- t (long (seg 0)))) (float (- (long (seg 2)) (long (seg 0))))))]
    (float (+ v0 (float (* a (float (- v1 v0))))))))

(defn- blend ^double [^double value ^double alpha ^double target ^double weight]
  (let [v  (float value)
        to (float (+ v (float (* (float alpha) (float (- (float target) v))))))]
    (float (+ v (float (* (float weight) (float (- to v))))))))

(defn sky-light-level ^double [^long time]
  (let [thunder (float (weather/thunder-level nil))
        rain    (float (- (float (weather/rain-level nil)) thunder))
        v       (float (* (float 15.0) (float (sky-level-factor time))))
        v       (float (if (pos? rain) (blend v 0.3125 4.0 rain) v))
        v       (float (if (pos? thunder) (blend v 0.52734375 4.0 thunder) v))]
    (float (min (float 15.0) (max (float 0.0) v)))))

(defn sky-darken ^long [^long time]
  (long (int (float (- (float 15.0) (float (sky-light-level time)))))))

(defn brightness [chunks template x y z time]
  (max (- (long (sky-light-at chunks template x y z)) (sky-darken (long time)))
       (long (block-light-at chunks template x y z))))

(defn- different? [^long old ^long new]
  (and (not= old new)
       (or (not= (block/dampening old) (block/dampening new))
           (not= (block/emits old) (block/emits new))
           (block/use-shape-for-light-occlusion? old)
           (block/use-shape-for-light-occlusion? new))))

(defn- sky-cells [chunks template x y z]
  (let [x (long x) y (long y) z (long z)
        src (sky-source-y chunks template x z)
        drop (loop [yy (dec src) acc []]
               (if (and (chunk/in-range? yy) (= 15 (long (stored-l chunks template SL x yy z))))
                 (recur (dec yy) (conj acc [x yy z 0]))
                 acc))
        add  (loop [yy src acc []]
               (if (and (<= yy chunk/max-y) (not= 15 (long (stored-l chunks template SL x yy z))))
                 (recur (inc yy) (conj acc [x yy z 15]))
                 acc))]
    (conj (into drop add) [x y z (if (>= y src) 15 0)])))

(defn relight-batch [chunks template changes]
  (let [changed (filter (fn [[_ old new]] (different? (long old) (long new))) changes)
        bcells  (mapv (fn [[[x y z] _ new]] [x y z (block/emits (long new))]) changed)
        scells  (into [] (comp (mapcat (fn [[[x y z] _ _]] (sky-cells chunks template x y z))) (distinct))
                      changed)]
    (if (empty? bcells)
      chunks
      (let [cache (HashMap.)]
        (channel-pass! cache chunks template 0 bcells)
        (channel-pass! cache chunks template SL scells)
        (if (.isEmpty cache) chunks (rebuild chunks template cache))))))

(defn relight [chunks template pos old-state new-state]
  (relight-batch chunks template [[pos old-state new-state]]))
