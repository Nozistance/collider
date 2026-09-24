(ns collider.world.light
  "Block light, sky light, and the sky brightness of the day cycle."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (java.util ArrayDeque HashMap)))

(set! *warn-on-reflection* true)

(def ^:private ^:const SL 1)

(def ^:private DX (long-array [0 0 0 0 -1 1]))

(def ^:private DY (long-array [-1 1 0 0 0 0]))

(def ^:private DZ (long-array [0 0 -1 1 0 0]))

(def ^:private ^:const DOWN 0)

(def ^:private ^:const OFF 8388608)

(defn- pack ^long [^long x ^long y ^long z ^long l]
  (bit-or (bit-shift-left (+ x OFF) 38)
          (bit-shift-left (+ z OFF) 14)
          (bit-shift-left (inc (- y chunk/min-y)) 4)
          l))

(defn- px ^long [^long e] (- (bit-shift-right e 38) OFF))

(defn- pz ^long [^long e]
  (- (bit-and (bit-shift-right e 14) 0xFFFFFF) OFF))

(defn- py ^long [^long e]
  (+ chunk/min-y (dec (bit-and (bit-shift-right e 4) 0x3FF))))

(defn- pl ^long [^long e] (bit-and e 0xF))

(defn- l-idx ^long [^long x ^long y ^long z]
  (+ (* (bit-and y 15) 256) (* (bit-and z 15) 16) (bit-and x 15)))

(defn- chunk-at [chunks x z]
  (chunk/chunk-at chunks (bit-shift-right (long x) 4)
                  (bit-shift-right (long z) 4)))

(defn- section [chunks x y z]
  (chunk/section-at chunks x y z))

(defn- absent-sky [chunks x y z]
  (let [x (long x) y (long y) z (long z)]
    (chunk/nil-sky (chunk-at chunks x z) (chunk/section-index y)
                   (bit-and x 15) (bit-and z 15))))

(defn- block-id-at [chunks x y z]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (long (chunk/chunks-get-block chunks x y z))
      0)))

(defn- light-key ^long [^long x ^long y ^long z ^long ch]
  (let [cx (bit-shift-right x 4)
        cz (bit-shift-right z 4)]
    (bit-or (bit-shift-left (chunk/pos->id cx cz) 6)
            (bit-shift-left (chunk/section-index y) 1)
            ch)))

(defn- stored-in ^long [s ^long ch ^long idx]
  (if (= ch SL) (chunk/sky-light s idx) (chunk/block-light s idx)))

(defn- stored-l [chunks ch x y z]
  (if (not (chunk/in-range? y))
    (if (and (= ch SL) (> y chunk/max-y)) 15 0)
    (if-let [s (section chunks x y z)]
      (stored-in s ch (l-idx x y z))
      (if (= ch SL) (absent-sky chunks x y z) 0))))

(defn- get-l [^HashMap cache chunks ch x y z]
  (let [ch (long ch) x (long x) y (long y) z (long z)]
    (if (not (chunk/in-range? y))
      (if (and (= ch SL) (> y chunk/max-y)) 15 0)
      (if-let [^bytes arr (.get cache (light-key x y z ch))]
        (chunk/nibble-get arr (l-idx x y z))
        (stored-l chunks ch x y z)))))

(defn- fresh-light ^bytes [chunks ch x y z]
  (let [ch (long ch) x (long x) y (long y) z (long z)
        c (chunk-at chunks x z)
        s (or (section chunks x y z)
              (chunk/new-section c (chunk/section-index y)))]
    (if (= ch SL)
      (chunk/sky-light-copy s)
      (chunk/block-light-copy s))))

(defn- set-l! [^HashMap cache chunks ch x y z v]
  (let [ch (long ch) x (long x) y (long y) z (long z) v (long v)]
    (when (chunk/in-range? y)
      (let [k (light-key x y z ch)]
        (if-let [^bytes arr (.get cache k)]
          (do (chunk/nibble-set! arr (l-idx x y z) v) true)
          (let [^bytes arr (fresh-light chunks ch x y z)]
            (.put cache k arr)
            (chunk/nibble-set! arr (l-idx x y z) v)
            true))))))

(defn- edge-occluded? [^long top ^long bottom]
  (or (pos? (block/dampening bottom))
      (block/shape-occludes? top bottom DOWN)))

(defn- sky-source-y ^long [chunks ^long x ^long z]
  (loop [y chunk/max-y top 0]
    (if (< y chunk/min-y)
      chunk/min-y
      (let [b (long (block-id-at chunks x y z))]
        (if (edge-occluded? top b) (inc y) (recur (dec y) b))))))

(defn- re-emit! [^HashMap cache chunks ^ArrayDeque pq ch nx ny nz]
  (let [ch (long ch) nx (long nx) ny (long ny) nz (long nz)
        em (block/emits (long (block-id-at chunks nx ny nz)))]
    (when (and (pos? em) (= ch 0))
      (set-l! cache chunks ch nx ny nz em)
      (.add pq (pack nx ny nz em)))))

(defn- unlight-at! [^HashMap cache chunks ^ArrayDeque rq
                    ^ArrayDeque pq ch e d]
  (let [ch (long ch) e (long e) d (long d)
        nx (+ (px e) (aget ^longs DX d))
        ny (+ (py e) (aget ^longs DY d))
        nz (+ (pz e) (aget ^longs DZ d))
        ln (long (get-l cache chunks ch nx ny nz))]
    (when (pos? ln)
      (if (< ln (pl e))
        (when (set-l! cache chunks ch nx ny nz 0)
          (.add rq (pack nx ny nz ln))
          (re-emit! cache chunks pq ch nx ny nz))
        (.add pq (pack nx ny nz ln))))))

(defn- unlight! [^HashMap cache chunks ch
                 ^ArrayDeque rq ^ArrayDeque pq]
  (loop []
    (when-let [e (.poll rq)]
      (dotimes [d 6]
        (unlight-at! cache chunks rq pq ch e d))
      (recur))))

(defn- propagate-to! [^HashMap cache chunks ^ArrayDeque pq
                      ch from e d]
  (let [ch (long ch) from (long from) e (long e) d (long d)
        nx (+ (px e) (aget ^longs DX d))
        ny (+ (py e) (aget ^longs DY d))
        nz (+ (pz e) (aget ^longs DZ d))]
    (when (chunk/in-range? ny)
      (let [to (long (block-id-at chunks nx ny nz))
            cand (- (pl e) (block/opacity to))]
        (when (and (pos? cand)
                   (> cand (long (get-l cache chunks ch nx ny nz)))
                   (not (block/shape-occludes? from to d))
                   (set-l! cache chunks ch nx ny nz cand))
          (.add pq (pack nx ny nz cand)))))))

(defn- propagate-from! [^HashMap cache chunks ^ArrayDeque pq
                        ch e]
  (let [ch (long ch) e (long e) x (px e) y (py e) z (pz e)]
    (when (= (pl e) (long (get-l cache chunks ch x y z)))
      (let [from (long (block-id-at chunks x y z))]
        (dotimes [d 6]
          (propagate-to! cache chunks pq ch from e d))))))

(defn- propagate! [^HashMap cache chunks ch ^ArrayDeque pq]
  (loop []
    (when-let [e (.poll pq)]
      (propagate-from! cache chunks pq ch e)
      (recur))))

(defn- seed-neighbors! [^HashMap cache chunks ch ^ArrayDeque pq
                        x y z]
  (let [x (long x) y (long y) z (long z)]
    (dotimes [d 6]
      (let [nx (+ x (aget ^longs DX d))
            ny (+ y (aget ^longs DY d))
            nz (+ z (aget ^longs DZ d))
            ln (long (get-l cache chunks ch nx ny nz))]
        (when (pos? ln)
          (.add pq (pack nx ny nz ln)))))))

(defn- relit [c [[_ k] ^bytes arr]]
  (let [k (long k)
        si (int (bit-and (bit-shift-right k 1) 31))
        s (or (chunk/chunk-section c si) (chunk/new-section c si))]
    (chunk/with-section c si
      (if (= (bit-and k 1) SL)
        (chunk/with-sky-light s arr)
        (chunk/with-block-light s arr)))))

(defn- rebuild [chunks ^HashMap cache]
  (let [ks (reverse (sort (keys cache)))
        entry (fn [k]
                [[(bit-shift-right (long k) 6) k] (.get cache k)])]
    (chunk/with-chunks chunks relit
      (partition-by ffirst (map entry ks)))))

(defn- clear-cell! [^HashMap cache chunks ch ^ArrayDeque rq cell]
  (let [[x y z _] cell
        cur (long (get-l cache chunks ch x y z))]
    (when (pos? cur)
      (set-l! cache chunks ch x y z 0)
      (.add rq (pack x y z cur)))))

(defn- seed-cell! [^HashMap cache chunks ch ^ArrayDeque pq cell]
  (let [[x y z source] cell
        x (long x) y (long y) z (long z) source (long source)]
    (when (and (pos? source)
               (> source (long (get-l cache chunks ch x y z))))
      (set-l! cache chunks ch x y z source)
      (.add pq (pack x y z source)))
    (seed-neighbors! cache chunks ch pq x y z)))

(defn- channel-pass! [^HashMap cache chunks ch cells]
  (let [ch (long ch) rq (ArrayDeque.) pq (ArrayDeque.)]
    (doseq [cell cells] (clear-cell! cache chunks ch rq cell))
    (unlight! cache chunks ch rq pq)
    (doseq [cell cells] (seed-cell! cache chunks ch pq cell))
    (propagate! cache chunks ch pq)))

(defn light-at
  "Returns the brighter of the sky and block light at x y z."
  [chunks x y z]
  (if (not (chunk/in-range? y))
    (if (> (long y) chunk/max-y) 15 0)
    (if-let [s (section chunks x y z)]
      (max (chunk/sky-light s (l-idx x y z))
           (chunk/block-light s (l-idx x y z)))
      (absent-sky chunks x y z))))

(defn block-light-at
  "Returns the block light at x y z."
  [chunks x y z]
  (if (not (chunk/in-range? y))
    0
    (if-let [s (section chunks x y z)]
      (chunk/block-light s (l-idx x y z))
      0)))

(defn sky-light-at
  "Returns the sky light at x y z, full above the world."
  [chunks x y z]
  (if (not (chunk/in-range? y))
    (if (> (long y) chunk/max-y) 15 0)
    (if-let [s (section chunks x y z)]
      (chunk/sky-light s (l-idx x y z))
      (absent-sky chunks x y z))))

(def ^:private ^:const day-period 24000)

(def ^:private sky-level-segments
  (let [ks [133 11867 13670 22330]
        dusk (float 0.26666668)
        vs [(float 1.0) (float 1.0) dusk dusk]
        n (dec (count ks))]
    (vec (concat [[(- (long (ks n)) day-period) (vs n) (ks 0) (vs 0)]]
                 (for [i (range n)]
                   [(ks i) (vs i) (ks (inc i)) (vs (inc i))])
                 [[(ks n) (vs n)
                   (+ (long (ks 0)) day-period) (vs 0)]]))))

(defn- sky-level-segment [^long t]
  (or (first (filter #(< t (long (% 2))) sky-level-segments))
      (peek sky-level-segments)))

(defn- sky-level-factor ^double [^long time]
  (let [t (mod time day-period)
        seg (sky-level-segment t)
        t0 (long (seg 0))
        t1 (long (seg 2))
        v0 (float (seg 1))
        v1 (float (seg 3))
        a (float (/ (float (- t t0)) (float (- t1 t0))))]
    (float (+ v0 (float (* a (float (- v1 v0))))))))

(defn- blend
  ^double [^double value ^double alpha ^double target ^double weight]
  (let [v (float value)
        gap (float (- (float target) v))
        to (float (+ v (float (* (float alpha) gap))))]
    (float (+ v (float (* (float weight) (float (- to v))))))))

(defn sky-light-level
  "Returns the brightness of the sky, 0.0 to 15.0, at a time of day.
  The rain and thunder levels, 0.0 to 1.0, dim it."
  (^double [^long time] (sky-light-level time 0.0 0.0))
  (^double [^long time ^double rain-level ^double thunder-level]
   (let [thunder (float thunder-level)
         rain (float (- (float rain-level) thunder))
         v (float (* (float 15.0) (float (sky-level-factor time))))
         v (float (if (pos? rain) (blend v 0.3125 4.0 rain) v))
         v (float (if (pos? thunder)
                    (blend v 0.52734375 4.0 thunder)
                    v))]
     (float (min (float 15.0) (max (float 0.0) v))))))

(defn sky-darken
  "Returns how much the sky light is dimmed, 0 to 15.
  The time of day and the weather decide."
  (^long [^long time] (sky-darken time 0.0 0.0))
  (^long [^long time ^double rain-level ^double thunder-level]
   (let [l (float (sky-light-level time rain-level thunder-level))]
     (long (int (float (- (float 15.0) l)))))))

(defn brightness
  "Returns the light level at x y z.
  The time of day and the weather dim the sky part."
  ([chunks x y z time] (brightness chunks x y z time 0.0 0.0))
  ([chunks x y z time rain-level thunder-level]
   (let [rain (double rain-level)
         dark (sky-darken (long time) rain (double thunder-level))]
     (max (- (long (sky-light-at chunks x y z)) dark)
          (long (block-light-at chunks x y z))))))

(defn- different? [^long old ^long new]
  (and (not= old new)
       (or (not= (block/dampening old) (block/dampening new))
           (not= (block/emits old) (block/emits new))
           (block/use-shape-for-light-occlusion? old)
           (block/use-shape-for-light-occlusion? new))))

(defn- sky-full? [chunks x y z]
  (= 15 (long (stored-l chunks SL x y z))))

(defn- sky-drop [chunks ^long x ^long z ^long src]
  (loop [yy (dec src) acc []]
    (if (and (chunk/in-range? yy) (sky-full? chunks x yy z))
      (recur (dec yy) (conj acc [x yy z 0]))
      acc)))

(defn- sky-add [chunks ^long x ^long z ^long src]
  (loop [yy src acc []]
    (if (and (<= yy chunk/max-y) (not (sky-full? chunks x yy z)))
      (recur (inc yy) (conj acc [x yy z 15]))
      acc)))

(defn- sky-column [chunks x z ys]
  (let [x (long x) z (long z)
        src (sky-source-y chunks x z)
        drop (sky-drop chunks x z src)
        add (sky-add chunks x z src)
        seen (into #{} (map second) (concat drop add))
        cell (fn [y]
               (when-not (seen y)
                 [x y z (if (>= (long y) src) 15 0)]))]
    (into (into drop add) (keep cell) ys)))

(defn- columns [changed]
  (let [m (HashMap.)]
    (doseq [[[x y z] _ _] changed]
      (let [k (chunk/pos->id x z)]
        (.put m k (conj (.getOrDefault m k #{}) y))))
    m))

(defn- column-cells [chunks [k ys]]
  (let [[x z] (chunk/id->pos k)]
    (sky-column chunks x z ys)))

(defn- sky-cells [chunks changed]
  (into [] (mapcat #(column-cells chunks %)) (columns changed)))

(defn- light-changed? [[_ old new]]
  (different? (long old) (long new)))

(defn- block-cells [changed]
  (mapv (fn [[[x y z] _ new]] [x y z (block/emits (long new))])
        changed))

(defn- relight-changed [chunks changed bcells sky?]
  (let [cache (HashMap.)]
    (channel-pass! cache chunks 0 bcells)
    (when sky?
      (channel-pass! cache chunks SL (sky-cells chunks changed)))
    (if (.isEmpty cache) chunks (rebuild chunks cache))))

(defn relight-batch
  "Returns chunks relit after changes, [pos old new] each.
  Sky light moves only when sky? is true."
  ([chunks changes] (relight-batch chunks changes true))
  ([chunks changes sky?]
   (let [changed (filter light-changed? changes)
         bcells (block-cells changed)]
     (if (empty? bcells)
       chunks
       (relight-changed chunks changed bcells sky?)))))

(defn relight
  "Returns chunks relit after the block at pos changed state."
  [chunks pos old-state new-state]
  (relight-batch chunks [[pos old-state new-state]]))
