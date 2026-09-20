(ns collider.world.space.explosion
  "Explosions, the blocks they break and their drops.
  Also their reach into a body."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.java Rays)))

(set! *warn-on-reflection* true)

(def ^:private ^:const region-r 10)

(deftype Region [^objects grid ^objects cols ^long cx0 ^long cz0
                 ^long sy0 ^long ncx ^long ncz ^long nsy
                 oracle ^clojure.lang.Atom loaded])

(defn- rg-grid ^objects [^Region rg] (.grid rg))

(defn- rg-cols ^objects [^Region rg] (.cols rg))

(defn- rg-oracle [^Region rg] (.oracle rg))

(defn loaded-payloads
  "Returns the chunks the reads pulled in, by id."
  [^Region rg]
  @(.loaded ^Region rg))

(defn- rg-cx0 ^long [^Region rg] (.cx0 rg))

(defn- rg-cz0 ^long [^Region rg] (.cz0 rg))

(defn- rg-sy0 ^long [^Region rg] (.sy0 rg))

(defn- rg-ncx ^long [^Region rg] (.ncx rg))

(defn- rg-ncz ^long [^Region rg] (.ncz rg))

(defn- rg-nsy ^long [^Region rg] (.nsy rg))

(defn- region-bounds [[cx cy cz]]
  (let [cx0 (bit-shift-right (- (long cx) region-r) 4)
        cx1 (bit-shift-right (+ (long cx) region-r) 4)
        cz0 (bit-shift-right (- (long cz) region-r) 4)
        cz1 (bit-shift-right (+ (long cz) region-r) 4)
        sy0 (max (bit-shift-right chunk/min-y 4) (bit-shift-right (- (long cy) region-r) 4))
        sy1 (min (bit-shift-right chunk/max-y 4) (bit-shift-right (+ (long cy) region-r) 4))]
    [cx0 cz0 sy0 (inc (- cx1 cx0)) (inc (- cz1 cz0)) (inc (- sy1 sy0))]))

(defn- section-at [col ^long sy]
  (chunk/chunk-section col (+ sy chunk/section-offset)))

(defn- put-column [^Region rg ^long ix ^long iz col]
  (aset ^objects (rg-cols rg) (+ (* ix (rg-ncz rg)) iz) col)
  (when col
    (dotimes [iy (rg-nsy rg)]
      (aset ^objects (rg-grid rg)
            (+ (* (+ (* ix (rg-ncz rg)) iz) (rg-nsy rg)) iy)
            (section-at col (+ (rg-sy0 rg) iy))))))

(defn- fill-grid [^Region rg chunks]
  (dotimes [ix (rg-ncx rg)]
    (dotimes [iz (rg-ncz rg)]
      (put-column rg ix iz
                  (get chunks (chunk/pos->id (+ (rg-cx0 rg) ix)
                                             (+ (rg-cz0 rg) iz)))))))

(defn block-reader
  "Returns a reader over the blocks around pos.
  An absent chunk is asked of oracle, which answers with the
  payload it reads or generates, as a mid-tick read does."
  (^Region [chunks pos] (block-reader chunks pos nil))
  (^Region [chunks pos oracle]
   (let [[cx0 cz0 sy0 ncx ncz nsy] (region-bounds pos)
         n (* (long ncx) (long ncz))
         rg (Region. (object-array (* n (long nsy))) (object-array n)
                     cx0 cz0 sy0 ncx ncz nsy oracle (atom {}))]
     (fill-grid rg chunks)
     rg)))

(defn- summon [^Region rg ^long ix ^long iz]
  (let [id (chunk/pos->id (+ (rg-cx0 rg) ix) (+ (rg-cz0 rg) iz))
        payload ((rg-oracle rg) id)]
    (swap! (.loaded ^Region rg) assoc id payload)
    (put-column rg ix iz (:chunk payload))))

(defn- column-at [^Region rg ^long ix ^long iz]
  (let [col (aget ^objects (rg-cols rg) (+ (* ix (rg-ncz rg)) iz))]
    (when (and (nil? col) (rg-oracle rg)) (summon rg ix iz))))

(defn read-block ^long [^Region rg ^long x ^long y ^long z]
  (let [ix (- (bit-shift-right x 4) (rg-cx0 rg))
        iz (- (bit-shift-right z 4) (rg-cz0 rg))
        iy (- (bit-shift-right y 4) (rg-sy0 rg))]
    (if (or (neg? ix) (>= ix (rg-ncx rg))
            (neg? iz) (>= iz (rg-ncz rg))
            (neg? iy) (>= iy (rg-nsy rg)))
      0
      (do
        (column-at rg ix iz)
        (if-let [s (aget ^objects (rg-grid rg)
                         (+ (* (+ (* ix (rg-ncz rg)) iz)
                               (rg-nsy rg)) iy))]
          (chunk/section-block s (+ (* (bit-and y 15) 256)
                                    (* (bit-and z 15) 16)
                                    (bit-and x 15)))
          0)))))

(def ^:private ^:const ray-w 21)

(defn- shell? [^long j ^long k ^long l]
  (or (= j 0) (= j 15) (= k 0) (= k 15) (= l 0) (= l 15)))

(defn- ray-dir [^long j ^long k ^long l]
  (let [d0 (- (/ (double j) 7.5) 1.0)
        d1 (- (/ (double k) 7.5) 1.0)
        d2 (- (/ (double l) 7.5) 1.0)
        d3 (Math/sqrt (+ (* d0 d0) (* d1 d1) (* d2 d2)))]
    [(/ d0 d3) (/ d1 d3) (/ d2 d3)]))

(defn- mark-hit [^booleans hit origin bx by bz]
  (let [ix (- (long bx) (long (origin 0)))
        iy (- (long by) (long (origin 1)))
        iz (- (long bz) (long (origin 2)))]
    (when (and (< -1 ix ray-w) (< -1 iy ray-w) (< -1 iz ray-w))
      (aset hit (+ (* (+ (* ix ray-w) iy) ray-w) iz) true))))

(defn- cast-ray [^Region rg ^booleans hit origin center dir f0]
  (let [[cx cy cz] center
        [d0 d1 d2] dir]
    (loop [f (double f0) x (double cx) y (double cy) z (double cz)]
      (when (pos? f)
        (let [bx (long (Math/floor x))
              by (long (Math/floor y))
              bz (long (Math/floor z))
              st (read-block rg bx by bz)
              f (if (zero? st) f (- f (* (+ (block/resist st) 0.3) 0.3)))]
          (when (and (pos? f) (pos? st))
            (mark-hit hit origin bx by bz))
          (recur (- f 0.22500001)
                 (+ x (* (double d0) 0.3)) (+ y (* (double d1) 0.3)) (+ z (* (double d2) 0.3))))))))

(defn- cast-shell [^Region rg ^booleans hit origin center power seed-h]
  (dotimes [j 16]
    (dotimes [k 16]
      (dotimes [l 16]
        (when (shell? j k l)
          (cast-ray rg hit origin center (ray-dir j k l)
                    (* (double power) (+ 0.7 (* 0.6 (random/of-longs (long seed-h) j k l))))))))))

(defn- hit-positions [^booleans hit origin]
  (let [ox (long (origin 0)) oy (long (origin 1)) oz (long (origin 2))
        out (transient [])]
    (dotimes [ix ray-w]
      (dotimes [iy ray-w]
        (dotimes [iz ray-w]
          (when (aget hit (+ (* (+ (* ix ray-w) iy) ray-w) iz))
            (conj! out [(+ ox ix) (+ oy iy) (+ oz iz)])))))
    (persistent! out)))

(defn affected-blocks [^Region rg [cx cy cz] power seed]
  (let [center [(double cx) (double cy) (double cz)]
        origin [(- (long (Math/floor (double cx))) region-r)
                (- (long (Math/floor (double cy))) region-r)
                (- (long (Math/floor (double cz))) region-r)]
        hit (boolean-array (* ray-w ray-w ray-w))]
    (cast-shell rg hit origin center (double power) (long (hash seed)))
    (hit-positions hit origin)))

(defn- path-probe [^Region rg cx cy cz]
  (let [^objects grid (rg-grid rg)
        gx (unchecked-int (rg-cx0 rg))
        gz (unchecked-int (rg-cz0 rg))
        gy (unchecked-int (rg-sy0 rg))
        nx (unchecked-int (rg-ncx rg))
        nz (unchecked-int (rg-ncz rg))
        ny (unchecked-int (rg-nsy rg))
        cx (double cx) cy (double cy) cz (double cz)]
    (fn ^long [^double x ^double y ^double z]
      (long (Rays/clearPath grid gx gz gy nx nz ny ^booleans (block/solid-arr) cx cy cz x y z)))))

(defn- density-steps [^double half ^double height]
  (let [sx (/ 1.0 (+ (* 4.0 half) 1.0))
        sy (/ 1.0 (+ (* 2.0 height) 1.0))]
    [sx sy (/ (- 1.0 (* (Math/floor (/ 1.0 sx)) sx)) 2.0)]))

(defn- density-loop [probe [px py pz] half height [sx sy ox]]
  (let [px (double px) py (double py) pz (double pz)
        half (double half) height (double height)
        sx (double sx) sy (double sy) ox (double ox)]
    (loop [fx 0.0 fy 0.0 fz 0.0 hit 0 total 0]
      (cond
        (> fx 1.0) (if (zero? total) 0.0 (/ (double hit) (double total)))
        (> fy 1.0) (recur (+ fx sx) 0.0 0.0 hit total)
        (> fz 1.0) (recur fx (+ fy sy) 0.0 hit total)
        :else (recur fx fy (+ fz sx)
                     (+ hit (long (probe (+ (- px half) (* fx 2.0 half) ox)
                                         (+ py (* fy height))
                                         (+ (- pz half) (* fz 2.0 half) ox))))
                     (inc total))))))

(defn block-density
  "Returns the share, 0.0 to 1.0, of a body at p that the blast at the
  center reaches without a block in the way. The body is a box of
  half width half and height height."
  [^Region rg [cx cy cz] p half height]
  (let [half (double half) height (double height)]
    (density-loop (path-probe rg cx cy cz) p half height (density-steps half height))))

(defn shuffled [v seed]
  (let [^objects a (to-array v)]
    (loop [i (alength a)]
      (if (> i 1)
        (let [j (long (* i (random/of-key [seed :shuffle i])))
              t (aget a (dec i))]
          (aset a (dec i) (aget a j))
          (aset a j t)
          (recur (dec i)))
        (vec a)))))

(def ^:private ^:const merge-cap 16)

(defn- add-stack [cs pos item n]
  (let [mx (data/max-stack item)
        lim (min mx merge-cap)]
    (loop [i 0 n (long n) cs cs]
      (cond
        (zero? n) cs
        (>= i (count cs)) (conj cs [pos item n])
        :else
        (let [[p it c] (nth cs i)
              c (long c)]
          (if (and (= it item) (<= (+ c n) mx))
            (let [d (max 0 (min (- lim c) n))]
              (recur (inc i) (- n d) (assoc cs i [p it (+ c d)])))
            (recur (inc i) n cs)))))))

(defn- pos-stacks [cs st pos seed radius]
  (let [roll (fn [salt] (random/of-key [seed pos salt]))]
    (reduce (fn [acc s] (add-stack acc pos (:item s) (long (:count s 1))))
            cs
            (block/drops st roll radius))))

(defn- dropping? [^Region rg [x y z]]
  (let [st (read-block rg (long x) (long y) (long z))]
    (when (and (pos? st) (not (block/tnt? st))) st)))

(defn stacks
  "Returns [pos stack] pairs of what the blast leaves behind.
  The pairs come from the destroyed positions and are merged
  into few stacks. seed decides the random drops. radius is the
  blast radius the drops depend on."
  [^Region rg positions seed radius]
  (mapv (fn [[pos item n]] [pos {:item item :count n}])
        (reduce (fn [cs pos]
                  (if-let [st (dropping? rg pos)]
                    (pos-stacks cs st pos seed radius)
                    cs))
                []
                (shuffled positions seed))))
