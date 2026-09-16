(ns collider.world.space.explosion
  "Explosions: the blocks a blast breaks, the stacks they drop, and how much of
   a body the blast reaches."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.java Chunk Rays Section)))

(set! *warn-on-reflection* true)

(def ^:private ^:const region-r 10)
(deftype Region [^objects grid ^long cx0 ^long cz0 ^long sy0
                 ^long ncx ^long ncz ^long nsy])

(defn- region-bounds [[cx cy cz]]
  (let [cx0 (bit-shift-right (- (long cx) region-r) 4)
        cx1 (bit-shift-right (+ (long cx) region-r) 4)
        cz0 (bit-shift-right (- (long cz) region-r) 4)
        cz1 (bit-shift-right (+ (long cz) region-r) 4)
        sy0 (max (bit-shift-right chunk/min-y 4) (bit-shift-right (- (long cy) region-r) 4))
        sy1 (min (bit-shift-right chunk/max-y 4) (bit-shift-right (+ (long cy) region-r) 4))]
    [cx0 cz0 sy0 (inc (- cx1 cx0)) (inc (- cz1 cz0)) (inc (- sy1 sy0))]))

(defn- section-at [^Chunk col ^long sy]
  (.section col (int (+ sy chunk/section-offset))))

(defn- fill-grid [^objects grid chunks template [cx0 cz0 sy0 ncx ncz nsy]]
  (dotimes [ix (long ncx)]
    (dotimes [iz (long ncz)]
      (let [col (Chunk/at chunks template (+ (long cx0) ix)
                          (+ (long cz0) iz))]
        (dotimes [iy (long nsy)]
          (aset grid (+ (* (+ (* ix (long ncz)) iz) (long nsy)) iy)
                (section-at col (+ (long sy0) iy))))))))

(defn block-reader
  "Returns the block states around pos in one value the blast reads from."
  ^Region [chunks template pos]
  (let [[cx0 cz0 sy0 ncx ncz nsy :as bounds] (region-bounds pos)
        grid (object-array (* (long ncx) (long ncz) (long nsy)))]
    (fill-grid grid chunks template bounds)
    (Region. grid cx0 cz0 sy0 ncx ncz nsy)))

(defn read-block
  "Returns the block state at x y z, air outside what was read."
  ^long [^Region rg ^long x ^long y ^long z]
  (let [ix (- (bit-shift-right x 4) (.cx0 rg))
        iz (- (bit-shift-right z 4) (.cz0 rg))
        iy (- (bit-shift-right y 4) (.sy0 rg))]
    (if (or (neg? ix) (>= ix (.ncx rg))
            (neg? iz) (>= iz (.ncz rg))
            (neg? iy) (>= iy (.nsy rg)))
      0
      (if-let [s (aget ^objects (.grid rg)
                       (+ (* (+ (* ix (.ncz rg)) iz) (.nsy rg)) iy))]
        (.block ^Section s (int (+ (* (bit-and y 15) 256)
                                   (* (bit-and z 15) 16)
                                   (bit-and x 15))))
        0))))

(def ^:private ^:const ray-w 21)

(defn- shell?
  "Returns true when j k l lies on the outside of the cube of ray
   directions."
  [^long j ^long k ^long l]
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

(defn affected-blocks
  "Returns the positions a blast of the given power destroys around a center.
   seed decides the ragged edge."
  [^Region rg [cx cy cz] power seed]
  (let [center [(double cx) (double cy) (double cz)]
        origin [(- (long (Math/floor (double cx))) region-r)
                (- (long (Math/floor (double cy))) region-r)
                (- (long (Math/floor (double cz))) region-r)]
        hit (boolean-array (* ray-w ray-w ray-w))]
    (cast-shell rg hit origin center (double power) (long (hash seed)))
    (hit-positions hit origin)))

(defn- path-probe
  "Returns a test of whether the blast at cx cy cz reaches a point with
   nothing in the way."
  [^Region rg cx cy cz]
  (let [^objects grid (.grid rg)
        gx (unchecked-int (.cx0 rg)) gz (unchecked-int (.cz0 rg)) gy (unchecked-int (.sy0 rg))
        nx (unchecked-int (.ncx rg)) nz (unchecked-int (.ncz rg)) ny (unchecked-int (.nsy rg))
        cx (double cx) cy (double cy) cz (double cz)]
    (fn ^long [^double x ^double y ^double z]
      (long (Rays/clearPath grid gx gz gy nx nz ny ^booleans (block/solid-arr) cx cy cz x y z)))))

(defn- density-steps [^double half ^double height]
  (let [sx (/ 1.0 (+ (* 4.0 half) 1.0))
        sy (/ 1.0 (+ (* 2.0 height) 1.0))]
    [sx sy (/ (- 1.0 (* (Math/floor (/ 1.0 sx)) sx)) 2.0)]))

(defn- density-loop
  "Returns the share, 0.0 to 1.0, of a box at p the probe reaches."
  [probe [px py pz] half height [sx sy ox]]
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
  "Returns the share, 0.0 to 1.0, of a body at p that the blast at the center
   reaches without a block in the way. The body is a box of half width half and
   height height."
  [^Region rg [cx cy cz] p half height]
  (let [half (double half) height (double height)]
    (density-loop (path-probe rg cx cy cz) p half height (density-steps half height))))

(defn shuffled
  "Returns v in an order decided by seed."
  [v seed]
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
  "Returns [pos stack] pairs of what the destroyed positions leave behind,
   gathered into few stacks. seed decides the random drops, radius is the blast
   radius they depend on."
  [^Region rg positions seed radius]
  (mapv (fn [[pos item n]] [pos {:item item :count n}])
        (reduce (fn [cs pos]
                  (if-let [st (dropping? rg pos)]
                    (pos-stacks cs st pos seed radius)
                    cs))
                []
                (shuffled positions seed))))
