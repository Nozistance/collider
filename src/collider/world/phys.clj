(ns collider.world.phys
  "Collision of moving bodies with the blocks of the world."
  (:require [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.java Chunk Phys Section)))

(set! *warn-on-reflection* true)

(def ^:private ^:const eps 1.0E-7)
(def ^:private ^ThreadLocal sweep-buf
  (proxy [ThreadLocal] [] (initialValue [] (double-array 1536))))

(defn- full-cube? [chunks x y z]
  (let [st (chunk/block-state chunks x y z)]
    (and (block/solid? st) (block/full-cube? st))))

(defn standing-on-cubes? [chunks x y z half]
  (let [x (double x) y (double y) z (double z) half (double half)]
    (and (== y (Math/floor y))
         (pos? y)
         (let [yb (dec (long y))
               x0 (long (Math/floor (- x half))) x1 (long (Math/floor (+ x half)))
               z0 (long (Math/floor (- z half))) z1 (long (Math/floor (+ z half)))]
           (and (full-cube? chunks x0 yb z0)
                (or (= x1 x0) (full-cube? chunks x1 yb z0))
                (or (= z1 z0) (full-cube? chunks x0 yb z1))
                (or (and (= x1 x0) (= z1 z0)) (full-cube? chunks x1 yb z1))
                true)))))

(defn fence-at? [chunks x y z]
  (let [st (chunk/block-state chunks x y z)]
    (or (block/fence? st) (= :gate (block/shape-of st)))))

(defn solid?
  "Returns true when x y z stops a walking body. Everything below the world is
   solid and everything above it is not."
  [chunks x y z]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (or (block/solid? (chunk/block-state chunks x y z))
          (and (> y chunk/min-y) (fence-at? chunks x (dec y) z)))
      (< y chunk/min-y))))

(deftype Sweep [^doubles a ^long n])
(deftype Move [pos vel ^boolean on-ground])

(defn- lo-bound ^long [^double c ^double v]
  (long (Math/floor (- (+ c (min 0.0 v)) eps))))

(defn- hi-bound ^long [^double c ^double v]
  (long (Math/floor (+ (+ c (max 0.0 v)) eps))))

(defn- sweep-buffer ^doubles [^long need]
  (let [^doubles b (.get sweep-buf)]
    (if (>= (alength b) need)
      b
      (let [nb (double-array (* 2 need))] (.set sweep-buf nb) nb))))

(defn- section-key ^long [^long cx ^long cy ^long cz]
  (bit-or (bit-shift-left (chunk/pos->id (bit-shift-right cx 4) (bit-shift-right cz 4)) 5)
          (chunk/section-index cy)))

(defn- section-blocks ^Section [chunks cx cy cz]
  (Chunk/sectionAt chunks (unchecked-int cx)
                   (unchecked-int cy) (unchecked-int cz)))

(definline ^:private block-at [blocks cx cy cz]
  `(let [^collider.java.Section b# ~blocks]
     (if b#
       (long (.block b# (int (+ (* (bit-and (long ~cy) 15) 256)
                                (* (bit-and (long ~cz) 15) 16)
                                (bit-and (long ~cx) 15)))))
       0)))

(definline ^:private put-void! [a n cx cz]
  `(let [^{:tag ~'doubles} a# ~a n# (long ~n) cx# (long ~cx) cz# (long ~cz) o# (* n# 6)]
     (aset a# o# (double cx#)) (aset a# (+ o# 1) (- chunk/min-y 4.0)) (aset a# (+ o# 2) (double cz#))
     (aset a# (+ o# 3) (+ cx# 1.0)) (aset a# (+ o# 4) (double chunk/min-y)) (aset a# (+ o# 5) (+ cz# 1.0))
     (inc n#)))

(definline ^:private put-cube! [a n cx cy cz]
  `(let [^{:tag ~'doubles} a# ~a n# (long ~n) cx# (long ~cx) cy# (long ~cy) cz# (long ~cz) o# (* n# 6)]
     (aset a# o# (double cx#)) (aset a# (+ o# 1) (double cy#)) (aset a# (+ o# 2) (double cz#))
     (aset a# (+ o# 3) (+ cx# 1.0)) (aset a# (+ o# 4) (+ cy# 1.0)) (aset a# (+ o# 5) (+ cz# 1.0))
     (inc n#)))

(definline ^:private put-shape! [a n st cx cy cz]
  `(let [^{:tag ~'doubles} a# ~a cx# (long ~cx) cy# (long ~cy) cz# (long ~cz)]
     (long (reduce (fn [^long n# b#]
                     (let [o# (* n# 6)]
                       (aset a# o# (+ cx# (/ (double (nth b# 0)) 16.0)))
                       (aset a# (+ o# 1) (+ cy# (/ (double (nth b# 1)) 16.0)))
                       (aset a# (+ o# 2) (+ cz# (/ (double (nth b# 2)) 16.0)))
                       (aset a# (+ o# 3) (+ cx# (/ (double (nth b# 3)) 16.0)))
                       (aset a# (+ o# 4) (+ cy# (/ (double (nth b# 4)) 16.0)))
                       (aset a# (+ o# 5) (+ cz# (/ (double (nth b# 5)) 16.0)))
                       (inc n#)))
                   (long ~n) (block/collision-boxes (long ~st))))))

(defn- swept-boxes ^Sweep [chunks ^doubles ebox vx vy vz]
  (let [vx (double vx) vy (double vy) vz (double vz)
        x1 (lo-bound (aget ebox 0) vx) x2 (hi-bound (aget ebox 3) vx)
        y1 (max (dec chunk/min-y) (- (lo-bound (aget ebox 1) vy) 1)) y2 (hi-bound (aget ebox 4) vy)
        z1 (lo-bound (aget ebox 2) vz) z2 (hi-bound (aget ebox 5) vz)
        cells (* (inc (- x2 x1)) (inc (- (max y1 y2) y1)) (inc (- z2 z1)))
        ^doubles a (sweep-buffer (* 96 (max 1 cells)))]
    (loop [cx x1 cz z1 cy y1 n 0 ckey -1 blocks nil]
      (cond
        (> cx x2) (Sweep. a n)
        (> cz z2) (recur (inc cx) z1 y1 n ckey blocks)
        (> cy y2) (recur cx (inc cz) y1 n ckey blocks)
        (< cy chunk/min-y) (recur cx cz (inc cy) (long (put-void! a n cx cz)) ckey blocks)
        (> cy chunk/max-y) (recur cx cz (inc cy) n ckey blocks)
        :else
        (let [k (section-key cx cy cz)
              blocks (if (= k ckey) blocks (section-blocks chunks cx cy cz))
              st (long (block-at blocks cx cy cz))]
          (cond
            (not (block/solid? st)) (recur cx cz (inc cy) n k blocks)
            (not (block/full-cube? st)) (recur cx cz (inc cy) (long (put-shape! a n st cx cy cz)) k blocks)
            :else (recur cx cz (inc cy) (long (put-cube! a n cx cy cz)) k blocks)))))))

(defn- shifted ^doubles [^doubles box ^long axis ^double d]
  (let [b (aclone box)]
    (aset b axis (+ (aget b axis) d))
    (aset b (+ axis 3) (+ (aget b (+ axis 3)) d))
    b))

(defn move
  "Returns the position, velocity and ground flag of a body moved by vel from
   pos, stopped by the blocks it meets. The body is a box of half width half and
   height height. step is how high it climbs without jumping."
  ([chunks pos vel half height]
   (move chunks pos vel half height 0.0))
  ([chunks pos vel half height step]
   (let [half (double half) step (double step)
         x (v/x pos) y (v/y pos) z (v/z pos)
         vx (v/x vel) vy (v/y vel) vz (v/z vel)
         box0 (double-array [(- x half) y (- z half) (+ x half) (+ y (double height)) (+ z half)])
         sw (swept-boxes chunks box0 vx vy vz)
         out (double-array 3)
         _ (Phys/clampAxes (.a sw) (.n sw) box0 vx vy vz out)
         dy0 (aget out 1)
         hit-y? (not= dy0 vy)
         grounded? (and hit-y? (neg? vy))
         _ (when (and (pos? step) grounded?
                      (or (not= (aget out 0) vx) (not= (aget out 2) vz)))
             (let [^doubles e (shifted box0 1 dy0)
                   ^Sweep sb (swept-boxes chunks e vx step vz)
                   ^doubles a (.a sb)
                   n (.n sb)
                   du (Phys/clampAll a n e 0 1 step)
                   _ (do (aset e 1 (+ (aget e 1) du)) (aset e 4 (+ (aget e 4) du)))
                   sx (Phys/clampAll a n e 0 0 vx)
                   _ (do (aset e 0 (+ (aget e 0) sx)) (aset e 3 (+ (aget e 3) sx)))
                   sz (Phys/clampAll a n e 0 2 vz)
                   _ (do (aset e 2 (+ (aget e 2) sz)) (aset e 5 (+ (aget e 5) sz)))
                   dd (Phys/clampAll a n e 0 1 (- du))]
               (when (> (+ (* sx sx) (* sz sz))
                        (+ (* (aget out 0) (aget out 0)) (* (aget out 2) (aget out 2))))
                 (aset out 0 sx)
                 (aset out 1 (+ dy0 du dd))
                 (aset out 2 sz))))
         dx (aget out 0) dy (aget out 1) dz (aget out 2)]
     (Move. (v/v3 (+ x dx) (+ y dy) (+ z dz))
            (v/v3 (if (= dx vx) vx 0.0)
                  (if hit-y? 0.0 vy)
                  (if (= dz vz) vz 0.0))
            grounded?))))
