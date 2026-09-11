(ns collider.world.explosion
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.java Rays)
           (collider.world.chunk Section)))

(set! *warn-on-reflection* true)

(def ^:private solid-arr block/solid-arr)
(def ^:private ^:const region-r 10)
(deftype Region [^objects grid ^long cx0 ^long cz0 ^long sy0
                 ^long ncx ^long ncz ^long nsy])

(defn block-reader ^Region [chunks template [cx cy cz]]
  (let [cx0 (bit-shift-right (- (long cx) region-r) 4)
        cx1 (bit-shift-right (+ (long cx) region-r) 4)
        cz0 (bit-shift-right (- (long cz) region-r) 4)
        cz1 (bit-shift-right (+ (long cz) region-r) 4)
        sy0 (max (bit-shift-right chunk/min-y 4) (bit-shift-right (- (long cy) region-r) 4))
        sy1 (min (bit-shift-right chunk/max-y 4) (bit-shift-right (+ (long cy) region-r) 4))
        ncx (inc (- cx1 cx0))
        ncz (inc (- cz1 cz0))
        nsy (inc (- sy1 sy0))
        grid (object-array (* ncx ncz nsy))]
    (dotimes [ix ncx]
      (dotimes [iz ncz]
        (let [col (get chunks (chunk/pos->id (+ cx0 ix) (+ cz0 iz)) template)]
          (dotimes [iy nsy]
            (when-let [^Section s (get-in col [:sections (+ sy0 iy chunk/section-offset)])]
              (aset grid (+ (* (+ (* ix ncz) iz) nsy) iy) (.blocks s)))))))
    (Region. grid cx0 cz0 sy0 ncx ncz nsy)))

(defn read-block ^long [^Region rg ^long x ^long y ^long z]
  (let [ix (- (bit-shift-right x 4) (.cx0 rg))
        iz (- (bit-shift-right z 4) (.cz0 rg))
        iy (- (bit-shift-right y 4) (.sy0 rg))]
    (if (or (neg? ix) (>= ix (.ncx rg))
            (neg? iz) (>= iz (.ncz rg))
            (neg? iy) (>= iy (.nsy rg)))
      0
      (if-let [blocks (aget ^objects (.grid rg) (+ (* (+ (* ix (.ncz rg)) iz) (.nsy rg)) iy))]
        (bit-and (aget ^shorts blocks (+ (* (bit-and y 15) 256)
                                         (* (bit-and z 15) 16)
                                         (bit-and x 15)))
                 0xFFFF)
        0))))

(defn affected-blocks [^Region rg [cx cy cz] power seed]
  (let [cx (double cx) cy (double cy) cz (double cz)
        power (double power)
        seed-h (long (hash seed))
        R (long 10) W (inc (* 2 R))
        ox (- (long (Math/floor cx)) R) oy (- (long (Math/floor cy)) R) oz (- (long (Math/floor cz)) R)
        ^booleans hit (boolean-array (* W W W))]
    (dotimes [j 16]
      (dotimes [k 16]
        (dotimes [l 16]
          (when (or (= j 0) (= j 15) (= k 0) (= k 15) (= l 0) (= l 15))
            (let [d0 (- (/ (double j) 7.5) 1.0)
                  d1 (- (/ (double k) 7.5) 1.0)
                  d2 (- (/ (double l) 7.5) 1.0)
                  d3 (Math/sqrt (+ (* d0 d0) (* d1 d1) (* d2 d2)))
                  d0 (/ d0 d3) d1 (/ d1 d3) d2 (/ d2 d3)]
              (loop [f (* power (+ 0.7 (* 0.6 (random/of-longs seed-h j k l))))
                     x cx y cy z cz]
                (when (pos? f)
                  (let [bx (long (Math/floor x))
                        by (long (Math/floor y))
                        bz (long (Math/floor z))
                        st (read-block rg bx by bz)
                        f  (if (zero? st)
                             f
                             (- f (* (+ (block/resist st) 0.3) 0.3)))]
                    (when (and (pos? f) (pos? st))
                      (let [ix (- bx ox) iy (- by oy) iz (- bz oz)]
                        (when (and (< -1 ix W) (< -1 iy W) (< -1 iz W))
                          (aset hit (+ (* (+ (* ix W) iy) W) iz) true))))
                    (recur (- f 0.22500001)
                           (+ x (* d0 0.3)) (+ y (* d1 0.3)) (+ z (* d2 0.3)))))))))))
    (persistent!
     (let [out (transient [])]
       (dotimes [ix W]
         (dotimes [iy W]
           (dotimes [iz W]
             (when (aget hit (+ (* (+ (* ix W) iy) W) iz))
               (conj! out [(+ ox ix) (+ oy iy) (+ oz iz)])))))
       out))))

(defn block-density [^Region rg [cx cy cz] [px py pz] half height]
  (let [cx (double cx) cy (double cy) cz (double cz)
        px (double px) py (double py) pz (double pz)
        ^objects grid (.grid rg)
        gx (unchecked-int (.cx0 rg)) gz (unchecked-int (.cz0 rg)) gy (unchecked-int (.sy0 rg))
        nx (unchecked-int (.ncx rg)) nz (unchecked-int (.ncz rg)) ny (unchecked-int (.nsy rg))
        half (double half) height (double height)
        sx (/ 1.0 (+ (* 4.0 half) 1.0))
        sy (/ 1.0 (+ (* 2.0 height) 1.0))
        ox (/ (- 1.0 (* (Math/floor (/ 1.0 sx)) sx)) 2.0)]
    (loop [fx 0.0 fy 0.0 fz 0.0 hit 0 total 0]
      (cond
        (> fx 1.0) (if (zero? total) 0.0 (/ (double hit) (double total)))
        (> fy 1.0) (recur (+ fx sx) 0.0 0.0 hit total)
        (> fz 1.0) (recur fx (+ fy sy) 0.0 hit total)
        :else (recur fx fy (+ fz sx)
                     (+ hit (Rays/clearPath grid gx gz gy nx nz ny ^booleans solid-arr
                                            cx cy cz
                                            (+ (- px half) (* fx 2.0 half) ox)
                                            (+ py (* fy height))
                                            (+ (- pz half) (* fz 2.0 half) ox)))
                     (inc total))))))
