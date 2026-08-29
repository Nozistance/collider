(ns collider.world.explosion

  (:require [collider.rnd :as rnd]
            [collider.world.chunk :as chunk]
            [collider.world.liquid :as liquid]
            [collider.world.support :as support])
  (:import (clojure.lang Murmur3)
           (collider.java Rays)
           (collider.world.chunk Section)))

(set! *warn-on-reflection* true)

(def ^:private resistance
  {1 6.0, 2 0.6, 3 0.5, 4 6.0, 5 3.0, 7 3.6E7
   8 100.0, 9 100.0, 10 100.0, 11 100.0
   12 0.5, 13 0.6, 17 2.0, 18 0.2, 20 0.3, 24 0.8, 35 0.8
   43 6.0, 44 6.0, 45 6.0, 46 0.0, 48 6.0, 49 1200.0, 50 0.0, 51 0.0
   79 0.5, 80 0.1, 85 3.0, 98 6.0, 109 6.0, 121 9.0, 155 0.8, 162 2.0})

(def ^:private ^doubles resist-arr
  (let [a (double-array 256)] (java.util.Arrays/fill a 3.0)
    (doseq [[id r] resistance] (aset a (long id) (double r))) a))

(defn- resist ^double [^long id] (if (< -1 id 256) (aget ^doubles resist-arr id) 3.0))
(def ^:private ^booleans solid-arr
  (let [a (boolean-array 256)]
    (dotimes [id 256]
      (aset a id (boolean (and (pos? id)
                               (not (liquid/liquid-state? (bit-shift-left id 4)))
                               (not (support/fragile-id? id))))))
    a))

(def ^:private ^:const region-r 10)
(deftype Region [^objects grid ^long cx0 ^long cz0 ^long sy0
                 ^long ncx ^long ncz ^long nsy])

(defn block-reader
  ^Region [chunks template [cx cy cz]]
  (let [cx0 (bit-shift-right (- (long cx) region-r) 4)
        cx1 (bit-shift-right (+ (long cx) region-r) 4)
        cz0 (bit-shift-right (- (long cz) region-r) 4)
        cz1 (bit-shift-right (+ (long cz) region-r) 4)
        sy0 (max 0 (bit-shift-right (- (long cy) region-r) 4))
        sy1 (min 15 (bit-shift-right (+ (long cy) region-r) 4))
        ncx (inc (- cx1 cx0))
        ncz (inc (- cz1 cz0))
        nsy (inc (- sy1 sy0))
        grid (object-array (* ncx ncz nsy))]
    (dotimes [ix ncx]
      (dotimes [iz ncz]
        (let [col (get chunks (chunk/pos->id (+ cx0 ix) (+ cz0 iz)) template)]
          (dotimes [iy nsy]
            (when-let [^Section s (get-in col [:sections (+ sy0 iy)])]
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
              (loop [f (* power (+ 0.7 (* 0.6 (rnd/rnd-v4 seed-h j k l))))
                     x cx y cy z cz]
                (when (pos? f)
                  (let [bx (long (Math/floor x))
                        by (long (Math/floor y))
                        bz (long (Math/floor z))
                        st (read-block rg bx by bz)
                        f  (if (zero? st)
                             f
                             (- f (* (+ (resist (bit-shift-right st 4)) 0.3) 0.3)))]
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

(defn- solid-state? [^long st]
  (let [id (bit-shift-right st 4)]
    (and (< id 256) (aget ^booleans solid-arr id))))

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
