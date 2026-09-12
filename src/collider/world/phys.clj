(ns collider.world.phys
  (:require [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.java Phys)
           (collider.world.chunk Section)))

(set! *warn-on-reflection* true)

(def ^:private ^:const eps 1.0E-7)
(def ^:private ^ThreadLocal sweep-buf
  (proxy [ThreadLocal] [] (initialValue [] (double-array 1536))))

(defn- full-cube? [chunks template x y z]
  (let [st (chunk/block-state chunks template x y z)]
    (and (block/solid? st) (block/full-cube? st))))

(defn standing-on-cubes? [chunks template x y z half]
  (let [x (double x) y (double y) z (double z) half (double half)]
    (and (== y (Math/floor y))
         (pos? y)
         (let [yb (dec (long y))
               x0 (long (Math/floor (- x half))) x1 (long (Math/floor (+ x half)))
               z0 (long (Math/floor (- z half))) z1 (long (Math/floor (+ z half)))]
           (and (full-cube? chunks template x0 yb z0)
                (or (= x1 x0) (full-cube? chunks template x1 yb z0))
                (or (= z1 z0) (full-cube? chunks template x0 yb z1))
                (or (and (= x1 x0) (= z1 z0)) (full-cube? chunks template x1 yb z1))
                true)))))

(defn fence-at? [chunks template x y z]
  (let [st (chunk/block-state chunks template x y z)]
    (or (block/fence? st) (= :gate (block/shape-of st)))))

(defn solid? [chunks template x y z]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (or (block/solid? (chunk/block-state chunks template x y z))
          (and (> y chunk/min-y) (fence-at? chunks template x (dec y) z)))
      (< y chunk/min-y))))

(defn- table-boxes [x y z st]
  (mapv (fn [[a b c d e f]]
          [(+ (long x) (/ (double a) 16.0)) (+ (long y) (/ (double b) 16.0)) (+ (long z) (/ (double c) 16.0))
           (+ (long x) (/ (double d) 16.0)) (+ (long y) (/ (double e) 16.0)) (+ (long z) (/ (double f) 16.0))])
        (block/collision-boxes st)))

(defn- cell-boxes [chunks template cx cy cz]
  (let [cx (long cx) cy (long cy) cz (long cz)]
    (cond
      (< cy chunk/min-y) [[(double cx) (- chunk/min-y 4.0) (double cz) (+ cx 1.0) (double chunk/min-y) (+ cz 1.0)]]
      (> cy chunk/max-y) nil
      :else
      (let [st (chunk/chunks-get-block chunks template cx cy cz)]
        (when (block/solid? st)
          (table-boxes cx cy cz st))))))

(deftype Sweep [^doubles a ^long n])
(deftype Move [pos vel ^boolean on-ground])

(defn- swept-boxes ^Sweep [chunks template ^doubles ebox vx vy vz]
  (let [vx (double vx) vy (double vy) vz (double vz)
        x1 (long (Math/floor (- (+ (aget ebox 0) (min 0.0 vx)) eps)))
        x2 (long (Math/floor (+ (+ (aget ebox 3) (max 0.0 vx)) eps)))
        y1 (max (dec chunk/min-y) (- (long (Math/floor (- (+ (aget ebox 1) (min 0.0 vy)) eps))) 1))
        y2 (long (Math/floor (+ (+ (aget ebox 4) (max 0.0 vy)) eps)))
        z1 (long (Math/floor (- (+ (aget ebox 2) (min 0.0 vz)) eps)))
        z2 (long (Math/floor (+ (+ (aget ebox 5) (max 0.0 vz)) eps)))
        cells (* (inc (- x2 x1)) (inc (- (max y1 y2) y1)) (inc (- z2 z1)))
        need (* 24 (max 1 cells))
        ^doubles a (let [^doubles b (.get sweep-buf)]
                     (if (>= (alength b) need)
                       b
                       (let [nb (double-array (* 2 need))] (.set sweep-buf nb) nb)))]
    (loop [cx x1 cz z1 cy y1 n 0 ckey -1 ^shorts blocks nil]
      (cond
        (> cx x2) (Sweep. a n)
        (> cz z2) (recur (inc cx) z1 y1 n ckey blocks)
        (> cy y2) (recur cx (inc cz) y1 n ckey blocks)
        (< cy chunk/min-y) (let [o (* n 6)]
                             (aset a o (double cx)) (aset a (+ o 1) (- chunk/min-y 4.0)) (aset a (+ o 2) (double cz))
                             (aset a (+ o 3) (+ cx 1.0)) (aset a (+ o 4) (double chunk/min-y)) (aset a (+ o 5) (+ cz 1.0))
                             (recur cx cz (inc cy) (inc n) ckey blocks))
        (> cy chunk/max-y) (recur cx cz (inc cy) n ckey blocks)
        :else
        (let [
              k (bit-or (bit-shift-left (chunk/pos->id (bit-shift-right cx 4) (bit-shift-right cz 4)) 5)
                        (chunk/section-index cy))
              ^shorts blocks (if (= k ckey)
                               blocks
                               (let [c (get chunks (chunk/pos->id (bit-shift-right cx 4) (bit-shift-right cz 4)) template)]
                                 (when-let [^Section sec (get (:sections c) (chunk/section-index cy))]
                                   (.blocks sec))))
              st (if blocks
                   (bit-and (long (aget blocks (+ (* (bit-and cy 15) 256) (* (bit-and cz 15) 16) (bit-and cx 15)))) 0xFFFF)
                   0)]
          (cond
            (not (block/solid? st))
            (recur cx cz (inc cy) n k blocks)
            (not (block/full-cube? st))
            (let [n (long (reduce (fn [^long n b]
                                    (let [o (* n 6)]
                                      (dotimes [i 6] (aset a (+ o i) (double (nth b i))))
                                      (inc n)))
                                  n (cell-boxes chunks template cx cy cz)))]
              (recur cx cz (inc cy) n k blocks))
            :else
            (let [o (* n 6)]
              (aset a o (double cx)) (aset a (+ o 1) (double cy)) (aset a (+ o 2) (double cz))
              (aset a (+ o 3) (+ cx 1.0)) (aset a (+ o 4) (+ cy 1.0)) (aset a (+ o 5) (+ cz 1.0))
              (recur cx cz (inc cy) (inc n) k blocks))))))))

(defn- shifted ^doubles [^doubles box ^long axis ^double d]
  (let [b (aclone box)]
    (aset b axis (+ (aget b axis) d))
    (aset b (+ axis 3) (+ (aget b (+ axis 3)) d))
    b))

(defn move
  ([chunks template pos vel half height]
   (move chunks template pos vel half height 0.0))
  ([chunks template pos vel half height step]
   (let [half (double half) step (double step)
         x (v/x pos) y (v/y pos) z (v/z pos)
         vx (v/x vel) vy (v/y vel) vz (v/z vel)
         box0 (double-array [(- x half) y (- z half) (+ x half) (+ y (double height)) (+ z half)])
         sw (swept-boxes chunks template box0 vx vy vz)
         out (double-array 3)
         _ (Phys/clampAxes (.a sw) (.n sw) box0 vx vy vz out)
         dx (aget out 0) dy (aget out 1) dz (aget out 2)
         hit-y? (not= dy vy)
         grounded? (and hit-y? (neg? vy))
         [dx dy dz] (if (and (pos? step) grounded?
                             (or (not= dx vx) (not= dz vz)))
                      (let [gbox (shifted box0 1 dy)
                            ^Sweep sb (swept-boxes chunks template gbox vx step vz)
                            ^doubles a (.a sb) n (.n sb)
                            ^doubles e (aclone ^doubles gbox)
                            du (Phys/clampAll a n e 0 1 step)
                            _ (do (aset e 1 (+ (aget e 1) du)) (aset e 4 (+ (aget e 4) du)))
                            sx (Phys/clampAll a n e 0 0 vx)
                            _ (do (aset e 0 (+ (aget e 0) sx)) (aset e 3 (+ (aget e 3) sx)))
                            sz (Phys/clampAll a n e 0 2 vz)
                            _ (do (aset e 2 (+ (aget e 2) sz)) (aset e 5 (+ (aget e 5) sz)))
                            dd (Phys/clampAll a n e 0 1 (- du))]
                        (if (> (+ (* sx sx) (* sz sz)) (+ (* dx dx) (* dz dz)))
                          [sx (+ dy du dd) sz]
                          [dx dy dz]))
                      [dx dy dz])
         dx (double dx) dy (double dy) dz (double dz)]
     (Move. (v/v3 (+ x dx) (+ y dy) (+ z dz))
            (v/v3 (if (= dx vx) vx 0.0)
                  (if hit-y? 0.0 vy)
                  (if (= dz vz) vz 0.0))
            grounded?))))
