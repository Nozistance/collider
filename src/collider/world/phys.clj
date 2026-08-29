(ns collider.world.phys

  (:require [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.liquid :as liquid]
            [collider.world.support :as support])
  (:import (collider.world.chunk Section)))

(set! *warn-on-reflection* true)

(def ^:private ^:const eps 1.0E-7)
(def ^:private ^ThreadLocal sweep-buf
  (proxy [ThreadLocal] [] (initialValue [] (double-array 1536))))

(def fence-ids #{85 107 113 139})
(def ^:private slab-ids #{44 126 182})
(def ^:private stair-ids #{53 67 108 109 114 128 134 135 136 156 163 164 180})
(def ^:private ^booleans fence-table
  (let [a (boolean-array 256)] (doseq [id fence-ids] (aset a (long id) true)) a))
(def ^:private ^booleans shaped-table
  (let [a (boolean-array 256)]
    (doseq [id (concat fence-ids slab-ids stair-ids [107])] (aset a (long id) true)) a))

(defn- full-cube? [chunks template x y z]
  (let [st (chunk/block-state chunks template x y z)
        id (bit-shift-right st 4)]
    (and (pos? id) (< id 256)
         (not (aget ^booleans shaped-table id))
         (not (liquid/liquid-state? st))
         (not (support/fragile-id? id)))))

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

(defn- fence-id? [chunks template x y z]
  (let [id (bit-shift-right (chunk/block-state chunks template x y z) 4)]
    (and (< id 256) (aget ^booleans fence-table id))))

(defn solid? [chunks template x y z]
  (let [y (long y)]
    (if (or (neg? y) (> y 255))
      (neg? y)
      (let [st (chunk/block-state chunks template x y z)
            id (bit-shift-right (long st) 4)]
        (or (and (pos? id)
                 (not (liquid/liquid-state? st))
                 (not (support/fragile-id? id)))
            (and (pos? y) (fence-id? chunks template x (dec y) z)))))))

(defn- connects? [chunks template x y z]
  (let [st (chunk/chunks-get-block chunks template x y z)
        id (bit-shift-right (long st) 4)]
    (and (pos? id)
         (not (liquid/liquid-state? st))
         (not (support/fragile-id? id)))))

(defn- fence-boxes [chunks template x y z]
  (let [x (long x) y (long y) z (long z)
        n? (connects? chunks template x y (dec z))
        s? (connects? chunks template x y (inc z))
        w? (connects? chunks template (dec x) y z)
        e? (connects? chunks template (inc x) y z)
        top (+ y 1.5)
        acc (if (or n? s?)
              [[(+ x 0.375) (double y) (+ z (if n? 0.0 0.375))
                (+ x 0.625) top (+ z (if s? 1.0 0.625))]]
              [])]
    (if (or w? e? (and (not n?) (not s?)))
      (conj acc [(+ x (if w? 0.0 0.375)) (double y) (+ z 0.375)
                 (+ x (if e? 1.0 0.625)) top (+ z 0.625)])
      acc)))

(defn- gate-boxes [x y z meta]
  (when (zero? (bit-and (long meta) 4))
    (let [x (long x) y (long y) z (long z) top (+ y 1.5)]
      (if (zero? (bit-and (long meta) 1))
        [[(double x) (double y) (+ z 0.375) (+ x 1.0) top (+ z 0.625)]]
        [[(+ x 0.375) (double y) (double z) (+ x 0.625) top (+ z 1.0)]]))))

(defn- slab-box [x y z meta]
  (let [top? (pos? (bit-and (long meta) 8))]
    [[(double x) (+ (long y) (if top? 0.5 0.0)) (double z)
      (+ (long x) 1.0) (+ (long y) (if top? 1.0 0.5)) (+ (long z) 1.0)]]))

(defn- stair-boxes [x y z meta]
  (let [x (long x) y (long y) z (long z)
        inv? (pos? (bit-and (long meta) 4))
        base (if inv?
               [(double x) (+ y 0.5) (double z) (+ x 1.0) (+ y 1.0) (+ z 1.0)]
               [(double x) (double y) (double z) (+ x 1.0) (+ y 0.5) (+ z 1.0)])
        [sx1 sz1 sx2 sz2] (case (int (bit-and (long meta) 3))
                            0 [(+ x 0.5) (double z) (+ x 1.0) (+ z 1.0)]
                            1 [(double x) (double z) (+ x 0.5) (+ z 1.0)]
                            2 [(double x) (+ z 0.5) (+ x 1.0) (+ z 1.0)]
                            3 [(double x) (double z) (+ x 1.0) (+ z 0.5)])
        step (if inv?
               [sx1 (double y) sz1 sx2 (+ y 0.5) sz2]
               [sx1 (+ y 0.5) sz1 sx2 (+ y 1.0) sz2])]
    [base step]))

(defn- cell-boxes [chunks template cx cy cz]
  (let [cx (long cx) cy (long cy) cz (long cz)]
    (cond
      (neg? cy)  [[(double cx) -4.0 (double cz) (+ cx 1.0) 0.0 (+ cz 1.0)]]
      (> cy 255) nil
      :else
      (let [st (chunk/chunks-get-block chunks template cx cy cz)
            id (bit-shift-right (long st) 4)
            meta (bit-and (long st) 15)]
        (cond
          (zero? id)                nil
          (liquid/liquid-state? st) nil
          (support/fragile-id? id)  nil
          (= 107 id)                (gate-boxes cx cy cz meta)
          (contains? fence-ids id)  (fence-boxes chunks template cx cy cz)
          (contains? slab-ids id)   (slab-box cx cy cz meta)
          (contains? stair-ids id)  (stair-boxes cx cy cz meta)
          :else [[(double cx) (double cy) (double cz)
                  (+ cx 1.0) (+ cy 1.0) (+ cz 1.0)]])))))

(deftype Sweep [^doubles a ^long n])
(deftype Move [pos vel ^boolean on-ground])

(defn- swept-boxes
  ^Sweep [chunks template ^doubles ebox vx vy vz]
  (let [vx (double vx) vy (double vy) vz (double vz)
        x1 (long (Math/floor (- (+ (aget ebox 0) (min 0.0 vx)) eps)))
        x2 (long (Math/floor (+ (+ (aget ebox 3) (max 0.0 vx)) eps)))
        y1 (max -1 (- (long (Math/floor (- (+ (aget ebox 1) (min 0.0 vy)) eps))) 1))
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
        (neg? cy) (let [o (* n 6)]
                    (aset a o (double cx)) (aset a (+ o 1) -4.0) (aset a (+ o 2) (double cz))
                    (aset a (+ o 3) (+ cx 1.0)) (aset a (+ o 4) 0.0) (aset a (+ o 5) (+ cz 1.0))
                    (recur cx cz (inc cy) (inc n) ckey blocks))
        (> cy 255) (recur cx cz (inc cy) n ckey blocks)
        :else
        (let [
              k (bit-or (bit-shift-left (chunk/pos->id (bit-shift-right cx 4) (bit-shift-right cz 4)) 4)
                        (bit-shift-right cy 4))
              ^shorts blocks (if (= k ckey)
                               blocks
                               (let [c (get chunks (chunk/pos->id (bit-shift-right cx 4) (bit-shift-right cz 4)) template)]
                                 (when-let [^Section sec (get (:sections c) (bit-shift-right cy 4))]
                                   (.blocks sec))))
              st (if blocks
                   (bit-and (long (aget blocks (+ (* (bit-and cy 15) 256) (* (bit-and cz 15) 16) (bit-and cx 15)))) 0xFFFF)
                   0)
              id (bit-shift-right st 4)]
          (cond
            (or (zero? id) (liquid/liquid-state? st) (support/fragile-id? id))
            (recur cx cz (inc cy) n k blocks)
            (and (< id 256) (aget ^booleans shaped-table id))
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
         sw   (swept-boxes chunks template box0 vx vy vz)
         out  (double-array 3)
         _    (collider.java.Phys/clampAxes (.a sw) (.n sw) box0 vx vy vz out)
         dx (aget out 0) dy (aget out 1) dz (aget out 2)
         hit-y? (not= dy vy)
         grounded? (and hit-y? (neg? vy))
         [dx dy dz] (if (and (pos? step) grounded?
                             (or (not= dx vx) (not= dz vz)))
                      (let [gbox (shifted box0 1 dy)
                            ^Sweep sb (swept-boxes chunks template gbox vx step vz)
                            ^doubles a (.a sb) n (.n sb)
                            ^doubles e (aclone ^doubles gbox)
                            du (collider.java.Phys/clampAll a n e 0 1 step)
                            _  (do (aset e 1 (+ (aget e 1) du)) (aset e 4 (+ (aget e 4) du)))
                            sx (collider.java.Phys/clampAll a n e 0 0 vx)
                            _  (do (aset e 0 (+ (aget e 0) sx)) (aset e 3 (+ (aget e 3) sx)))
                            sz (collider.java.Phys/clampAll a n e 0 2 vz)
                            _  (do (aset e 2 (+ (aget e 2) sz)) (aset e 5 (+ (aget e 5) sz)))
                            dd (collider.java.Phys/clampAll a n e 0 1 (- du))]
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
