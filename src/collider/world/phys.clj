(ns collider.world.phys
  "Collision of moving bodies with the blocks of the world."
  (:require [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.java Phys Section)))

(set! *warn-on-reflection* true)

(def ^:private ^:const eps 1.0E-7)

(def ^:private ^ThreadLocal sweep-buf
  (proxy [ThreadLocal] [] (initialValue [] (double-array 1536))))

(defn- full-cube? [chunks x y z]
  (let [st (chunk/block-state chunks x y z)]
    (and (block/solid? st) (block/full-cube? st))))

(defn standing-on-cubes?
  "Whether whole blocks carry a box of that half width at x y z."
  [chunks x y z half]
  (let [x (double x) y (double y) z (double z) half (double half)
        yb (dec (long y))
        x0 (long (Math/floor (- x half)))
        x1 (long (Math/floor (+ x half)))
        z0 (long (Math/floor (- z half)))
        z1 (long (Math/floor (+ z half)))]
    (and (== y (Math/floor y)) (pos? y)
         (full-cube? chunks x0 yb z0)
         (or (= x1 x0) (full-cube? chunks x1 yb z0))
         (or (= z1 z0) (full-cube? chunks x0 yb z1))
         (or (and (= x1 x0) (= z1 z0))
             (full-cube? chunks x1 yb z1)))))

(defn fence-at? [chunks x y z]
  (let [st (chunk/block-state chunks x y z)]
    (or (block/fence? st) (= :gate (block/shape-of st)))))

(defn solid?
  "Returns true when x y z stops a walking body. Everything below the
  world is solid and everything above it is not."
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
  (let [ix (bit-shift-right cx 4)
        iz (bit-shift-right cz 4)
        id (long (chunk/pos->id ix iz))]
    (bit-or (bit-shift-left id 5) (chunk/section-index cy))))

(defn- section-blocks ^Section [chunks cx cy cz]
  (chunk/section-at chunks cx cy cz))

(definline ^:private block-at [blocks cx cy cz]
  `(let [b# ~blocks]
     (if b#
       (chunk/section-block
         b# (+ (* (bit-and (long ~cy) 15) 256)
               (* (bit-and (long ~cz) 15) 16)
               (bit-and (long ~cx) 15)))
       0)))

(definline ^:private put-void! [a n cx cz]
  `(let [^{:tag ~'doubles} a# ~a
         n# (long ~n) cx# (long ~cx) cz# (long ~cz)
         o# (* n# 6)]
     (aset a# o# (double cx#))
     (aset a# (+ o# 1) (- chunk/min-y 4.0))
     (aset a# (+ o# 2) (double cz#))
     (aset a# (+ o# 3) (+ cx# 1.0))
     (aset a# (+ o# 4) (double chunk/min-y))
     (aset a# (+ o# 5) (+ cz# 1.0))
     (inc n#)))

(definline ^:private put-cube! [a n cx cy cz]
  `(let [^{:tag ~'doubles} a# ~a
         n# (long ~n) cx# (long ~cx) cy# (long ~cy)
         cz# (long ~cz) o# (* n# 6)]
     (aset a# o# (double cx#))
     (aset a# (+ o# 1) (double cy#))
     (aset a# (+ o# 2) (double cz#))
     (aset a# (+ o# 3) (+ cx# 1.0))
     (aset a# (+ o# 4) (+ cy# 1.0))
     (aset a# (+ o# 5) (+ cz# 1.0))
     (inc n#)))

(defmacro ^:private scaled [base b i]
  `(+ (double ~base) (/ (double (nth ~b ~i)) 16.0)))

(definline ^:private put-shape! [a n st cx cy cz]
  `(let [^{:tag ~'doubles} a# ~a
         cx# (long ~cx) cy# (long ~cy) cz# (long ~cz)]
     (long (reduce (fn [^long n# b#]
                     (let [o# (* n# 6)]
                       (aset a# o# (scaled cx# b# 0))
                       (aset a# (+ o# 1) (scaled cy# b# 1))
                       (aset a# (+ o# 2) (scaled cz# b# 2))
                       (aset a# (+ o# 3) (scaled cx# b# 3))
                       (aset a# (+ o# 4) (scaled cy# b# 4))
                       (aset a# (+ o# 5) (scaled cz# b# 5))
                       (inc n#)))
                   (long ~n)
                   (block/collision-boxes (long ~st))))))

(defn- swept-boxes ^Sweep [chunks ^doubles ebox vx vy vz]
  (let [vx (double vx) vy (double vy) vz (double vz)
        x1 (lo-bound (aget ebox 0) vx)
        x2 (hi-bound (aget ebox 3) vx)
        y1 (max (dec chunk/min-y)
                (- (lo-bound (aget ebox 1) vy) 1))
        y2 (hi-bound (aget ebox 4) vy)
        z1 (lo-bound (aget ebox 2) vz)
        z2 (hi-bound (aget ebox 5) vz)
        cells (* (inc (- x2 x1))
                 (inc (- (max y1 y2) y1))
                 (inc (- z2 z1)))
        ^doubles a (sweep-buffer (* 96 (max 1 cells)))]
    (loop [cx x1 cz z1 cy y1 n 0 ckey -1 blocks nil]
      (cond
        (> cx x2) (Sweep. a n)
        (> cz z2) (recur (inc cx) z1 y1 n ckey blocks)
        (> cy y2) (recur cx (inc cz) y1 n ckey blocks)
        (< cy chunk/min-y)
        (recur cx cz (inc cy) (long (put-void! a n cx cz))
               ckey blocks)
        (> cy chunk/max-y) (recur cx cz (inc cy) n ckey blocks)
        :else
        (let [k (section-key cx cy cz)
              blocks (if (= k ckey)
                       blocks
                       (section-blocks chunks cx cy cz))
              st (long (block-at blocks cx cy cz))]
          (cond
            (not (block/solid? st))
            (recur cx cz (inc cy) n k blocks)
            (not (block/full-cube? st))
            (recur cx cz (inc cy)
                   (long (put-shape! a n st cx cy cz)) k blocks)
            :else
            (recur cx cz (inc cy)
                   (long (put-cube! a n cx cy cz)) k blocks)))))))

(def ^:private ^:const equal-slack (double (float 1.0E-5)))

(defn- mth-equal?
  "Whether the two speeds are close enough that a body counts as
  stopped by a block on that axis."
  [^double a ^double b]
  (< (Math/abs (- b a)) equal-slack))

(defn- overlaps? [^doubles a ^long o ^doubles box]
  (and (> (aget a (+ o 3)) (aget box 0))
       (> (aget box 3) (aget a o))
       (> (aget a (+ o 4)) (aget box 1))
       (> (aget box 4) (aget a (+ o 1)))
       (> (aget a (+ o 5)) (aget box 2))
       (> (aget box 5) (aget a (+ o 2)))))

(defn free?
  "Whether a body of that size meets no block when moved by dx dy
  dz, tested where it lands and not on the way there."
  [chunks pos half height dx dy dz]
  (let [half (double half)
        x (+ (v/x pos) (double dx)) y (+ (v/y pos) (double dy))
        z (+ (v/z pos) (double dz))
        corners [(- x half) y (- z half)
                 (+ x half) (+ y (double height)) (+ z half)]
        box (double-array corners)
        ^Sweep sw (swept-boxes chunks box 0.0 0.0 0.0)
        ^doubles a (.a sw)]
    (loop [i 0]
      (cond (>= i (.n sw)) true
            (overlaps? a (* 6 i) box) false
            :else (recur (inc i))))))

(definline ^:private to-center-sq [bx by bz x y z]
  `(let [dx# (- (+ (long ~bx) 0.5) (double ~x))
         dy# (- (+ (long ~by) 0.5) (double ~y))
         dz# (- (+ (long ~bz) 0.5) (double ~z))]
     (+ (* dx# dx#) (* dy# dy#) (* dz# dz#))))

(defn- later-pos?
  "Whether the block at bx by bz comes after the best one so far,
  by y, then z, then x."
  [^long bx ^long by ^long bz best]
  (or (nil? best)
      (let [[ox oy oz] best]
        (cond (not= oy by) (< (long oy) by)
              (not= oz bz) (< (long oz) bz)
              :else (< (long ox) bx)))))

(defn supporting-block
  "Returns the block a box of that half width standing at pos rests
  on, nil when it rests on nothing. The nearest block centre wins,
  so an edge of the box carries the whole body."
  [chunks pos half]
  (let [x (v/x pos) y (v/y pos) z (v/z pos) half (double half)
        corners [(- x half) (- y 1.0E-6) (- z half)
                 (+ x half) y (+ z half)]
        box (double-array corners)
        ^Sweep sw (swept-boxes chunks box 0.0 0.0 0.0)
        ^doubles a (.a sw)]
    (loop [i 0 best nil bd Double/MAX_VALUE]
      (if (>= i (.n sw))
        best
        (let [o (* 6 i)
              bx (long (Math/floor (aget a o)))
              by (long (Math/floor (aget a (+ o 1))))
              bz (long (Math/floor (aget a (+ o 2))))
              d (to-center-sq bx by bz x y z)]
          (if (and (overlaps? a o box)
                   (or (< d bd)
                       (and (== d bd) (later-pos? bx by bz best))))
            (recur (inc i) [bx by bz] d)
            (recur (inc i) best bd)))))))

(defn- shifted ^doubles [^doubles box ^long axis ^double d]
  (let [b (aclone box)]
    (aset b axis (+ (aget b axis) d))
    (aset b (+ axis 3) (+ (aget b (+ axis 3)) d))
    b))

(defn- shift-box! [^doubles e ^double dx ^double dy ^double dz]
  (aset e 0 (+ (aget e 0) dx))
  (aset e 3 (+ (aget e 3) dx))
  (aset e 1 (+ (aget e 1) dy))
  (aset e 4 (+ (aget e 4) dy))
  (aset e 2 (+ (aget e 2) dz))
  (aset e 5 (+ (aget e 5) dz)))

(defn- step-up!
  "Puts into out the motion of a body that climbs the block in its
  way, when the climb carries it further sideways than the motion
  already there."
  [chunks ^doubles box0 ^doubles out vel step]
  (let [step (double step) vx (v/x vel) vz (v/z vel)
        dy0 (aget out 1)
        ^doubles e (shifted box0 1 dy0)
        ^Sweep sb (swept-boxes chunks e vx step vz)
        ^doubles a (.a sb) n (.n sb)
        ^doubles s (double-array 3)
        _ (Phys/clampAxes a n e vx step vz s)
        sx (aget s 0) du (aget s 1) sz (aget s 2)
        _ (shift-box! e sx du sz)
        dd (Phys/clampAll a n e 0 1 (- du))
        ox (aget out 0) oz (aget out 2)]
    (when (> (+ (* sx sx) (* sz sz)) (+ (* ox ox) (* oz oz)))
      (aset out 0 sx)
      (aset out 1 (+ dy0 du dd))
      (aset out 2 sz))))

(defn move
  "Returns the position, velocity and ground flag of a body.
  The body moves by vel from pos and the blocks it meets stop
  it. The body is a box of half width half and height height.
  step is how high it climbs without jumping."
  ([chunks pos vel half height]
   (move chunks pos vel half height 0.0))
  ([chunks pos vel half height step]
   (let [half (double half) step (double step)
         x (v/x pos) y (v/y pos) z (v/z pos)
         vx (v/x vel) vy (v/y vel) vz (v/z vel)
         corners [(- x half) y (- z half)
                  (+ x half) (+ y (double height)) (+ z half)]
         box0 (double-array corners)
         sw (swept-boxes chunks box0 vx vy vz)
         out (double-array 3)
         _ (Phys/clampAxes (.a sw) (.n sw) box0 vx vy vz out)
         dy0 (aget out 1)
         hit-y? (not= dy0 vy)
         grounded? (and hit-y? (neg? vy))
         aside? (or (not= (aget out 0) vx)
                    (not= (aget out 2) vz))
         _ (when (and (pos? step) grounded? aside?)
             (step-up! chunks box0 out vel step))
         dx (aget out 0) dy (aget out 1) dz (aget out 2)]
     (Move. (v/v3 (+ x dx) (+ y dy) (+ z dz))
            (v/v3 (if (mth-equal? dx vx) vx 0.0)
                  (if hit-y? 0.0 vy)
                  (if (mth-equal? dz vz) vz 0.0))
            grounded?))))
