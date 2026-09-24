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

(defn fence-at?
  "Returns true when a fence or a fence gate stands at x y z."
  [chunks x y z]
  (let [st (chunk/block-state chunks x y z)]
    (or (block/fence? st) (= :gate (block/shape-of st)))))

(defn solid?
  "Returns true when x y z stops a walking body. Nothing outside
  the world height does."
  [chunks x y z]
  (let [y (long y)]
    (and (chunk/in-range? y)
         (or (block/solid? (chunk/block-state chunks x y z))
             (and (> y chunk/min-y)
                  (fence-at? chunks x (dec y) z))))))

(deftype Sweep [^doubles a ^long n])

(defn- boxes ^doubles [^Sweep s] (.a s))

(defn- box-count ^long [^Sweep s] (.n s))

(deftype Move [pos vel ^boolean on-ground])

(defn pos
  "Returns the position a move ends at."
  [^Move m] (.pos m))

(defn vel
  "Returns the velocity a move leaves the body with."
  [^Move m] (.vel m))

(defn on-ground?
  "Returns true when a move ends on the ground."
  {:inline (fn [m]
             `(.on-ground
                ~(with-meta m {:tag 'collider.world.phys.Move})))}
  [^Move m]
  (.on-ground m))

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

(definline ^:private put-block! [a n st cx cy cz]
  `(let [a# ~a n# (long ~n) st# (long ~st)
         cx# (long ~cx) cy# (long ~cy) cz# (long ~cz)]
     (cond
       (not (block/solid? st#)) n#
       (not (block/full-cube? st#)) (put-shape! a# n# st# cx# cy# cz#)
       :else (put-cube! a# n# cx# cy# cz#))))

(definline ^:private room-for [x1 x2 y1 y2 z1 z2]
  `(let [cells# (* (inc (- ~x2 ~x1)) (inc (- (max ~y1 ~y2) ~y1))
                   (inc (- ~z2 ~z1)))]
     (sweep-buffer (* 96 (max 1 cells#)))))

(definline ^:private section-for [chunks k ckey blocks cx cy cz]
  `(if (= ~k ~ckey) ~blocks (section-blocks ~chunks ~cx ~cy ~cz)))

(definline ^:private fill-boxes [chunks a x1 x2 y1 y2 z1 z2]
  `(let [ch# ~chunks a# ~a x1# (long ~x1) x2# (long ~x2)
         y1# (long ~y1) y2# (long ~y2) z1# (long ~z1) z2# (long ~z2)]
     (loop [cx# x1# cz# z1# cy# y1# n# 0 ckey# -1 blocks# nil]
       (cond
         (> cx# x2#) n#
         (> cz# z2#) (recur (inc cx#) z1# y1# n# ckey# blocks#)
         (> cy# y2#) (recur cx# (inc cz#) y1# n# ckey# blocks#)
         :else
         (let [k# (section-key cx# cy# cz#)
               b# (section-for ch# k# ckey# blocks# cx# cy# cz#)
               st# (block-at b# cx# cy# cz#)
               n2# (long (put-block! a# n# st# cx# cy# cz#))]
           (recur cx# cz# (inc cy#) n2# k# b#))))))

(defn- swept-boxes ^Sweep [chunks ^doubles ebox vx vy vz]
  (let [vx (double vx) vy (double vy) vz (double vz)
        x1 (lo-bound (aget ebox 0) vx) x2 (hi-bound (aget ebox 3) vx)
        y1 (max chunk/min-y (- (lo-bound (aget ebox 1) vy) 1))
        y2 (min chunk/max-y (hi-bound (aget ebox 4) vy))
        z1 (lo-bound (aget ebox 2) vz) z2 (hi-bound (aget ebox 5) vz)
        ^doubles a (room-for x1 x2 y1 y2 z1 z2)]
    (Sweep. a (fill-boxes chunks a x1 x2 y1 y2 z1 z2))))

(def ^:private ^:const equal-slack (double (float 1.0E-5)))

(defn- mth-equal?
  "Whether the two speeds are close enough that a body counts as
  stopped by a block on that axis."
  [^double a ^double b]
  (< (Math/abs (- b a)) equal-slack))

(defn- restituted
  "The speed a stopped axis is left with: the reversed speed
  scaled by a bounciness of zero, which keeps its sign bit."
  ^double [^double v]
  (* (- v) 0.0))

(defn- overlaps? [^doubles a ^long o ^doubles box]
  (and (> (aget a (+ o 3)) (aget box 0))
       (> (aget box 3) (aget a o))
       (> (aget a (+ o 4)) (aget box 1))
       (> (aget box 4) (aget a (+ o 1)))
       (> (aget a (+ o 5)) (aget box 2))
       (> (aget box 5) (aget a (+ o 2)))))

(defn- meets-any? [^Sweep sw ^doubles box]
  (let [a (boxes sw) n (box-count sw)]
    (loop [i 0]
      (cond (>= i n) false
            (overlaps? a (* 6 i) box) true
            :else (recur (inc i))))))

(defn free?
  "Whether a body of that size meets no block when moved by dx dy
  dz, tested where it lands and not on the way there."
  [chunks pos half height dx dy dz]
  (let [half (double half)
        x (+ (v/x pos) (double dx)) y (+ (v/y pos) (double dy))
        z (+ (v/z pos) (double dz))
        corners [(- x half) y (- z half)
                 (+ x half) (+ y (double height)) (+ z half)]
        box (double-array corners)]
    (not (meets-any? (swept-boxes chunks box 0.0 0.0 0.0) box))))

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

(definline ^:private cell [a i k]
  `(let [^{:tag ~'doubles} a# ~a]
     (long (Math/floor (aget a# (+ (* 6 (long ~i)) ~k))))))

(definline ^:private closer? [d bd bx by bz best]
  `(let [d# (double ~d) bd# (double ~bd)]
     (or (< d# bd#)
         (and (== d# bd#) (later-pos? ~bx ~by ~bz ~best)))))

(defn- nearest-cube
  "Returns the cell of the box among the n in a that overlaps box
  and has its centre nearest to pos."
  [^doubles a ^long n ^doubles box pos]
  (let [x (v/x pos) y (v/y pos) z (v/z pos)]
    (loop [i 0 best nil bd Double/MAX_VALUE]
      (if (>= i n)
        best
        (let [bx (cell a i 0) by (cell a i 1) bz (cell a i 2)
              d (to-center-sq bx by bz x y z)]
          (if (and (overlaps? a (* 6 i) box)
                   (closer? d bd bx by bz best))
            (recur (inc i) [bx by bz] d)
            (recur (inc i) best bd)))))))

(defn supporting-block
  "Returns the block a box of that half width standing at pos rests
  on, nil when it rests on nothing. The nearest block centre wins,
  so an edge of the box carries the whole body."
  [chunks pos half]
  (let [x (v/x pos) y (v/y pos) z (v/z pos) half (double half)
        corners [(- x half) (- y 1.0E-6) (- z half)
                 (+ x half) y (+ z half)]
        box (double-array corners)
        ^Sweep sw (swept-boxes chunks box 0.0 0.0 0.0)]
    (nearest-cube (boxes sw) (box-count sw) box pos)))

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

(defn- climb
  "Moves box e up by step and then by vel sideways, and back down
  onto what it meets. Returns the sideways and up motion and the
  way back down."
  ^doubles [chunks ^doubles e vel step]
  (let [step (double step) vx (v/x vel) vz (v/z vel)
        ^Sweep sb (swept-boxes chunks e vx step vz)
        a (boxes sb) n (box-count sb)
        s (double-array 4)]
    (Phys/clampAxes a n e vx step vz s)
    (shift-box! e (aget s 0) (aget s 1) (aget s 2))
    (aset s 3 (Phys/clampAll a n e 0 1 (- (aget s 1))))
    s))

(defn- step-up!
  "Puts into out the motion of a body that climbs the block in its
  way, when the climb carries it further sideways than the motion
  already there."
  [chunks ^doubles box0 ^doubles out vel step]
  (let [dy0 (aget out 1)
        s (climb chunks (shifted box0 1 dy0) vel step)
        sx (aget s 0) sz (aget s 2)
        ox (aget out 0) oz (aget out 2)]
    (when (> (+ (* sx sx) (* sz sz)) (+ (* ox ox) (* oz oz)))
      (aset out 0 sx)
      (aset out 1 (+ dy0 (aget s 1) (aget s 3)))
      (aset out 2 sz))))

(defn- moved
  "Returns the move of a body at pos with velocity vel that went
  by out. hit-y? tells that a block stopped it on the y axis."
  ^Move [pos vel ^doubles out hit-y?]
  (let [vx (v/x vel) vy (v/y vel) vz (v/z vel)
        dx (aget out 0) dy (aget out 1) dz (aget out 2)]
    (Move. (v/v3 (+ (v/x pos) dx) (+ (v/y pos) dy)
                 (+ (v/z pos) dz))
           (v/v3 (if (mth-equal? dx vx) vx (restituted vx))
                 (if hit-y? (restituted vy) vy)
                 (if (mth-equal? dz vz) vz (restituted vz)))
           (boolean (and hit-y? (neg? vy))))))

(defn- body-box
  "Returns the box of a body of half width half and height height
  standing at pos."
  ^doubles [pos half height]
  (let [x (v/x pos) y (v/y pos) z (v/z pos) half (double half)]
    (double-array [(- x half) y (- z half)
                   (+ x half) (+ y (double height)) (+ z half)])))

(defn- clamped
  "Returns how far box moves by vel before the blocks stop it."
  ^doubles [chunks ^doubles box vel]
  (let [vx (v/x vel) vy (v/y vel) vz (v/z vel)
        ^Sweep sw (swept-boxes chunks box vx vy vz)
        out (double-array 3)]
    (Phys/clampAxes (boxes sw) (box-count sw) box vx vy vz out)
    out))

(defn- step-up?
  "Whether a body that went by out landed on a block and was held
  back sideways, so it tries to climb by step."
  [^doubles out vel step hit-y?]
  (and (pos? (double step)) hit-y? (neg? (v/y vel))
       (or (not= (aget out 0) (v/x vel))
           (not= (aget out 2) (v/z vel)))))

(defn move
  "Returns the position, velocity and ground flag of a body.
  The body moves by vel from pos and the blocks it meets stop
  it. The body is a box of half width half and height height.
  step is how high it climbs without jumping."
  ([chunks pos vel half height]
   (move chunks pos vel half height 0.0))
  ([chunks pos vel half height step]
   (let [box0 (body-box pos half height)
         out (clamped chunks box0 vel)
         hit-y? (not= (aget out 1) (v/y vel))]
     (when (step-up? out vel step hit-y?)
       (step-up! chunks box0 out vel step))
     (moved pos vel out hit-y?))))
