(ns collider.game.projectile
  "What every projectile shares, the boxes it meets on its way, its
  owner left behind and its turn towards its motion."
  (:require [collider.game.entity :as entity]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mode :as game-mode]
            [collider.game.reach :as reach]
            [collider.num :as num]
            [collider.parallel :as par]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn facing
  "Returns the yaw and the pitch that point along vel."
  [vel]
  (let [x (v/x vel) y (v/y vel) z (v/z vel)
        d (Math/sqrt (+ (* x x) (* z z)))]
    [(Math/toDegrees (Math/atan2 x z))
     (Math/toDegrees (Math/atan2 y d))]))

(defn box-of
  "Returns the box of half width w and height h standing at pos."
  [pos ^double w ^double h]
  (let [x (v/x pos) y (v/y pos) z (v/z pos)]
    [(- x w) y (- z w) (+ x w) (+ y h) (+ z w)]))

(defn body-box
  "Returns the box of e standing at pos."
  [e pos]
  (let [[w h] (entity/box e)] (box-of pos (double w) (double h))))

(defn inflated
  "Returns box b grown by dx, dy and dz on both sides."
  [b ^double dx ^double dy ^double dz]
  [(- (double (b 0)) dx) (- (double (b 1)) dy)
   (- (double (b 2)) dz) (+ (double (b 3)) dx)
   (+ (double (b 4)) dy) (+ (double (b 5)) dz)])

(defn- swept [b d ^double m]
  (let [lo (fn ^double [^long i ^double c]
             (+ (double (b i)) (min c 0.0) (- m)))
        hi (fn ^double [^long i ^double c]
             (+ (double (b i)) (max c 0.0) m))
        dx (v/x d) dy (v/y d) dz (v/z d)]
    [(lo 0 dx) (lo 1 dy) (lo 2 dz) (hi 3 dx) (hi 4 dy) (hi 5 dz)]))

(defn- axis-overlaps?
  [^double lo1 ^double hi1 ^double lo2 ^double hi2]
  (and (< lo1 hi2) (> hi1 lo2)))

(defn overlaps? [a b]
  (and (axis-overlaps? (a 0) (a 3) (b 0) (b 3))
       (axis-overlaps? (a 1) (a 4) (b 1) (b 4))
       (axis-overlaps? (a 2) (a 5) (b 2) (b 5))))

(defn hittable?
  "Returns true when a projectile can hit e."
  [e]
  (or (= :player (:type e)) (mobs/mob-type? (:type e))))

(defn target-box
  "Returns the box of e where it stands."
  [e] (body-box e (:pos e)))

(defn- nearer [best hit tail]
  (if (and hit (or (nil? best)
                   (< (double (hit 0)) (double (best 0)))))
    (into [(hit 0)] (cons (hit 1) tail))
    best))

(defn- cell-range [^double a ^double b]
  (range (long (Math/floor (Math/min a b)))
         (inc (long (Math/floor (Math/max a b))))))

(defn- cells [from to]
  (for [x (cell-range (v/x from) (v/x to))
        y (cell-range (v/y from) (v/y to))
        z (cell-range (v/z from) (v/z to))]
    [x y z]))

(defn- abs-box [[x y z] b]
  (let [c (fn ^double [^long i ^long o]
            (+ (double o) (/ (double (nth b i)) 16.0)))]
    [(c 0 x) (c 1 y) (c 2 z) (c 3 x) (c 4 y) (c 5 z)]))

(defn- cell-clip [chunks cell from d]
  (let [st (chunk/at chunks cell)]
    (when (block/solid? st)
      (reduce (fn [best b]
                (nearer best
                        (reach/box-entry from d (abs-box cell b))
                        nil))
              nil (block/collision-boxes st)))))

(defn block-clip
  "Returns [t face cell] of the first block a ray from along d
  meets, or nil."
  [world from d]
  (reduce (fn [best cell]
            (nearer best
                    (when (chunk/in-range? (nth cell 1))
                      (cell-clip (:chunks world) cell from d))
                    [cell]))
          nil (cells from (v/add from d))))

(defn- skip? [eid e oid o]
  (or (= (long oid) (long eid))
      (not (hittable? o))
      (not (pos? (double (:health o 1.0))))
      (game-mode/spectator? o)
      (and (not (:left-owner? e))
           (= (long oid) (long (:owner e -1))))))

(defn- margin ^double [e]
  (let [lived (dec (long (:age e 0)))]
    (max 0.0 (min (num/f32 0.3) (num/f32 (/ (num/f32 lived) 20.0))))))

(defn- entity-clip [world eid e from d]
  (let [m (margin e)]
    (reduce (fn [best [oid o]]
              (if (skip? eid e oid o)
                best
                (let [box (inflated (target-box o) m m m)]
                  (nearer best (reach/box-entry from d box) [oid]))))
            nil (par/keyed (:entities world)))))

(defn span
  "Returns d as it moves from, rounded as the sum from + d."
  [from d]
  (let [to (v/add from d)]
    [(- (v/x to) (v/x from)) (- (v/y to) (v/y from))
     (- (v/z to) (v/z from))]))

(defn clip
  "Returns the first block or body that projectile e of eid meets
  moving by d, or nil."
  [world eid e d]
  (let [from (:pos e)
        d (span from d)
        b (block-clip world from d)
        x (entity-clip world eid e from d)]
    (cond
      (and x (or (nil? b) (<= (double (x 0)) (double (b 0)))))
      {:kind :entity :t (x 0) :target (nth x 2)}
      b {:kind :block :t (b 0) :face (b 1) :cell (nth b 2)})))

(defn left-owner?
  "Returns true once projectile e moving by d no longer touches its
  owner."
  [world e d]
  (or (boolean (:left-owner? e))
      (if-let [o (get-in world [:entities (:owner e)])]
        (not (overlaps? (swept (body-box e (:pos e)) d 1.0)
                        (target-box o)))
        true)))

(defn point
  "Returns the point at fraction t of d from from."
  [from d ^double t]
  [(+ (v/x from) (* t (v/x d))) (+ (v/y from) (* t (v/y d)))
   (+ (v/z from) (* t (v/z d)))])

(defn- wrapped ^double [^double old ^double new]
  (cond (< (- new old) -180.0) (recur (- old 360.0) new)
        (>= (- new old) 180.0) (recur (+ old 360.0) new)
        :else old))

(defn lerp-rotation
  "Returns angle old turned a fifth of the way to new."
  ^double [^double old ^double new]
  (let [old (wrapped old new)]
    (+ old (* 0.2 (- new old)))))

(defn rotated
  "Returns the yaw and the pitch of projectile e turned towards its
  motion d."
  [e d]
  (let [[yaw pitch] (facing d)]
    [(lerp-rotation (double (:yaw e 0.0)) yaw)
     (lerp-rotation (double (:pitch e 0.0)) pitch)]))
