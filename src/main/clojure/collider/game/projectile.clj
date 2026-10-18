(ns collider.game.projectile
  "What every projectile shares, the boxes it meets on its way, its
  owner left behind and its turn towards its motion."
  (:require [collider.cell :as cell]
            [collider.data :as data]
            [collider.game.changes :as changes]
            [collider.game.entity :as entity]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mode :as game-mode]
            [collider.game.reach :as reach]
            [collider.game.schedule :as schedule]
            [collider.num :as num]
            [collider.parallel :as par]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.target :as target]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- degrees ^double [^double rad] (num/f32 (num/degrees rad)))

(defn facing
  "Returns the yaw and the pitch that point along vel."
  [vel]
  (let [x (v/x vel) y (v/y vel) z (v/z vel)
        d (Math/sqrt (+ (* x x) (* z z)))]
    [(degrees (num/atan2 x z)) (degrees (num/atan2 y d))]))

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

(defn gap-sq
  "Returns the square of the gap between boxes a and b, 0 when they
  touch."
  ^double [a b]
  (let [gap (fn ^double [^long i]
              (max (- (double (a i)) (double (b (+ i 3))))
                   (- (double (b i)) (double (a (+ i 3))))
                   0.0))
        x (gap 0) y (gap 1) z (gap 2)]
    (+ (* x x) (* y y) (* z z))))

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

(defn abs-box
  "Returns collision box b of a block at cell in world space."
  [[x y z] b]
  (let [c (fn ^double [^long i ^long o]
            (+ (double o) (/ (double (nth b i)) 16.0)))]
    [(c 0 x) (c 1 y) (c 2 z) (c 3 x) (c 4 y) (c 5 z)]))

(defn point
  "Returns the point at fraction t of d from from."
  [from d ^double t]
  [(+ (v/x from) (* t (v/x d))) (+ (v/y from) (* t (v/y d)))
   (+ (v/z from) (* t (v/z d)))])

(defn inside?
  "Returns true when point p lies in box b, its far sides open."
  [b p]
  (and (<= (double (b 0)) (v/x p)) (< (v/x p) (double (b 3)))
       (<= (double (b 1)) (v/y p)) (< (v/y p) (double (b 4)))
       (<= (double (b 2)) (v/z p)) (< (v/z p) (double (b 5)))))

(defn- dot ^double [d k]
  (let [[x y z] (dir/offset k) f num/f32]
    (f (+ (f (+ (f (* (f (v/x d)) (double x)))
                (f (* (f (v/y d)) (double y)))))
          (f (* (f (v/z d)) (double z)))))))

(defn- facing-away
  "Returns the face a ray along d meets head on."
  [d]
  (let [best (reduce (fn [[_ hi :as best] k]
                       (let [x (dot d k)]
                         (if (> x (double hi)) [k x] best)))
                     [:north Float/MIN_VALUE] dir/six)]
    (dir/opposite (first best))))

(def ^:private ^:const entry-step 0.001)

(defn- cell-clip [chunks cell from d]
  (let [st (chunk/at chunks cell)]
    (when (block/solid? st)
      (let [bs (mapv #(abs-box cell %) (block/collision-boxes st))
            p (point from d entry-step)]
        (if (some #(inside? % p) bs)
          [entry-step (facing-away d)]
          (reduce (fn [best b]
                    (nearer best (reach/box-entry from d b) nil))
                  nil bs))))))

(def ^:private ^:const min-ray-sq 1.0E-7)

(defn block-clip
  "Returns [t face cell] of the first block a ray from along d
  meets, or nil. A ray that starts its first thousandth inside a
  block meets it there, and a ray too short meets nothing."
  [world from d]
  (when (>= (v/dot d d) min-ray-sq)
    (reduce (fn [best cell]
              (nearer best
                      (when (chunk/in-range? (nth cell 1))
                        (cell-clip (:chunks world) cell from d))
                      [cell]))
            nil (cells from (v/add from d)))))

(defn- skip? [eid e oid o]
  (or (= (long oid) (long eid))
      (not (hittable? o))
      (not (pos? (double (:health o 1.0))))
      (game-mode/spectator? o)
      (and (not (:left-owner? e))
           (= (long oid) (long (:owner e -1))))))

(def ^:private ^:const face-slack 1.0E-7)

(defn- face-hit
  "Returns the fraction t where a ray crosses the face of box b at
  index i along axis a, when it does so nearer than t and inside the
  face, or t."
  [b from d a i t]
  (let [t (double t)
        s (/ (- (double (b i)) (double (from a))) (double (d a)))
        inside? (fn [^long k]
                  (let [x (+ (double (from k)) (* s (double (d k))))]
                    (and (< (- (double (b k)) face-slack) x)
                         (< x (+ (double (b (+ k 3))) face-slack)))))]
    (if (and (< 0.0 s t) (every? inside? (remove #{a} [0 1 2])))
      s
      t)))

(defn- entry
  "Returns the fraction of ray from along d where it enters box b
  through a face it faces, or nil."
  [b from d]
  (let [hit (fn [^double t a]
              (let [da (double (d a))]
                (cond (> da face-slack) (face-hit b from d a a t)
                      (< da (- face-slack))
                      (face-hit b from d a (+ a 3) t)
                      :else t)))
        t (double (reduce hit 1.0 [0 1 2]))]
    (when (< t 1.0) t)))

(defn box-clip
  "Returns the point where a ray from to to enters box b through a
  face it faces, or nil."
  [b from to]
  (let [from [(v/x from) (v/y from) (v/z from)]
        d (mapv - [(v/x to) (v/y to) (v/z to)] from)]
    (some->> (entry b from d) (point from d))))

(defn margin
  "Returns how far beyond its box projectile e meets a body, growing
  with its age."
  ^double [e]
  (let [lived (dec (long (:age e 0)))]
    (max 0.0 (min (num/f32 0.3) (num/f32 (/ (num/f32 lived) 20.0))))))

(defn- entity-clip [world eid e from d]
  (let [m (margin e) from [(v/x from) (v/y from) (v/z from)]]
    (reduce (fn [best [oid o]]
              (if (skip? eid e oid o)
                best
                (let [box (inflated (target-box o) m m m)
                      hit (some-> (entry box from d) (vector nil))]
                  (nearer best hit [oid]))))
            nil (par/keyed (:entities world)))))

(defn- cut
  "Returns the move from from to to, rounded as their difference."
  [from to]
  [(- (v/x to) (v/x from)) (- (v/y to) (v/y from))
   (- (v/z to) (v/z from))])

(defn span
  "Returns d as it moves from, rounded as the sum from + d."
  [from d]
  (cut from (v/add from d)))

(defn clip
  "Returns the first block or body that projectile e of eid meets
  moving by d, with the point it meets it at, or nil. A body is
  sought on the ray cut short at the block."
  [world eid e d]
  (let [from (:pos e)
        d (span from d)
        b (block-clip world from d)
        to (when b (point from d (b 0)))
        d (if b (cut from to) d)
        x (entity-clip world eid e from d)]
    (cond
      x {:kind :entity :at (point from d (x 0)) :target (nth x 2)}
      b {:kind :block :at to :face (b 1) :cell (nth b 2)})))

(defn left-owner?
  "Returns true once projectile e moving by d no longer touches its
  owner."
  [world e d]
  (or (boolean (:left-owner? e))
      (if-let [o (get-in world [:entities (:owner e)])]
        (not (overlaps? (swept (body-box e (:pos e)) d 1.0)
                        (target-box o)))
        true)))

(defn- wrapped ^double [^double old ^double new]
  (cond (< (num/f32 (- new old)) -180.0) (recur (num/f32 (- old 360.0)) new)
        (>= (num/f32 (- new old)) 180.0) (recur (num/f32 (+ old 360.0)) new)
        :else old))

(defn lerp-rotation
  "Returns angle old turned a fifth of the way to new, in floats."
  ^double [^double old ^double new]
  (let [old (wrapped (num/f32 old) new)]
    (num/f32 (+ old (num/f32 (* (num/f32 0.2) (num/f32 (- new old))))))))

(defn rotated
  "Returns the yaw and the pitch of projectile e turned towards its
  motion d."
  [e d]
  (let [[yaw pitch] (facing d)]
    [(lerp-rotation (double (:yaw e 0.0)) yaw)
     (lerp-rotation (double (:pitch e 0.0)) pitch)]))

(def ^:private ^:table impact
  (delay (set (data/tag-values "entity_type" "impact_projectiles"))))

(defn- may-interact? [world e]
  (let [o (get-in world [:entities (:owner e)])]
    (or (nil? o) (= :player (:type o))
        (get-in world [:rules :mob-griefing] true))))

(defn- may-break? [world e]
  (and (contains? @impact (:type e))
       (get-in world [:rules :projectiles-can-break-blocks] true)))

(defn- burning? [e] (pos? (long (:fire e 0))))

(defn- lit? [st] (= :true (:lit (block/props-of st))))

(defn- soaked? [st] (= :true (:waterlogged (block/props-of st))))

(defn- lights? [world e st]
  (and (burning? e) (not (lit? st)) (not (soaked? st))
       (case (block/type-of st)
         :campfire (may-interact? world e)
         (:candle :candle-cake) true
         false)))

(defn- primer [world e]
  (let [o (get-in world [:entities (:owner e)])]
    (when (or (= :player (:type o)) (mobs/mob-type? (:type o)))
      (:owner e))))

(defn- primed-deltas [world e cell]
  (into (changes/change-deltas world [[cell 0]])
        (changes/primed-deltas world cell (primer world e))))

(defn- held-ticks
  "Returns how long a hit of projectile e keeps a target powered."
  ^long [e]
  (let [t (:type e)]
    (cond
      (#{:arrow :spectral-arrow} t) 20
      (contains? entity/thrown-types t) 8
      :else (throw (ex-info "a target hit by a projectile not known"
                            {:type t})))))

(defn- target-deltas [world e {:keys [at face cell]} st]
  (let [t (long (:tick world))
        o (:owner e)]
    (concat
      (when-not (schedule/scheduled? (:block-ticks world) (cell/pack cell)
                                     :target)
        (changes/change-deltas
          world
          (target/hit cell st (target/strength at face)
                      (held-ticks e) t)
          t))
      (when (= :player (get-in world [:entities o :type]))
        [[:award o :custom/target-hit 1]]))))

(defn block-hit-deltas
  "Returns the deltas of projectile e hitting the block at the :cell of
  hit, on its :face at point :at. A chorus flower breaks with its
  drops and a target is powered. A burning one primes tnt and lights
  a campfire or a candle."
  [world e {:keys [cell] :as hit}]
  (let [st (changes/block-at world cell)]
    (cond
      (target/target? st) (target-deltas world e hit st)
      (and (= :chorus-flower (block/type-of st))
           (may-interact? world e) (may-break? world e))
      (changes/change-deltas world
                             [[cell 0 [[:break st] [:drop st]]]])
      (and (block/tnt? st) (burning? e) (may-interact? world e)
           (get-in world [:rules :tnt-explodes] true))
      (primed-deltas world e cell)
      (lights? world e st)
      (changes/change-deltas world
                             [[cell (block/with st :lit :true)]]))))
