(ns collider.game.systems.blocks.reach
  "Reach and block raycasts from the eye."
  (:require [collider.game.state :as state]
            [collider.game.systems.blocks.edit :as edit]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const use-buffer 1.0)

(def ^:private ^:const inf Double/POSITIVE_INFINITY)

(def ^:private ^:const neg-inf Double/NEGATIVE_INFINITY)

(defn eye-pos
  "Returns where the entity's eyes are."
  [e]
  (let [p (:pos e) crouch? (and (:sneaking? e) (not (:flying e)))]
    [(v/x p) (+ (v/y p) (if crouch? 1.27 1.62)) (v/z p)]))

(defn axis-gap
  "Returns how far the eye is from a box of size along one axis."
  (^double [^double eye ^double lo] (axis-gap eye lo 1.0))
  (^double [^double eye ^double lo ^double size]
   (max (- lo eye) (- eye (+ lo size)) 0.0)))

(def ^:private ^:const edit-buffer 4.0)

(defn- gap-sq ^double [e pos]
  (let [[ex ey ez] (eye-pos e)
        dx (axis-gap ex (double (nth pos 0)))
        dy (axis-gap ey (double (nth pos 1)))
        dz (axis-gap ez (double (nth pos 2)))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- within? [e pos ^double buffer]
  (let [r (+ (state/block-reach e) buffer)]
    (< (gap-sq e pos) (* r r))))

(defn in-reach?
  "Tests whether the entity may act on the block at pos."
  [e pos]
  (within? e pos use-buffer))

(defn in-edit-range?
  "Tests whether the entity is near enough to keep a sign open.
  Player.isWithinBlockInteractionRange with a buffer of four."
  [e pos]
  (within? e pos edit-buffer))

(defn look-dir
  "Returns the unit vector the entity looks along."
  [e]
  (let [yaw (Math/toRadians (double (:yaw e)))
        pitch (Math/toRadians (double (:pitch e)))]
    [(- (* (Math/sin yaw) (Math/cos pitch)))
     (- (Math/sin pitch))
     (* (Math/cos yaw) (Math/cos pitch))]))

(defn- slab-span
  "Returns the ray fractions entering and leaving the slab lo..hi
  along one axis, with the face the ray enters by."
  [f d lo hi neg pos]
  (let [f (double f) d (double d) lo (double lo) hi (double hi)]
    (cond
      (pos? d) [(/ (- lo f) d) (/ (- hi f) d) neg]
      (neg? d) [(/ (- hi f) d) (/ (- lo f) d) pos]
      (<= lo f hi) [neg-inf inf nil]
      :else [inf inf nil])))

(defn box-entry
  "Returns the ray fraction and face where the box is entered.
  Returns nil when the ray misses."
  [[fx fy fz] [dx dy dz] [x0 y0 z0 x1 y1 z1]]
  (let [[ax bx facex] (slab-span fx dx x0 x1 :west :east)
        [ay by facey] (slab-span fy dy y0 y1 :down :up)
        [az bz facez] (slab-span fz dz z0 z1 :north :south)
        t-in (max (double ax) (double ay) (double az))
        t-out (min (double bx) (double by) (double bz))
        face (cond (= t-in (double ax)) facex
                   (= t-in (double ay)) facey
                   :else facez)]
    (when (and (<= t-in t-out) (< 0.0 t-in 1.0) face)
      [t-in face])))

(defn- unscale ^double [^long base ^double v]
  (+ base (/ v 16.0)))

(defn- box-at [[x y z] [a b c d e f]]
  [(unscale x a) (unscale y b) (unscale z c)
   (unscale x d) (unscale y e) (unscale z f)])

(defn- fluid-height [world pos st fluids]
  (when (not= :none fluids)
    (liquid/fluid-height-of (:chunks world) pos st fluids)))

(defn- fluid-box [world pos st fluids]
  (when-let [fh (fluid-height world pos st fluids)]
    (let [[x y z] pos]
      [[(long x) (long y) (long z)
        (inc (long x)) (+ (long y) (double fh)) (inc (long z))]])))

(defn cell-boxes
  "Returns the world-space boxes a ray can hit in the block cell."
  [world pos fluids]
  (let [st (edit/block-at world pos)
        boxes (when (and (pos? st) (not (block/liquid? st)))
                (map #(box-at pos %) (block/outline-boxes st)))]
    (concat boxes (fluid-box world pos st fluids))))

(defn- axis-step ^long [dc] (if (neg? (double dc)) -1 1))

(defn- first-cross ^double [f dc c]
  (if (zero? (double dc))
    inf
    (let [edge (double (if (pos? (double dc)) (inc (long c)) c))]
      (/ (- edge (double f)) (double dc)))))

(defn- cross-delta ^double [dc] (/ 1.0 (Math/abs (double dc))))

(defn- nearer-hit [best hit]
  (if (or (nil? best) (< (double (hit 0)) (double (best 0))))
    hit
    best))

(defn- cell-hit [world from d cell fluids]
  (reduce (fn [best box]
            (if-let [hit (box-entry from d box)]
              (nearer-hit best hit)
              best))
          nil
          (cell-boxes world cell fluids)))

(defn- first-crosses [from d]
  (mapv (fn [i]
          (first-cross (from i) (d i)
                       (long (Math/floor (double (from i))))))
        (range 3)))

(defn- step-axis ^long [[tx ty tz]]
  (let [tx (double tx) ty (double ty) tz (double tz)]
    (cond (and (<= tx ty) (<= tx tz)) 0
          (<= ty tz) 1
          :else 2)))

(defn- stop? [[tx ty tz] ^long n]
  (or (> n 24)
      (and (> (double tx) 1.0) (> (double ty) 1.0)
           (> (double tz) 1.0))))

(defn- advance [cell d t ^long axis]
  [(update cell axis + (axis-step (d axis)))
   (update t axis + (cross-delta (d axis)))])

(defn clip
  "Returns the block an entity looks at and the face it sees.
  Returns nil when it looks at nothing."
  [world e fluids]
  (let [from (eye-pos e)
        d (mapv #(* (state/block-reach e) (double %)) (look-dir e))]
    (loop [cell (mapv #(long (Math/floor (double %))) from)
           t (first-crosses from d)
           n 0]
      (let [hit (when (chunk/in-range? (cell 1))
                  (cell-hit world from d cell fluids))]
        (cond
          hit {:pos cell :face (second hit)}
          (stop? t n) nil
          :else (let [[cell' t'] (advance cell d t (step-axis t))]
                  (recur cell' t' (inc n))))))))
