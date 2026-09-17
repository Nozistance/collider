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

(defn eye-pos [e]
  (let [p (:pos e) crouch? (and (:sneaking? e) (not (:flying e)))]
    [(v/x p) (+ (v/y p) (if crouch? 1.27 1.62)) (v/z p)]))

(defn axis-gap ^double [^double eye ^double lo]
  (max (- lo eye) (- eye (+ lo 1.0)) 0.0))

(defn in-reach? [e pos]
  (let [[ex ey ez] (eye-pos e)
        dx (axis-gap ex (double (nth pos 0)))
        dy (axis-gap ey (double (nth pos 1)))
        dz (axis-gap ez (double (nth pos 2)))]
    (< (+ (* dx dx) (* dy dy) (* dz dz))
       (let [r (+ (state/block-reach e) use-buffer)] (* r r)))))

(defn look-dir [e]
  (let [yaw (Math/toRadians (double (:yaw e)))
        pitch (Math/toRadians (double (:pitch e)))]
    [(- (* (Math/sin yaw) (Math/cos pitch)))
     (- (Math/sin pitch))
     (* (Math/cos yaw) (Math/cos pitch))]))

(defn box-entry
  "Returns the ray fraction where the box is entered and the face entered, or nil
   when the ray misses."
  [[fx fy fz] [dx dy dz] [x0 y0 z0 x1 y1 z1]]
  (let [axis (fn [f d lo hi neg pos]
               (cond (pos? (double d)) [(/ (- (double lo) (double f)) (double d)) (/ (- (double hi) (double f)) (double d)) neg]
                     (neg? (double d)) [(/ (- (double hi) (double f)) (double d)) (/ (- (double lo) (double f)) (double d)) pos]
                     :else [(if (<= (double lo) (double f) (double hi)) Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY) Double/POSITIVE_INFINITY nil]))
        [ax bx facex] (axis fx dx x0 x1 :west :east)
        [ay by facey] (axis fy dy y0 y1 :down :up)
        [az bz facez] (axis fz dz z0 z1 :north :south)
        t-in (max (double ax) (double ay) (double az))
        t-out (min (double bx) (double by) (double bz))
        face (cond (= t-in (double ax)) facex (= t-in (double ay)) facey :else facez)]
    (when (and (<= t-in t-out) (< 0.0 t-in 1.0) face)
      [t-in face])))

(defn cell-boxes [world [x y z :as pos] fluids]
  (let [st (edit/block-at world pos)
        abs (fn [[a b c d e f]] [(+ (long x) (/ (double a) 16.0)) (+ (long y) (/ (double b) 16.0)) (+ (long z) (/ (double c) 16.0))
                                 (+ (long x) (/ (double d) 16.0)) (+ (long y) (/ (double e) 16.0)) (+ (long z) (/ (double f) 16.0))])
        fh (when (not= :none fluids) (liquid/fluid-height-of (:chunks world) pos st fluids))
        fluid (when fh
                [[(long x) (long y) (long z) (inc (long x)) (+ (long y) (double fh)) (inc (long z))]])]
    (concat (when (and (pos? st) (not (liquid/liquid-state? st))) (map abs (block/outline-boxes st)))
            fluid)))

(defn- axis-step ^long [dc] (if (neg? (double dc)) -1 1))

(defn- first-cross ^double [f dc c]
  (if (zero? (double dc))
    Double/POSITIVE_INFINITY
    (/ (- (if (pos? (double dc)) (inc (long c)) (double c)) (double f)) (double dc))))

(defn- cross-delta ^double [dc] (/ 1.0 (Math/abs (double dc))))

(defn- cell-hit [world from d cell fluids]
  (reduce (fn [best box]
            (if-let [hit (box-entry from d box)]
              (if (or (nil? best) (< (double (hit 0)) (double (best 0)))) hit best)
              best))
          nil (cell-boxes world cell fluids)))

(defn- first-crosses [from d]
  (mapv (fn [i] (first-cross (from i) (d i) (long (Math/floor (double (from i))))))
        (range 3)))

(defn- step-axis ^long [[tx ty tz]]
  (cond (and (<= (double tx) (double ty)) (<= (double tx) (double tz))) 0
        (<= (double ty) (double tz)) 1
        :else 2))

(defn- stop? [[tx ty tz] ^long n]
  (or (> n 24) (and (> (double tx) 1.0) (> (double ty) 1.0) (> (double tz) 1.0))))

(defn- advance [cell d t ^long axis]
  [(update cell axis + (axis-step (d axis)))
   (update t axis + (cross-delta (d axis)))])

(defn clip
  "Returns the block an entity looks at and the face it sees, or nil when it looks
   at nothing."
  [world e fluids]
  (let [from (eye-pos e)
        d (mapv #(* (state/block-reach e) (double %)) (look-dir e))]
    (loop [cell (mapv #(long (Math/floor (double %))) from)
           t (first-crosses from d)
           n 0]
      (let [hit (when (chunk/in-range? (cell 1)) (cell-hit world from d cell fluids))]
        (cond
          hit {:pos cell :face (second hit)}
          (stop? t n) nil
          :else (let [[cell' t'] (advance cell d t (step-axis t))]
                  (recur cell' t' (inc n))))))))
