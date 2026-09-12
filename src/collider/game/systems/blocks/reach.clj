(ns collider.game.systems.blocks.reach
  (:require [collider.game.systems.blocks.edit :as edit]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def ^:const block-interaction-range 6.0)

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
       (* block-interaction-range block-interaction-range))))

(defn look-dir [e]
  (let [yaw (Math/toRadians (double (:yaw e)))
        pitch (Math/toRadians (double (:pitch e)))]
    [(- (* (Math/sin yaw) (Math/cos pitch)))
     (- (Math/sin pitch))
     (* (Math/cos yaw) (Math/cos pitch))]))

(defn box-entry [[fx fy fz] [dx dy dz] [x0 y0 z0 x1 y1 z1]]
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
        fluid (when (and (not= :none fluids) (liquid/fluid-height-of (:chunks world) gen/flat-chunk pos st fluids))
                [[(long x) (long y) (long z) (inc (long x)) (+ (long y) (double (liquid/fluid-height-of (:chunks world) gen/flat-chunk pos st fluids))) (inc (long z))]])]
    (concat (when (and (pos? st) (not (liquid/liquid-state? st))) (map abs (block/outline-boxes st)))
            fluid)))

(defn clip [world e fluids]
  (let [from (eye-pos e) dir (look-dir e)
        d (mapv #(* 5.0 (double %)) dir)
        step (fn [_ dc] (if (neg? (double dc)) -1 1))
        next-t (fn [f dc c] (if (zero? (double dc)) Double/POSITIVE_INFINITY
                                                    (/ (- (if (pos? (double dc)) (inc (long c)) (double c)) (double f)) (double dc))))]
    (loop [[cx cy cz :as cell] (mapv #(long (Math/floor (double %))) from)
           tx (next-t (from 0) (d 0) cx) ty (next-t (from 1) (d 1) cy) tz (next-t (from 2) (d 2) cz)
           n 0]
      (let [hit (when (chunk/in-range? cy)
                  (first (sort-by first (keep #(box-entry from d %) (cell-boxes world cell fluids)))))]
        (cond
          hit {:pos cell :face (second hit)}
          (or (> n 24) (every? #(> (double %) 1.0) [tx ty tz])) nil
          (and (<= tx ty) (<= tx tz)) (recur [(+ cx (step cx (d 0))) cy cz] (+ tx (/ 1.0 (Math/abs (double (d 0))))) ty tz (inc n))
          (<= ty tz) (recur [cx (+ cy (step cy (d 1))) cz] tx (+ ty (/ 1.0 (Math/abs (double (d 1))))) tz (inc n))
          :else (recur [cx cy (+ cz (step cz (d 2)))] tx ty (+ tz (/ 1.0 (Math/abs (double (d 2))))) (inc n)))))))
