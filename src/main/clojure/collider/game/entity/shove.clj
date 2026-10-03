(ns collider.game.entity.shove
  "Bodies stuck in blocks pushed towards open space."
  (:require [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private sides
  [[:z -1] [:z 1] [:x -1] [:x 1] [:y 1]])

(defn- full-block? [chunks [x y z]]
  (and (chunk/in-range? (long y))
       (block/full-cube? (chunk/chunks-get-block chunks [x y z]))))

(defn- beside [[bx by bz] [a s]]
  (let [s (long s) bx (long bx) by (long by) bz (long bz)]
    (case a
      :x [(+ bx s) by bz] :y [bx (+ by s) bz] :z [bx by (+ bz s)])))

(defn- toward ^double [d [a s]]
  (if (pos? (long s)) (- 1.0 (double (d a))) (double (d a))))

(defn- closest-side [chunks ^double x ^double y ^double z]
  (let [b [(num/floor x) (num/floor y) (num/floor z)]
        d {:x (- x (double (b 0))) :y (- y (double (b 1)))
           :z (- z (double (b 2)))}
        open? #(not (full-block? chunks (beside b %)))
        step (fn [[_ best :as acc] side]
               (let [o (toward d side)]
                 (if (and (< o (double best)) (open? side))
                   [side o]
                   acc)))]
    (first (reduce step [[:y 1] Double/MAX_VALUE] sides))))

(defn shoved
  "Returns velocity vel of a body of that height at pos stuck in a
  block, pushed towards the nearest side of its block that is no
  full block, up when none. r is the random float of the push."
  [chunks pos height vel r]
  (let [y0 (v/y pos)
        y (/ (+ y0 (+ y0 (double height))) 2.0)
        [a s] (closest-side chunks (v/x pos) y (v/z pos))
        spread (num/fmul (num/f32 r) (num/f32 0.2))
        speed (num/f32 (+ spread (num/f32 0.1)))
        push (num/f32 (* (double s) speed))
        sx (* (v/x vel) 0.75) sy (* (v/y vel) 0.75)
        sz (* (v/z vel) 0.75)]
    (case a
      :x [push sy sz] :y [sx push sz] :z [sx sy push])))
