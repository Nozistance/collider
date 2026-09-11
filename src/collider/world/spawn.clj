(ns collider.world.spawn
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.liquid :as liquid]))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-attempts 1024)
(def ^:private ^:const player-half 0.3)
(def ^:private ^:const player-height 1.8)
(def ^:private ^:const eps 1.0E-7)
(def ^:private ^:const none -65)

(def ^:private air-blocks #{:air :cave-air :void-air})

(defn- state-at [chunks template x y z]
  (if (chunk/in-range? (long y)) (chunk/chunks-get-block chunks template x y z) 0))

(defn- air? [^long st] (contains? air-blocks (block/block-of st)))

(defn- fluid? [^long st] (some? (liquid/liquid-class st)))

(defn- motion-blocking? [^long st] (or (block/blocks-motion? st) (fluid? st)))

(defn- own-height ^double [^long st]
  (let [l (liquid/level st)]
    (/ (double (if (or (zero? l) (>= l 8)) 8 (- 8 l))) 9.0)))

(defn- fluid-height [chunks template x y z st]
  (if (= (liquid/liquid-class (long st))
         (liquid/liquid-class (long (state-at chunks template x (inc (long y)) z))))
    1.0
    (own-height (long st))))

(defn- column-heights [chunks template x z]
  (loop [y (long chunk/max-y) surface none motion none floor none]
    (if (or (< y (long chunk/min-y)) (not= floor none))
      [surface motion floor]
      (let [st (long (state-at chunks template x y z))]
        (recur (dec y)
               (if (and (= surface none) (not (air? st))) y surface)
               (if (and (= motion none) (motion-blocking? st)) y motion)
               (if (block/blocks-motion? st) y floor))))))

(defn- overlaps? [lo box]
  (let [[x0 y0 z0 x1 y1 z1] lo [a b c d e f] box]
    (and (< (double x0) (double d)) (> (double x1) (double a))
         (< (double y0) (double e)) (> (double y1) (double b))
         (< (double z0) (double f)) (> (double z1) (double c)))))

(defn- cell-boxes [chunks template x y z]
  (let [x (long x) y (long y) z (long z)
        st (long (state-at chunks template x y z))
        solid (mapv (fn [[a b c d e f]]
                      [(+ x (/ (double a) 16.0)) (+ y (/ (double b) 16.0)) (+ z (/ (double c) 16.0))
                       (+ x (/ (double d) 16.0)) (+ y (/ (double e) 16.0)) (+ z (/ (double f) 16.0))])
                    (block/collision-boxes st))]
    (if (fluid? st)
      (conj solid [(double x) (double y) (double z)
                   (+ x 1.0) (+ y (double (fluid-height chunks template x y z st))) (+ z 1.0)])
      solid)))

(defn- box-free? [chunks template px py pz]
  (let [px (long px) py (long py) pz (long pz)
        cx (+ px 0.5) cz (+ pz 0.5)
        x0 (- cx player-half) x1 (+ cx player-half)
        y0 (double py) y1 (+ py player-height)
        z0 (- cz player-half) z1 (+ cz player-half)
        i0 (dec (long (Math/floor (- x0 eps)))) i1 (inc (long (Math/floor (+ x1 eps))))
        j0 (dec (long (Math/floor (- y0 eps)))) j1 (inc (long (Math/floor (+ y1 eps))))
        k0 (dec (long (Math/floor (- z0 eps)))) k1 (inc (long (Math/floor (+ z1 eps))))]
    (loop [i i0 j j0 k k0]
      (cond
        (> i i1) true
        (> j j1) (recur (inc i) j0 k0)
        (> k k1) (recur i (inc j) k0)
        (some (partial overlaps? [x0 y0 z0 x1 y1 z1]) (cell-boxes chunks template i j k)) false
        :else (recur i j (inc k))))))

(defn- bottom-center [[x y z]]
  [(+ (long x) 0.5) (double y) (+ (long z) 0.5)])

(defn- level-respawn-pos [chunks template x z]
  (let [[surface top floor] (column-heights chunks template x z)]
    (when (and (>= (long top) (long chunk/min-y))
               (not (and (<= (long surface) (long top)) (> (long surface) (long floor)))))
      (loop [y (inc (long top))]
        (when (>= y (long chunk/min-y))
          (let [st (long (state-at chunks template x y z))]
            (cond
              (fluid? st) nil
              (block/collision-face-full-up? st) [x (inc y) z]
              :else (recur (dec y)))))))))

(defn- fixup-height [chunks template [x y z]]
  (let [x (long x) z (long z)
        up (loop [y (long y)]
             (if (or (box-free? chunks template x y z) (>= y (long chunk/max-y)))
               y
               (recur (inc y))))
        down (loop [y (dec (long up))]
               (if (or (not (box-free? chunks template x y z)) (<= y (long chunk/min-y)))
                 y
                 (recur (dec y))))]
    (bottom-center [x (inc (long down)) z])))

(defn- coprime ^long [^long n] (if (<= n 16) (dec n) 17))

(defn find-spawn
  [chunks template suggestion radius seed]
  (let [radius (max 0 (long radius))
        seed (double seed)
        side (inc (* 2 radius))
        n (long (min (long max-attempts) (* (long side) (long side))))
        step (coprime n)
        offset (long (Math/floor (* seed n)))
        ox (long (nth suggestion 0)) oz (long (nth suggestion 2))]
    (loop [i 0]
      (if (>= i n)
        (fixup-height chunks template suggestion)
        (let [value (rem (+ offset (* step i)) n)
              x (+ ox (rem value side) (- radius))
              z (+ oz (quot value side) (- radius))
              pos (level-respawn-pos chunks template x z)]
          (if (and pos (box-free? chunks template (nth pos 0) (nth pos 1) (nth pos 2)))
            (bottom-center pos)
            (recur (inc i))))))))
