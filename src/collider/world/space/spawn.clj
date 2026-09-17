(ns collider.world.space.spawn
  "Places to put a player: the world spawn and the room a body needs to stand."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.liquid :as liquid]))

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

(defn motion-blocking-height
  "Returns the y just above the highest block or fluid of the column at x z."
  ^long [chunks template x z]
  (let [[_ motion _] (column-heights chunks template x z)]
    (if (= (long motion) (long none)) (long chunk/min-y) (inc (long motion)))))

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

(defn- player-box [^long px ^long py ^long pz]
  (let [cx (+ px 0.5) cz (+ pz 0.5)]
    [(- cx player-half) (double py) (- cz player-half)
     (+ cx player-half) (+ py player-height) (+ cz player-half)]))

(defn- cell-edge ^long [^double v ^long d]
  (+ d (long (Math/floor (+ v (* d (double eps)))))))

(defn- box-free? [chunks template px py pz]
  (let [[x0 y0 z0 x1 y1 z1 :as box] (player-box (long px) (long py) (long pz))
        i0 (cell-edge x0 -1) i1 (cell-edge x1 1)
        j0 (cell-edge y0 -1) j1 (cell-edge y1 1)
        k0 (cell-edge z0 -1) k1 (cell-edge z1 1)]
    (loop [i i0 j j0 k k0]
      (cond
        (> i i1) true
        (> j j1) (recur (inc i) j0 k0)
        (> k k1) (recur i (inc j) k0)
        (some (partial overlaps? box) (cell-boxes chunks template i j k)) false
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

(defn- coprime
  "Returns a stride that walks every one of n cells before repeating."
  ^long [^long n] (if (<= n 16) (dec n) 17))

(defn- scan-params [radius seed]
  (let [radius (max 0 (long radius))
        side (inc (* 2 radius))
        n (long (min (long max-attempts) (* (long side) (long side))))]
    [radius side n (coprime n) (long (Math/floor (* (double seed) n)))]))

(defn- candidate-cell [[radius side n step offset] ^long ox ^long oz ^long i]
  (let [value (rem (+ (long offset) (* (long step) i)) (long n))]
    [(+ ox (rem value (long side)) (- (long radius)))
     (+ oz (quot value (long side)) (- (long radius)))]))

(defn- free-spawn [chunks template x z]
  (let [pos (level-respawn-pos chunks template x z)]
    (when (and pos (box-free? chunks template (nth pos 0) (nth pos 1) (nth pos 2)))
      (bottom-center pos))))

(defn find-spawn
  "Returns a standing position with room for a player, looking at the columns
   within radius of suggestion in an order decided by seed. When no column is
   free, returns a position above or below suggestion instead."
  [chunks template suggestion radius seed]
  (let [[_ _ n :as params] (scan-params radius seed)
        ox (long (nth suggestion 0)) oz (long (nth suggestion 2))]
    (loop [i 0]
      (if (>= i (long n))
        (fixup-height chunks template suggestion)
        (let [[x z] (candidate-cell params ox oz i)]
          (if-let [p (free-spawn chunks template x z)] p (recur (inc i))))))))
