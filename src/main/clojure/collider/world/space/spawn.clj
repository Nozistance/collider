(ns collider.world.space.spawn
  "Where a player spawns in a level."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.space.column :as column]))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-attempts 1024)

(def ^:private ^:const eps 1.0E-7)

(defn- overlaps? [lo box]
  (let [[x0 y0 z0 x1 y1 z1] lo [a b c d e f] box]
    (and (< (double x0) (double d)) (> (double x1) (double a))
         (< (double y0) (double e)) (> (double y1) (double b))
         (< (double z0) (double f)) (> (double z1) (double c)))))

(defn- box-at [^long x ^long y ^long z [a b c d e f]]
  [(+ x (/ (double a) 16.0)) (+ y (/ (double b) 16.0))
   (+ z (/ (double c) 16.0)) (+ x (/ (double d) 16.0))
   (+ y (/ (double e) 16.0)) (+ z (/ (double f) 16.0))])

(defn- fluid-box [chunks x y z st]
  (let [x (long x) y (long y) z (long z)
        h (double (liquid/height chunks [x y z] st))]
    [(double x) (double y) (double z) (+ x 1.0) (+ y h) (+ z 1.0)]))

(defn- cell-boxes [chunks x y z]
  (let [x (long x) y (long y) z (long z)
        st (long (column/state-at chunks x y z))
        solid (mapv #(box-at x y z %) (block/collision-boxes st))]
    (if (column/fluid? st)
      (conj solid (fluid-box chunks x y z st))
      solid)))

(def ^:private ^:table player-size
  (delay (let [{:keys [width height]} (:player (data/entities))]
           [(* 0.5 (double (float width))) (double (float height))])))

(defn- player-box [^long px ^long py ^long pz]
  (let [cx (+ px 0.5) cz (+ pz 0.5)
        w (double (nth @player-size 0))
        h (double (nth @player-size 1))]
    [(- cx w) (double py) (- cz w) (+ cx w) (+ py h) (+ cz w)]))

(defn- cell-edge ^long [^double v ^long d]
  (+ d (long (Math/floor (+ v (* d (double eps)))))))

(defn- player-box-free? [chunks px py pz]
  (let [box (player-box (long px) (long py) (long pz))
        [x0 y0 z0 x1 y1 z1] box
        i0 (cell-edge x0 -1) i1 (cell-edge x1 1)
        j0 (cell-edge y0 -1) j1 (cell-edge y1 1)
        k0 (cell-edge z0 -1) k1 (cell-edge z1 1)]
    (loop [i i0 j j0 k k0]
      (cond
        (> i i1) true
        (> j j1) (recur (inc i) j0 k0)
        (> k k1) (recur i (inc j) k0)
        (some (partial overlaps? box) (cell-boxes chunks i j k)) false
        :else (recur i j (inc k))))))

(defn- bottom-center [[x y z]]
  [(+ (long x) 0.5) (double y) (+ (long z) 0.5)])

(defn- open-top? [surface top floor]
  (not (and (<= (long surface) (long top))
            (> (long surface) (long floor)))))

(defn- level-respawn-pos [chunks x z]
  (let [[surface top floor] (column/column-heights chunks x z)]
    (when (and (>= (long top) (long chunk/min-y))
               (open-top? surface top floor))
      (loop [y (inc (long top))]
        (when (>= y (long chunk/min-y))
          (let [st (long (column/state-at chunks x y z))]
            (cond
              (column/fluid? st) nil
              (block/collision-face-full-up? st) [x (inc y) z]
              :else (recur (dec y)))))))))

(defn- rise [chunks x y z]
  (loop [y (long y)]
    (if (or (player-box-free? chunks x y z)
            (>= y (long chunk/max-y)))
      y
      (recur (inc y)))))

(defn- sink [chunks x y z]
  (loop [y (long y)]
    (if (or (not (player-box-free? chunks x y z))
            (<= y (long chunk/min-y)))
      y
      (recur (dec y)))))

(defn- fixup-height [chunks [x y z]]
  (let [x (long x) z (long z)
        up (rise chunks x y z)
        down (sink chunks x (dec (long up)) z)]
    (bottom-center [x (inc (long down)) z])))

(defn- coprime
  ^long [^long n] (if (<= n 16) (dec n) 17))

(defn- scan-params [radius seed]
  (let [radius (max 0 (long radius))
        side (inc (* 2 radius))
        n (min (long max-attempts) (* side side))
        offset (long (Math/floor (* (double seed) n)))]
    [radius side n (coprime n) offset]))

(defn- candidate-cell [params ^long ox ^long oz ^long i]
  (let [[radius side n step offset] params
        value (rem (+ (long offset) (* (long step) i)) (long n))]
    [(+ ox (rem value (long side)) (- (long radius)))
     (+ oz (quot value (long side)) (- (long radius)))]))

(defn- free-spawn [chunks x z]
  (when-let [[px py pz :as pos] (level-respawn-pos chunks x z)]
    (when (player-box-free? chunks px py pz)
      (bottom-center pos))))

(defn search-chunk-ids
  "Returns the ids of the chunks a spawn search reads.
  The search covers radius around suggestion."
  [suggestion radius]
  (let [r (max 0 (long radius))
        x (long (nth suggestion 0))
        z (long (nth suggestion 2))
        span (fn [^long c]
               (range (chunk/block->chunk (- c r))
                      (inc (chunk/block->chunk (+ c r)))))]
    (for [cx (span x) cz (span z)] (chunk/pos->id cx cz))))

(defn find-spawn
  "Returns a standing position with room for a player.
  It looks at the columns within radius of suggestion in an
  order decided by seed. When no column is free, it returns a
  position above or below suggestion instead."
  [chunks suggestion radius seed]
  (let [[_ _ n :as params] (scan-params radius seed)
        ox (long (nth suggestion 0)) oz (long (nth suggestion 2))]
    (loop [i 0]
      (if (>= i (long n))
        (fixup-height chunks suggestion)
        (let [[x z] (candidate-cell params ox oz i)]
          (or (free-spawn chunks x z) (recur (inc i))))))))
