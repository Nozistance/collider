(ns collider.world.space.explosion
  "Explosions, the blocks they break, their drops and their
  reach into a body."
  (:require [collider.data :as data]
            [collider.par :as par]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys])
  (:import (clojure.lang Atom)
           (collider.world.space Exposure Rays SectionGrid)))

(set! *warn-on-reflection* true)

(def ^:private ^:const region-r 10)

(defn- rg-grid ^objects [^SectionGrid rg] (.grid rg))

(defn- rg-cols ^objects [^SectionGrid rg] (.cols rg))

(defn- rg-read [^SectionGrid rg] (.readAbsent rg))

(defn- rg-loaded ^Atom [^SectionGrid rg] (.loaded rg))

(defn- rg-cx0 ^long [^SectionGrid rg] (.cx0 rg))

(defn- rg-cz0 ^long [^SectionGrid rg] (.cz0 rg))

(defn- rg-sy0 ^long [^SectionGrid rg] (.sy0 rg))

(defn- rg-ncx ^long [^SectionGrid rg] (.ncx rg))

(defn- rg-ncz ^long [^SectionGrid rg] (.ncz rg))

(defn- rg-nsy ^long [^SectionGrid rg] (.nsy rg))

(defn loaded-payloads
  "Returns the chunks that the reads of rg load, by id."
  [^SectionGrid rg]
  @(rg-loaded rg))

(defn- section-range [^long cy]
  [(max (bit-shift-right chunk/min-y 4)
        (bit-shift-right (- cy region-r) 4))
   (min (bit-shift-right chunk/max-y 4)
        (bit-shift-right (+ cy region-r) 4))])

(defn- region-bounds [[cx cy cz]]
  (let [cx0 (bit-shift-right (- (long cx) region-r) 4)
        cx1 (bit-shift-right (+ (long cx) region-r) 4)
        cz0 (bit-shift-right (- (long cz) region-r) 4)
        cz1 (bit-shift-right (+ (long cz) region-r) 4)
        [sy0 sy1] (section-range (long cy))]
    [cx0 cz0 sy0 (inc (- cx1 cx0)) (inc (- cz1 cz0))
     (max 0 (inc (- sy1 sy0)))]))

(defn- section-at [col ^long sy]
  (chunk/chunk-section col (+ sy chunk/section-offset)))

(defn- region-id [^SectionGrid rg ^long ix ^long iz]
  (chunk/pos->id (+ (rg-cx0 rg) ix) (+ (rg-cz0 rg) iz)))

(defn- col-index ^long [^SectionGrid rg ^long ix ^long iz]
  (+ (* ix (rg-ncz rg)) iz))

(defn- cell-index ^long [^SectionGrid rg ^long ix ^long iz ^long iy]
  (+ (* (col-index rg ix iz) (rg-nsy rg)) iy))

(defn- put-column [^SectionGrid rg ^long ix ^long iz col]
  (aset ^objects (rg-cols rg) (col-index rg ix iz) col)
  (when col
    (dotimes [iy (rg-nsy rg)]
      (aset ^objects (rg-grid rg) (cell-index rg ix iz iy)
            (section-at col (+ (rg-sy0 rg) iy))))))

(defn- fill-grid [^SectionGrid rg chunks]
  (dotimes [ix (rg-ncx rg)]
    (dotimes [iz (rg-ncz rg)]
      (put-column rg ix iz (get chunks (region-id rg ix iz))))))

(defn- empty-grid ^SectionGrid [[cx0 cz0 sy0 ncx ncz nsy] read-absent]
  (let [n (* (long ncx) (long ncz))]
    (SectionGrid. (object-array (* n (long nsy))) (object-array n)
                  (int cx0) (int cz0) (int sy0) (int ncx) (int ncz)
                  (int nsy) read-absent (atom {}))))

(defn block-reader
  "Returns a reader over the blocks around pos. The reader asks
  read-absent for each absent chunk it needs. The answer is the
  payload of that chunk, read or generated."
  (^SectionGrid [chunks pos] (block-reader chunks pos nil))
  (^SectionGrid [chunks pos read-absent]
   (let [rg (empty-grid (region-bounds pos) read-absent)]
     (fill-grid rg chunks)
     rg)))

(defn- summon [^SectionGrid rg ^long ix ^long iz]
  (let [id (region-id rg ix iz)
        payload ((rg-read rg) id)]
    (swap! (rg-loaded rg) assoc id payload)
    (put-column rg ix iz (:chunk payload))))

(defn- summoner [rg]
  (fn [ix iz] (summon rg ix iz)))

(defn read-block
  "Returns the block state at x y z in region rg, air outside it."
  ^long [^SectionGrid rg ^long x ^long y ^long z]
  (Rays/block rg (summoner rg) (unchecked-int x) (unchecked-int y)
              (unchecked-int z)))

(def ^:private ^:const ray-w Rays/W)

(defn- resistance [^long st ^doubles resist]
  (cond (block/air-type? st) Float/NaN
        (block/waterlogged? st) (max (aget resist st) 100.0)
        :else (aget resist st)))

(def ^:private ^:table resist-table
  (delay
    (let [resist (block/resist-arr)
          a (float-array (alength resist))]
      (dotimes [i (alength a)]
        (aset a i (float (resistance i resist))))
      a)))

(defn- hit-positions [^bytes hit origin ^long least]
  (let [ox (long (origin 0)) oy (long (origin 1))
        oz (long (origin 2))
        out (transient [])]
    (dotimes [ix ray-w]
      (dotimes [iy ray-w]
        (dotimes [iz ray-w]
          (when (>= (aget hit (+ (* (+ (* ix ray-w) iy) ray-w) iz))
                    least)
            (conj! out [(+ ox ix) (+ oy iy) (+ oz iz)])))))
    (persistent! out)))

(defn- ray-origin [^double cx ^double cy ^double cz]
  [(- (long (Math/floor cx)) region-r)
   (- (long (Math/floor cy)) region-r)
   (- (long (Math/floor cz)) region-r)])

(defn- ray-parts [^long n]
  (mapv (fn [^long i]
          [(quot (* i Rays/COUNT) n) (quot (* (inc i) Rays/COUNT) n)])
        (range n)))

(defn- caster [^SectionGrid rg ^Exposure e [cx cy cz] power seed]
  (let [cx (double cx) cy (double cy) cz (double cz)
        [ox oy oz] (ray-origin cx cy cz)
        ox (long ox) oy (long oy) oz (long oz)
        resist ^floats @resist-table
        power (double power) h (long (hash seed))]
    (fn [[from to]]
      (Rays/hit
        (Rays/cast e (summoner rg) resist ox oy oz cx cy cz power h
                   (int from) (int to))))))

(defn rays
  "Returns the cells of the cube at the centre that the rays of a
  blast of power reach through rg, 1 for air and 2 for a block. The
  cells come from e. The rays go at once in n parts, which needs e
  frozen when n is above 1."
  ^bytes [rg e center power seed n]
  (reduce #(Rays/union %1 %2)
          (par/pmapv (caster rg e center power seed) (ray-parts n)
                     1 1)))

(defn reached
  "Returns the cells of hit, as rays returns them for a blast at
  center, with the cells that hold a block and the count of all of
  them."
  [^bytes hit [cx cy cz]]
  (let [origin (ray-origin (double cx) (double cy) (double cz))]
    {:blocks (hit-positions hit origin 2) :count (Rays/hitCount hit)
     :cells (delay (hit-positions hit origin 1))}))

(defn exposure
  "Returns what a blast at center sees through rg. The bodies that
  one blast reaches share it."
  ^Exposure [^SectionGrid rg [cx cy cz]]
  (Exposure. rg (block/collision-arr) (phys/kinds) (double cx)
             (double cy) (double cz)))

(defn frozen?
  "Returns true when the reads of e and of its grid change neither,
  so that the rays and the bodies of one blast may read them at
  once."
  [^Exposure e]
  (Exposure/frozen e))

(defn affected-blocks
  "Returns what the rays of a blast of power at center through rg
  reach, as reached does."
  [^SectionGrid rg center power seed]
  (reached (rays rg (exposure rg center) center power seed 1)
           center))

(defn exposed
  "Returns the share, 0.0 to 1.0, of a body at p that the blast of e
  reaches without a block in the way. The body is a box of half width
  half and height height. The flags tell how it meets the blocks
  whose shape depends on the body."
  ([e p half height] (exposed e p half height 0))
  ([^Exposure e [px py pz] half height flags]
   (.density e (double px) (double py) (double pz)
             (double half) (double height) (int flags))))

(defn block-density
  "Returns what exposed returns for the blast at center through rg."
  ([rg center p half height]
   (block-density rg center p half height 0))
  ([^SectionGrid rg center p half height flags]
   (exposed (exposure rg center) p half height flags)))

(defn shuffled
  "Returns v in the order seed shuffles it into."
  [v seed]
  (random/shuffled #(random/below (random/of-key [seed :shuffle %]) %)
                   v))

(def ^:private ^:const merge-cap 16)

(defn- add-stack [cs pos item n]
  (let [mx (data/max-stack item)
        lim (min mx merge-cap)]
    (loop [i 0 n (long n) cs cs]
      (cond
        (zero? n) cs
        (>= i (count cs)) (conj cs [pos item n])
        :else
        (let [[p it c] (nth cs i)
              c (long c)]
          (if (and (= it item) (<= (+ c n) mx))
            (let [d (max 0 (min (- lim c) n))]
              (recur (inc i) (- n d) (assoc cs i [p it (+ c d)])))
            (recur (inc i) n cs)))))))

(defn- pos-stacks [cs st pos seed radius]
  (let [roll (fn [salt] (random/of-key [seed pos salt]))
        add (fn [acc s]
              (add-stack acc pos (:item s) (long (:count s 1))))]
    (reduce add cs (block/drops st roll radius))))

(defn- dropping? [^SectionGrid rg [x y z]]
  (let [st (read-block rg (long x) (long y) (long z))]
    (when (and (pos? st) (not (block/tnt? st))) st)))

(defn stacks
  "Returns what the blast leaves behind at each position. The drops
  of the destroyed positions merge into few stacks. Seed decides the
  random drops, and the blast radius lowers them."
  [^SectionGrid rg positions seed radius]
  (let [add (fn [cs pos]
              (if-let [st (dropping? rg pos)]
                (pos-stacks cs st pos seed radius)
                cs))]
    (mapv (fn [[pos item n]] [pos {:item item :count n}])
          (reduce add [] (shuffled positions seed)))))
