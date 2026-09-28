(ns collider.world.space.explosion
  "Explosions, the blocks they break, their drops and their
  reach into a body."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.world.space Rays Region)))

(set! *warn-on-reflection* true)

(def ^:private ^:const region-r 10)

(defn- rg-grid ^objects [^Region rg] (.grid rg))

(defn- rg-cols ^objects [^Region rg] (.cols rg))

(defn- rg-read [^Region rg] (.readAbsent rg))

(defn- rg-loaded ^clojure.lang.Atom [^Region rg] (.loaded rg))

(defn- rg-cx0 ^long [^Region rg] (.cx0 rg))

(defn- rg-cz0 ^long [^Region rg] (.cz0 rg))

(defn- rg-sy0 ^long [^Region rg] (.sy0 rg))

(defn- rg-ncx ^long [^Region rg] (.ncx rg))

(defn- rg-ncz ^long [^Region rg] (.ncz rg))

(defn- rg-nsy ^long [^Region rg] (.nsy rg))

(defn loaded-payloads
  "Returns the chunks that the reads of rg load, by id."
  [^Region rg]
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

(defn- region-id [^Region rg ^long ix ^long iz]
  (chunk/pos->id (+ (rg-cx0 rg) ix) (+ (rg-cz0 rg) iz)))

(defn- col-index ^long [^Region rg ^long ix ^long iz]
  (+ (* ix (rg-ncz rg)) iz))

(defn- cell-index ^long [^Region rg ^long ix ^long iz ^long iy]
  (+ (* (col-index rg ix iz) (rg-nsy rg)) iy))

(defn- put-column [^Region rg ^long ix ^long iz col]
  (aset ^objects (rg-cols rg) (col-index rg ix iz) col)
  (when col
    (dotimes [iy (rg-nsy rg)]
      (aset ^objects (rg-grid rg) (cell-index rg ix iz iy)
            (section-at col (+ (rg-sy0 rg) iy))))))

(defn- fill-grid [^Region rg chunks]
  (dotimes [ix (rg-ncx rg)]
    (dotimes [iz (rg-ncz rg)]
      (put-column rg ix iz (get chunks (region-id rg ix iz))))))

(defn block-reader
  "Returns a reader over the blocks around pos. The reader asks
  read-absent for each absent chunk it needs. The answer is the
  payload of that chunk, read or generated."
  (^Region [chunks pos] (block-reader chunks pos nil))
  (^Region [chunks pos read-absent]
   (let [[cx0 cz0 sy0 ncx ncz nsy] (region-bounds pos)
         n (* (long ncx) (long ncz))
         rg (Region. (object-array (* n (long nsy))) (object-array n)
                     (int cx0) (int cz0) (int sy0)
                     (int ncx) (int ncz) (int nsy) read-absent
                     (atom {}))]
     (fill-grid rg chunks)
     rg)))

(defn- summon [^Region rg ^long ix ^long iz]
  (let [id (region-id rg ix iz)
        payload ((rg-read rg) id)]
    (swap! (rg-loaded rg) assoc id payload)
    (put-column rg ix iz (:chunk payload))))

(defn- summoner [rg]
  (fn [ix iz] (summon rg ix iz)))

(defn read-block
  "Returns the block state at x y z in region rg, air outside it."
  ^long [^Region rg ^long x ^long y ^long z]
  (Rays/block rg (summoner rg) (unchecked-int x) (unchecked-int y)
              (unchecked-int z)))

(def ^:private ^:const ray-w 21)

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

(defn affected-blocks
  "Returns the cells that a blast of power at center reaches.
  :blocks holds the cells with a block. :count is the number of
  all reached cells. :cells is a delay of all reached cells."
  [^Region rg [cx cy cz] power seed]
  (let [cx (double cx) cy (double cy) cz (double cz)
        [ox oy oz :as origin] (ray-origin cx cy cz)
        hit (byte-array (* ray-w ray-w ray-w))]
    (Rays/cast rg (summoner rg) (block/resist-arr) hit (long ox)
               (long oy) (long oz) cx cy cz (double power)
               (long (hash seed)))
    {:blocks (hit-positions hit origin 2) :count (Rays/hitCount hit)
     :cells (delay (hit-positions hit origin 1))}))

(defn- density-form [rg center p half height]
  `(let [[cx# cy# cz#] ~center [px# py# pz#] ~p]
     (Rays/density ~(with-meta rg {:tag `Region}) (block/solid-arr)
                   (double cx#) (double cy#) (double cz#)
                   (double px#) (double py#) (double pz#)
                   (double ~half) (double ~height))))

(defn block-density
  "Returns the share, 0.0 to 1.0, of a body at p that the blast at
  the center reaches without a block in the way. The body is a box
  of half width half and height height."
  {:inline (fn [rg c p h t] (density-form rg c p h t))}
  [^Region rg [cx cy cz] [px py pz] half height]
  (Rays/density rg (block/solid-arr) (double cx) (double cy)
                (double cz) (double px) (double py) (double pz)
                (double half) (double height)))

(defn shuffled
  "Returns v in the order seed shuffles it into."
  [v seed]
  (let [^objects a (to-array v)]
    (loop [i (alength a)]
      (if (> i 1)
        (let [j (long (* i (random/of-key [seed :shuffle i])))
              t (aget a (dec i))]
          (aset a (dec i) (aget a j))
          (aset a j t)
          (recur (dec i)))
        (vec a)))))

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

(defn- dropping? [^Region rg [x y z]]
  (let [st (read-block rg (long x) (long y) (long z))]
    (when (and (pos? st) (not (block/tnt? st))) st)))

(defn stacks
  "Returns [pos stack] pairs of what the blast leaves behind. The
  drops of the destroyed positions merge into few stacks. seed
  decides the random drops. The drops depend on the blast radius."
  [^Region rg positions seed radius]
  (let [add (fn [cs pos]
              (if-let [st (dropping? rg pos)]
                (pos-stacks cs st pos seed radius)
                cs))]
    (mapv (fn [[pos item n]] [pos {:item item :count n}])
          (reduce add [] (shuffled positions seed)))))
