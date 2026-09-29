(ns collider.world.space.explosion
  "Explosions, the blocks they break, their drops and their
  reach into a body."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.world.space Craters Exposure Rays Region)))

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

(def ^:private air-names #{:air :cave-air :void-air})

(defn- resistance [^long st ^doubles resist]
  (cond (contains? air-names (block/name-of st)) Float/NaN
        (block/waterlogged? st) (max (aget resist st) 100.0)
        :else (aget resist st)))

(def ^:private ^:table resist-table
  (delay
    (let [resist (block/resist-arr)
          a (float-array (alength resist))]
      (dotimes [i (alength a)]
        (aset a i (float (resistance i resist))))
      a)))

(defn craters
  "Returns an empty record of the cells the blasts of a tick change."
  ^Craters []
  (Craters.))

(defn crater!
  "Notes in c that the cell at x y z now holds st."
  [^Craters c x y z st]
  (Craters/note c (int x) (int y) (int z) (int st)))

(defn read-now
  "Returns the block state at x y z in region rg as the earlier
  blasts of the tick in c left it."
  [^Region rg ^Craters c x y z]
  (let [s (Craters/now c (int x) (int y) (int z))]
    (if (neg? s) (read-block rg (long x) (long y) (long z)) s)))

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

(defn rays
  "Returns the rays of a blast of power at center through rg as the
  blocks stand before the blasts of the tick."
  ^Rays [^Region rg [cx cy cz] power seed]
  (let [cx (double cx) cy (double cy) cz (double cz)
        [ox oy oz] (ray-origin cx cy cz)]
    (Rays/cast rg (summoner rg) ^floats @resist-table (long ox)
               (long oy) (long oz) cx cy cz (double power)
               (long (hash seed)))))

(defn reached
  "Returns the cells that the rays rs of a blast at center reach once
  the earlier blasts of the tick left the cells of c. The result
  holds the cells with a block and the count of all reached cells."
  [^Rays rs ^Craters c [cx cy cz]]
  (let [origin (ray-origin (double cx) (double cy) (double cz))
        hit (Rays/hit rs c)]
    {:blocks (hit-positions hit origin 2) :count (Rays/hitCount hit)
     :cells (delay (hit-positions hit origin 1))}))

(defn affected-blocks
  "Returns the cells that a blast of power at center reaches.
  The result holds the cells with a block and the count of all
  reached cells."
  [^Region rg center power seed]
  (reached (rays rg center power seed) (craters) center))

(defn- hanging? [^long st]
  (let [ps (block/props-of st)]
    (and (= "true" (name (get ps :bottom :false)))
         (not= "0" (name (get ps :distance :0))))))

(defn- shape-kind [^long st]
  (case (block/type-of st)
    :scaffolding (if (hanging? st)
                   Exposure/SCAFFOLDING_HANGING
                   Exposure/SCAFFOLDING)
    :powder-snow Exposure/POWDER_SNOW
    :bamboo-stalk Exposure/OFFSET_QUARTER
    (:pointed-dripstone :sulfur-spike) Exposure/OFFSET_EIGHTH
    Exposure/PLAIN))

(def ^:private ^:table kind-table
  (delay
    (let [a (byte-array (data/block-state-count))]
      (dotimes [i (alength a)] (aset a i (byte (shape-kind i))))
      a)))

(defn exposure
  "Returns what a blast at center sees through rg. The bodies that
  one blast reaches share it."
  ^Exposure [^Region rg [cx cy cz]]
  (Exposure/of rg (block/collision-arr) ^bytes @kind-table (double cx)
               (double cy) (double cz)))

(defn stale?
  "Returns true when the cells of c may change what a body at p sees
  of the blast of e. The body is a box of half width half and height
  height."
  [^Exposure e ^Craters c [px py pz] half height]
  (Exposure/stale e c (double px) (double py) (double pz)
                  (double half) (double height)))

(defn look
  "Returns what each sample point of a body at p sees of the blast of
  e before the blasts of the tick. The body is a box of half width
  half and height height; flags tell how it meets the blocks whose
  shape depends on the body."
  ^longs [^Exposure e [px py pz] half height flags]
  (Exposure/look e (double px) (double py) (double pz) (double half)
                 (double height) (int flags)))

(defn look-share
  "Returns the share, 0.0 to 1.0, of the sample points in look that
  see the blast."
  ^double [^longs look]
  (Double/longBitsToDouble (aget look 0)))

(defn exposed
  "Returns the share, 0.0 to 1.0, of a body at p that the blast of e
  at the center reaches without a block in the way. The body is a box
  of half width half and height height; flags tell how it meets the
  blocks whose shape depends on the body."
  ([e center p half height] (exposed e center p half height 0))
  ([^Exposure e _center [px py pz] half height flags]
   (Exposure/density e nil (double px) (double py) (double pz)
                     (double half) (double height) (int flags))))

(defn exposed-now
  "Returns what exposed does once the earlier blasts of the tick left
  the cells of c. look is what the body saw before them, or nil."
  [^Exposure e ^Craters c look [px py pz] half height flags]
  (Exposure/densityNow e c look (double px) (double py) (double pz)
                       (double half) (double height) (int flags)))

(defn block-density
  "Returns the share, 0.0 to 1.0, of a body at p that the blast at
  the center reaches through rg without a block in the way. The body
  is a box of half width half and height height; flags tell how it
  meets the blocks whose shape depends on the body."
  ([rg center p half height]
   (block-density rg center p half height 0))
  ([^Region rg center p half height flags]
   (exposed (exposure rg center) center p half height flags)))

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
