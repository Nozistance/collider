(ns collider.world.phys
  "Collision of moving bodies with the blocks of the world."
  (:require [collider.data :as data]
            [collider.data.state :as states]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider V3)
           (collider.world ChunkIndex Collision Move Phys YCoords)))

(set! *warn-on-reflection* true)

(defn joined?
  "Returns true when box a of a shape meets box b of a body."
  [a b]
  (let [axis (fn [^long i]
               (Collision/joins
                (double (nth a i)) (double (nth a (+ i 3)))
                (double (nth b i)) (double (nth b (+ i 3)))))]
    (and (axis 0) (axis 1) (axis 2))))

(defn- full-cube? [chunks x y z]
  (let [st (chunk/block-state chunks x y z)]
    (and (block/solid? st) (block/full-cube? st))))

(defn standing-on-cubes?
  "Returns true when whole blocks carry the body box at x y z."
  [chunks x y z half]
  (let [x (double x) y (double y) z (double z) half (double half)
        yb (dec (long y))
        x0 (long (Math/floor (- x half)))
        x1 (long (Math/floor (+ x half)))
        z0 (long (Math/floor (- z half)))
        z1 (long (Math/floor (+ z half)))]
    (and (== y (Math/floor y)) (pos? y)
         (full-cube? chunks x0 yb z0)
         (or (= x1 x0) (full-cube? chunks x1 yb z0))
         (or (= z1 z0) (full-cube? chunks x0 yb z1))
         (or (and (= x1 x0) (= z1 z0))
             (full-cube? chunks x1 yb z1)))))

(defn pos
  ^V3 [^Move m] (.pos m))

(defn vel
  ^V3 [^Move m] (.vel m))

(defn moved-y
  "Returns how far up a move took the body."
  ^double [^Move m] (.dy m))

(defn fallen
  "Returns fall distance fall after a move up by dy out of water."
  ^double [^double fall ^double dy]
  (if (< dy 0.0) (- fall (double (float dy))) fall))

(defn on-ground?
  {:inline (fn [m]
             (let [t 'collider.world.Move]
               `(.onGround ~(with-meta m {:tag t}))))}
  [^Move m]
  (.onGround m))

(defn- hanging? [^long st]
  (let [ps (block/props-of st)]
    (and (= :true (:bottom ps))
         (not= :0 (:distance ps :0)))))

(defn- shape-kind [^long st]
  (case (block/type-of st)
    :scaffolding (if (hanging? st)
                   Collision/SCAFFOLDING_HANGING
                   Collision/SCAFFOLDING)
    :powder-snow Collision/POWDER_SNOW
    :bamboo-stalk Collision/OFFSET_QUARTER
    (:pointed-dripstone :sulfur-spike) Collision/OFFSET_EIGHTH
    Collision/PLAIN))

(def ^:private ^:table kind-table
  (delay
    (let [a (byte-array (data/block-state-count))]
      (dotimes [i (alength a)] (aset a i (byte (shape-kind i))))
      a)))

(defn kinds
  "Returns the collision kind of every block state by state id. Only
  scaffolding, powder snow, bamboo, pointed dripstone and sulfur
  spikes differ from the plain kind."
  ^bytes [] @kind-table)

(defn- doubles-of [v] (when v (double-array v)))

(def ^:private ^:table y-coords-table
  (delay
    (let [t (states/collision-ys)]
      (YCoords. (object-array (map doubles-of (:states t)))
                (doubles-of (:block t))
                (doubles-of (:scaffolding-bottom t))
                (doubles-of (:powder-snow-falling t))))))

(defn y-coords
  ^YCoords [] @y-coords-table)

(def ^:private walker-tag "powder_snow_walkable_mobs")

(def ^:private ^:table walkers
  (delay (set (data/tag-values "entity_type" walker-tag))))

(def ^:private ^:const boots-slot 8)

(defn- walker? [e]
  (or (contains? @walkers (:type e))
      (= :leather-boots (:item (get (:inventory e) boots-slot)))))

(defn- descends? [e]
  (and (= :player (:type e)) (:sneaking? e)))

(defn context
  "Returns the collision flags of body e."
  ^long [e]
  (cond-> 0
    (descends? e) (bit-or Collision/DESCENDING)
    (> (double (or (:fall e) 0.0)) 2.5) (bit-or Collision/FALLING)
    (walker? e) (bit-or Collision/WALKER)
    (= :falling-block (:type e)) (bit-or Collision/FALLING_BLOCK)))

(defn- with-tables [m chunks args]
  (let [c (with-meta (gensym "chunks") {:tag `ChunkIndex})]
    `(let [~c ~chunks]
       (~m ~c (kinds) (block/cube-arr) (block/collision-arr)
           ~@args))))

(defn- at-form [m chunks pos args]
  (let [p (gensym "pos")]
    `(let [~p ~pos]
       ~(with-tables m chunks
          (into [`(v/x ~p) `(v/y ~p) `(v/z ~p)] args)))))

(defn- free-form [chunks pos half height dx dy dz ctx]
  (let [p (gensym "pos")]
    `(let [~p ~pos]
       ~(with-tables `Phys/free chunks
          [`(+ (v/x ~p) (double ~dx)) `(+ (v/y ~p) (double ~dy))
           `(+ (v/z ~p) (double ~dz)) `(double ~half)
           `(double ~height) `(v/y ~p) `(int ~ctx)]))))

(defmacro ^:private inlined [form & args]
  (apply @(resolve form) args))

(defn dry?
  "Returns true when no water or lava touches a body of that size
  at pos."
  [chunks pos half height]
  (Phys/dry chunks (block/tables) (v/x pos) (v/y pos) (v/z pos)
            (double half) (double height)))

(defn burns
  "Returns the marks in bits of the cells that span outer touches,
  with Phys/LAVA_INSIDE set when lava lies in span inner. Each span is
  x0 x1 y0 y1 z0 z1 with the ends left out."
  ^long [chunks ^bytes bits ^longs outer ^longs inner]
  (Phys/burns chunks bits outer inner))

(defn some-cell?
  "Returns true when a cell from x0 y0 z0 up to x1 y1 z1, the high
  ends left out, holds a state that marks is true for."
  {:inline (fn [c m x0 y0 z0 x1 y1 z1]
             `(Phys/someCell
                ~c ~m (long ~x0) (long ~y0) (long ~z0)
                (long ~x1) (long ~y1) (long ~z1)))}
  [chunks ^booleans marks x0 y0 z0 x1 y1 z1]
  (Phys/someCell chunks marks (long x0) (long y0) (long z0)
                 (long x1) (long y1) (long z1)))

(defn cool?
  "Returns true when no block of bits lies within a block of a body
  of that size at pos."
  [chunks ^bytes bits p half height]
  (Phys/cool chunks bits (v/x p) (v/y p) (v/z p) half height))

(defn free?
  "Returns true when a body of that size meets no block after a
  move by dx dy dz. Only the end position counts, not the path.
  ctx is the context of the body at pos."
  {:inline (fn [c p h t dx dy dz & [x]]
             (free-form c p h t dx dy dz (or x 0)))
   :inline-arities #{7 8}}
  ([chunks pos half height dx dy dz]
   (free? chunks pos half height dx dy dz 0))
  ([chunks pos half height dx dy dz ctx]
   (inlined free-form chunks pos half height dx dy dz ctx)))

(defn- clear-form [chunks pos half height inset]
  (let [p (gensym "pos")]
    `(let [~p ~pos]
       ~(with-tables `Phys/clear chunks
          [`(v/x ~p) `(v/y ~p) `(v/z ~p) `(double ~half)
           `(double ~height) `(double ~inset) `(v/y ~p) `(int 0)]))))

(defn clear?
  "Returns true when a body of that size at pos, its box shrunk by
  inset on each side, meets no block."
  {:inline (fn [c p h t i] (clear-form c p h t i))}
  [chunks pos half height inset]
  (inlined clear-form chunks pos half height inset))

(defn box-free?
  "Returns true when the box [x0 y0 z0 x1 y1 z1] meets no block. The
  body of the box has its foot at bottom and its context in ctx."
  ([chunks box bottom] (box-free? chunks box bottom 0))
  ([chunks box bottom ctx]
   (Phys/freeBox chunks (kinds) (block/cube-arr)
                 (block/collision-arr) (double-array box)
                 (double bottom) (int ctx))))

(defn- support-form [chunks pos half ctx]
  (let [c (gensym "cell")
        call (at-form `Phys/support chunks pos
                      [`(double ~half) `(int ~ctx)])]
    `(when-let [~c ~call]
       [(aget ~c 0) (aget ~c 1) (aget ~c 2)])))

(defn supporting-block
  "Returns the block a box of that half width standing at pos rests
  on, nil when it rests on nothing. The nearest block centre wins,
  so an edge of the box carries the whole body."
  {:inline (fn [c p h & [x]] (support-form c p h (or x 0)))
   :inline-arities #{3 4}}
  ([chunks pos half] (supporting-block chunks pos half 0))
  ([chunks pos half ctx]
   (inlined support-form chunks pos half ctx)))

(defn- move-form [chunks pos vel half height step ground? ctx]
  (let [v (gensym "vel")]
    `(let [~v ~vel]
       ~(at-form `Phys/move chunks pos
          [`(v/x ~v) `(v/y ~v) `(v/z ~v) `(double ~half)
           `(double ~height) `(double ~step) `(boolean ~ground?)
           `(int ~ctx) `(y-coords)]))))

(defn- move-rest [[a b c :as more]]
  (case (count more)
    0 [0.0 false 0]
    1 [a false 0]
    2 [a false b]
    [a b c]))

(defn- moved ^Move [chunks pos vel half height step ground? ctx]
  (inlined move-form chunks pos vel half height step ground? ctx))

(defn move
  "Returns the end of a move of a body by vel from pos. Blocks stop
  the body, and it climbs up to step when held back sideways."
  {:inline (fn [c p v h t & more]
             (apply move-form c p v h t (move-rest (vec more))))
   :inline-arities #{5 6 7 8}}
  ([chunks pos vel half height]
   (moved chunks pos vel half height 0.0 false 0))
  ([chunks pos vel half height step]
   (moved chunks pos vel half height step false 0))
  ([chunks pos vel half height step ctx]
   (moved chunks pos vel half height step false ctx))
  ([chunks pos vel half height step ground? ctx]
   (moved chunks pos vel half height step ground? ctx)))

(defn- overlaps? [[a b c d e f] [p q r s u w]]
  (and (< a s) (< p d) (< b u) (< q e) (< c w) (< r f)))

(defn- cells [lo hi]
  (let [cell #(long (Math/floor (double %)))]
    (range (dec (cell (- lo 1e-7))) (+ 2 (cell (+ hi 1e-7))))))

(defn- shape-at [chunks x y z bottom ctx]
  (let [bs (Collision/shape (block/collision-arr) (kinds)
                           (long (chunk/at chunks [x y z]))
                           (int x) (int y) (int z) (double bottom)
                           (int ctx))]
    (mapv (fn [[a b c d e f]]
            [(+ x a) (+ y b) (+ z c) (+ x d) (+ y e) (+ z f)])
          (partition 6 (or bs [])))))

(defn- block-boxes
  "Returns the boxes of the blocks that meet area, each block whole,
  as a body with its foot at bottom and context ctx meets them."
  [chunks [x0 y0 z0 x1 y1 z1 :as area] bottom ctx]
  (for [x (cells x0 x1) y (cells y0 y1) z (cells z0 z1)
        :let [boxes (shape-at chunks x y z bottom ctx)]
        :when (some #(overlaps? % area) boxes)
        b boxes]
    b))

(defn- merged
  "Returns the sorted values vs, each one within 1e-7 of the one
  before left out, as voxel shapes merge their coordinates."
  [vs]
  (reduce (fn [acc v]
            (if (and (seq acc)
                     (>= (double (peek acc)) (- (double v) 1e-7)))
              acc
              (conj acc v)))
          [] (sort vs)))

(defn- spans [lo hi solids i j]
  (->> (concat [lo hi] (map #(% i) solids) (map #(% j) solids))
       (filter #(<= lo % hi))
       merged
       (partition 2 1)))

(defn- dist2 ^double [p q]
  (reduce + (map #(let [d (- (double %1) (double %2))] (* d d)) p q)))

(defn- nearest [p boxes]
  (let [clamped (fn [[a b c d e f]]
                  (mapv #(max %2 (min %3 %1)) p [a b c] [d e f]))]
    (reduce (fn [best q]
              (if (or (nil? best) (< (dist2 p q) (dist2 p best)))
                q
                best))
            nil (map clamped boxes))))

(defn- grown [sizes [a b c d e f]]
  (let [[hx hy hz] (map #(/ (double %) 2.0) sizes)]
    [(- a hx) (- b hy) (- c hz) (+ d hx) (+ e hy) (+ f hz)]))

(defn- inside? [solids x y z]
  (some (fn [[a b c d e f]] (and (< a x d) (< b y e) (< c z f)))
        solids))

(defn- mid ^double [a b] (* 0.5 (+ (double a) (double b))))

(defn free-position
  "Returns the point nearest centre in the box allowed where the
  centre of a body of size sx sy sz meets no block, or nil when there
  is none. The body has its foot at bottom and its context in ctx."
  [chunks [ax0 ay0 az0 ax1 ay1 az1] centre sx sy sz bottom ctx]
  (let [area [(- ax0 sx) (- ay0 sy) (- az0 sz)
              (+ ax1 sx) (+ ay1 sy) (+ az1 sz)]
        solids (mapv #(grown [sx sy sz] %)
                     (block-boxes chunks area bottom ctx))]
    (nearest centre
             (for [[x0 x1] (spans ax0 ax1 solids 0 3)
                   [y0 y1] (spans ay0 ay1 solids 1 4)
                   [z0 z1] (spans az0 az1 solids 2 5)
                   :when (not (inside? solids (mid x0 x1) (mid y0 y1)
                                       (mid z0 z1)))]
               [x0 y0 z0 x1 y1 z1]))))
