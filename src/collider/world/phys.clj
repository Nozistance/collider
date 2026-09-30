(ns collider.world.phys
  "Collision of moving bodies with the blocks of the world."
  (:require [collider.data :as data]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider V3)
           (collider.world ChunkIndex Collision Move Phys)))

(set! *warn-on-reflection* true)

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

(defn fence-at?
  "Returns true when a fence or a fence gate stands at x y z."
  [chunks x y z]
  (let [st (chunk/block-state chunks x y z)]
    (or (block/fence? st) (= :gate (block/shape-of st)))))

(defn solid?
  "Returns true when x y z stops a walking body. Nothing outside
  the world height does."
  [chunks x y z]
  (let [y (long y)]
    (and (chunk/in-range? y)
         (or (block/solid? (chunk/block-state chunks x y z))
             (and (> y chunk/min-y)
                  (fence-at? chunks x (dec y) z))))))

(defn pos
  "Returns the position a move ends at."
  ^V3 [^Move m] (.pos m))

(defn vel
  "Returns the velocity a move leaves the body with."
  ^V3 [^Move m] (.vel m))

(defn on-ground?
  "Returns true when a move ends on the ground."
  {:inline (fn [m]
             (let [t 'collider.world.Move]
               `(.onGround ~(with-meta m {:tag t}))))}
  [^Move m]
  (.onGround m))

(defn- hanging? [^long st]
  (let [ps (block/props-of st)]
    (and (= "true" (name (get ps :bottom :false)))
         (not= "0" (name (get ps :distance :0))))))

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
  "Returns the Collision kind of each block state."
  ^bytes [] @kind-table)

(def ^:private walker-tag "powder_snow_walkable_mobs")

(def ^:private ^:table walkers
  (delay (set (data/tag-values "entity_type" walker-tag))))

(defn- walker? [e]
  (or (contains? @walkers (:type e))
      (= :leather-boots (:item (get (:inventory e) 8)))))

(defn- descends? [e]
  (and (= :player (:type e)) (:sneaking? e)))

(defn context
  "Returns the flags of body e as Collision.shape takes them."
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

(defn dry?
  "Returns true when no water or lava touches a body of that size
  at pos, as the fluid scan of liquid/fluid-info sees it."
  [chunks pos half height]
  (Phys/dry chunks (block/tables) (v/x pos) (v/y pos) (v/z pos)
            (double half) (double height)))

(defn burns
  "Returns the bits of bits of each cell the span outer touches, and
  bit 4 when a cell of the span inner holds bit 2. Each span is x0
  x1 y0 y1 z0 z1 with the ends left out."
  ^long [chunks ^bytes bits ^longs outer ^longs inner]
  (Phys/burns chunks bits outer inner))

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
   (Phys/free chunks (kinds) (block/cube-arr) (block/collision-arr)
              (+ (v/x pos) (double dx)) (+ (v/y pos) (double dy))
              (+ (v/z pos) (double dz)) (double half) (double height)
              (v/y pos) (int ctx))))

(defn box-free?
  "Returns true when the box [x0 y0 z0 x1 y1 z1] meets no block.
  bottom is the foot of the body the box belongs to."
  [chunks box bottom]
  (Phys/freeBox chunks (kinds) (block/cube-arr) (block/collision-arr)
                (double-array box) (double bottom) 0))

(defn- support-form [chunks pos half ctx]
  (let [c (gensym "cell")
        call (at-form `Phys/support chunks pos
                      [`(double ~half) `(int ~ctx)])]
    `(when-let [~c ~call]
       [(aget ~c 0) (aget ~c 1) (aget ~c 2)])))

(defn- support-cell ^longs [chunks pos half ctx]
  (Phys/support chunks (kinds) (block/cube-arr)
                (block/collision-arr) (v/x pos) (v/y pos) (v/z pos)
                (double half) (int ctx)))

(defn supporting-block
  "Returns the block a box of that half width standing at pos rests
  on, nil when it rests on nothing. The nearest block centre wins,
  so an edge of the box carries the whole body."
  {:inline (fn [c p h & [x]] (support-form c p h (or x 0)))
   :inline-arities #{3 4}}
  ([chunks pos half] (supporting-block chunks pos half 0))
  ([chunks pos half ctx]
   (when-let [c (support-cell chunks pos half ctx)]
     [(aget c 0) (aget c 1) (aget c 2)])))

(defn- move-form [chunks pos vel half height step ctx]
  (let [v (gensym "vel")]
    `(let [~v ~vel]
       ~(at-form `Phys/move chunks pos
          [`(v/x ~v) `(v/y ~v) `(v/z ~v) `(double ~half)
           `(double ~height) `(double ~step) `(int ~ctx)]))))

(defn- moved ^Move [chunks pos vel half height step ctx]
  (Phys/move chunks (kinds) (block/cube-arr) (block/collision-arr)
             (v/x pos) (v/y pos) (v/z pos) (v/x vel) (v/y vel)
             (v/z vel) (double half) (double height) (double step)
             (int ctx)))

(defn move
  "Returns the position, velocity and ground flag of a body.
  The body moves by vel from pos and the blocks it meets stop
  it. The body is a box of half width half and height height.
  step is how high it climbs without jumping. ctx is the context
  of the body."
  {:inline (fn [c p v h t & [s x]]
             (move-form c p v h t (or s 0.0) (or x 0)))
   :inline-arities #{5 6 7}}
  ([chunks pos vel half height]
   (moved chunks pos vel half height 0.0 0))
  ([chunks pos vel half height step]
   (moved chunks pos vel half height step 0))
  ([chunks pos vel half height step ctx]
   (moved chunks pos vel half height step ctx)))
