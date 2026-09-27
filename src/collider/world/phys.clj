(ns collider.world.phys
  "Collision of moving bodies with the blocks of the world."
  (:require [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.java ChunkIndex Move Phys V3)))

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
             (let [t 'collider.java.Move]
               `(.onGround ~(with-meta m {:tag t}))))}
  [^Move m]
  (.onGround m))

(defn- with-tables
  "Returns the call of the Phys method m on chunks, the block
  tables and args."
  [m chunks args]
  (let [c (with-meta (gensym "chunks") {:tag `ChunkIndex})]
    `(let [~c ~chunks]
       (~m ~c (block/solid-arr) (block/cube-arr)
           (block/collision-arr) ~@args))))

(defn- at-form
  "Returns the call of the Phys method m on chunks, the tables, the
  coordinates of pos and args."
  [m chunks pos args]
  (let [p (gensym "pos")]
    `(let [~p ~pos]
       ~(with-tables m chunks
          (into [`(v/x ~p) `(v/y ~p) `(v/z ~p)] args)))))

(defn- free-form [chunks pos half height dx dy dz]
  (let [p (gensym "pos")]
    `(let [~p ~pos]
       ~(with-tables `Phys/free chunks
          [`(+ (v/x ~p) (double ~dx)) `(+ (v/y ~p) (double ~dy))
           `(+ (v/z ~p) (double ~dz)) `(double ~half)
           `(double ~height)]))))

(defn free?
  "Returns true when a body of that size meets no block after a
  move by dx dy dz. Only the end position counts, not the path."
  {:inline (fn [c p h t dx dy dz]
             (free-form c p h t dx dy dz))}
  [chunks pos half height dx dy dz]
  (Phys/free chunks (block/solid-arr) (block/cube-arr)
             (block/collision-arr) (+ (v/x pos) (double dx))
             (+ (v/y pos) (double dy)) (+ (v/z pos) (double dz))
             (double half) (double height)))

(defn- support-form [chunks pos half]
  (let [c (gensym "cell")
        call (at-form `Phys/support chunks pos [`(double ~half)])]
    `(when-let [~c ~call]
       [(aget ~c 0) (aget ~c 1) (aget ~c 2)])))

(defn supporting-block
  "Returns the block a box of that half width standing at pos rests
  on, nil when it rests on nothing. The nearest block centre wins,
  so an edge of the box carries the whole body."
  {:inline (fn [c p h] (support-form c p h))}
  [chunks pos half]
  (let [s (block/solid-arr) k (block/cube-arr)
        b (block/collision-arr) x (v/x pos) y (v/y pos) z (v/z pos)]
    (when-let [c (Phys/support chunks s k b x y z (double half))]
      [(aget c 0) (aget c 1) (aget c 2)])))

(defn- move-form [chunks pos vel half height step]
  (let [v (gensym "vel")]
    `(let [~v ~vel]
       ~(at-form `Phys/move chunks pos
          [`(v/x ~v) `(v/y ~v) `(v/z ~v) `(double ~half)
           `(double ~height) `(double ~step)]))))

(defn move
  "Returns the position, velocity and ground flag of a body.
  The body moves by vel from pos and the blocks it meets stop
  it. The body is a box of half width half and height height.
  step is how high it climbs without jumping."
  {:inline (fn [c p v h t & [s]]
             (move-form c p v h t (or s 0.0)))
   :inline-arities #{5 6}}
  ([chunks pos vel half height]
   (move chunks pos vel half height 0.0))
  ([chunks pos vel half height step]
   (Phys/move chunks (block/solid-arr) (block/cube-arr)
              (block/collision-arr) (v/x pos) (v/y pos) (v/z pos)
              (v/x vel) (v/y vel) (v/z vel) (double half)
              (double height) (double step))))
