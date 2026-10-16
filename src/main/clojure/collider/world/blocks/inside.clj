(ns collider.world.blocks.inside
  "The blocks and fluids a body meets on its moves of a tick."
  (:require [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.cauldron :as cauldron]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]
            [collider.world.space.crossed :as crossed]))

(set! *warn-on-reflection* true)

(def ^:private shrink (double (float 1.0E-5)))

(def ^:private ^:const max-steps 16)

(def ^:private ^:const slack 1.0E-7)

(defn- state-at ^long [chunks ^long x ^long y ^long z]
  (if (chunk/in-range? y) (chunk/block-state chunks x y z) 0))

(def ^:private ^:table airy
  (delay (block/state-table :boolean block/air-type?)))

(defn- box-at [[x y z] half height ^double in]
  (let [x (double x) y (double y) z (double z)
        half (double half) height (double height)]
    [(+ (- x half) in) (+ y in) (+ (- z half) in)
     (- (+ x half) in) (- (+ y height) in) (- (+ z half) in)]))

(defn- contains-point? [[x0 y0 z0 x1 y1 z1] [x y z]]
  (and (>= (double x) (double x0)) (< (double x) (double x1))
       (>= (double y) (double y0)) (< (double y) (double y1))
       (>= (double z) (double z0)) (< (double z) (double z1))))

(defn- touches?
  "Returns true when the box of a body passing from point from to
  point to meets box b."
  [[b0 b1 b2 b3 b4 b5] from to half height]
  (let [[a0 a1 a2 a3 a4 a5] (box-at from half height 0.0)
        hx (* (- (double a3) (double a0)) 0.5)
        hy (* (- (double a4) (double a1)) 0.5)
        hz (* (- (double a5) (double a2)) 0.5)
        sx (- hx slack) sy (- hy slack) sz (- hz slack)
        grown [(- (double b0) sx) (- (double b1) sy) (- (double b2) sz)
               (+ (double b3) sx) (+ (double b4) sy) (+ (double b5) sz)]
        mid [(+ (double a0) hx) (+ (double a1) hy) (+ (double a2) hz)]
        end (mapv + mid (mapv - to from))]
    (boolean (or (contains-point? grown end) (contains-point? grown mid)
                 (crossed/box-clip grown mid end)))))

(defn- in-fluid?
  "Returns true when the box of a body passing from point from to
  point to meets fluid cls of cell c."
  [chunks cls [x y z :as c] from to half height]
  (let [x (double x) z (double z)]
    (touches? [x (double y) z (+ x 1.0) (liquid/surface chunks cls c)
               (+ z 1.0)]
              from to half height)))

(defn- in-block?
  "Returns true when the box of a body passing from point from to
  point to is inside state st of cell c."
  [st [x y z] from to half height]
  (if-let [bs (cauldron/inside-boxes st)]
    (some #(touches? (mapv (fn [c b] (+ (long c) (/ (double b) 16.0)))
                           [x y z x y z] %)
                     from to half height)
          bs)
    true))

(defn- walked
  "Returns acc after the box of a body passes from point from to point
  to, meeting no more than steps cells, and the steps it took. Each
  block whose inside or fluid the box touches joins acc once a tick,
  with the step it met it on and the fluid it was in there, nil for
  none. A block crossed untouched may join on a later move."
  [chunks acc from to half height steps]
  (let [cs (take-while #(< (long (nth % 3)) (long steps))
                       (crossed/cells from to
                                      (box-at to half height shrink)))
        ^booleans air @airy
        acc (reduce
              (fn [{:keys [seen] :as acc} [x y z s]]
                (let [st (state-at chunks x y z) k [x y z]]
                  (if (or (aget air st) (contains? seen k))
                    acc
                    (let [cls (block/liquid-class st)
                          wet (when (and cls (in-fluid? chunks cls k from
                                                        to half height))
                                cls)]
                      (if (or wet (in-block? st k from to half height))
                        {:seen (conj seen k)
                         :met (conj (:met acc) [x y z st s wet])}
                        acc)))))
              acc cs)]
    [acc (inc (long (if-let [c (last cs)] (nth c 3) 0)))]))

(defn- walked-move
  "Returns acc after one move [from to d] of a body, cut along the
  axes of d, the move it asked for, when it has one."
  [chunks acc [from to d] half height]
  (let [delta (mapv - to from)
        axes (if (< (Math/abs (double (nth d 0 0.0)))
                    (Math/abs (double (nth d 2 0.0))))
               [1 2 0]
               [1 0 2])
        [acc left]
        (if (and d (pos? (double (reduce + (map * delta delta)))))
          (reduce (fn [[acc left p] i]
                    (let [a (double (delta i))]
                      (if (zero? a)
                        [acc left p]
                        (let [q (update p i + a)
                              [acc n] (walked chunks acc p q half height
                                              left)]
                          [acc (- (long left) (long n)) q]))))
                  [acc max-steps (vec from)] axes)
          (let [[acc n] (walked chunks acc from to half height max-steps)]
            [acc (- max-steps (long n))]))]
    (if (<= (long left) 0)
      (first (walked chunks acc to to half height 1))
      acc)))

(defn- near?
  "Returns true when a state that marks holds lies in the cells the
  box of a body covers on any of moves, at either end of each."
  [chunks ^booleans marks moves half height]
  (let [half (double half) height (double height)]
    (loop [ps (mapcat #(subvec (vec %) 0 2) moves)
           x0 Double/MAX_VALUE y0 Double/MAX_VALUE z0 Double/MAX_VALUE
           x1 (- Double/MAX_VALUE) y1 (- Double/MAX_VALUE)
           z1 (- Double/MAX_VALUE)]
      (if-let [p (first ps)]
        (let [x (double (v/x p)) y (double (v/y p)) z (double (v/z p))]
          (recur (rest ps) (Math/min x0 x) (Math/min y0 y) (Math/min z0 z)
                 (Math/max x1 x) (Math/max y1 y) (Math/max z1 z)))
        (phys/some-cell? chunks marks
                         (num/floor (- x0 half)) (num/floor y0)
                         (num/floor (- z0 half))
                         (inc (num/floor (+ x1 half)))
                         (inc (num/floor (+ y1 height)))
                         (inc (num/floor (+ z1 half))))))))

(defn met
  "Returns the blocks a body of half width half and height met on its
  moves this tick, in the order it met them, as [x y z st step fluid].
  Each move is [from to d], d the move it asked for or nil. A block
  counts once a tick, and fluid is the fluid it was in there, nil for
  none. Empty when no state that marks holds lies near the moves."
  [chunks moves half height marks]
  (if (near? chunks marks moves half height)
    (:met (reduce #(walked-move chunks %1 %2 half height)
                  {:seen #{} :met []} moves))
    []))
