(ns collider.game.mob.randompos
  "Where a mob picks a place to walk to, and what turns a cell down."
  (:require [collider.game.mob.mobs :as mobs]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.weather :as weather]
            [collider.world.space.path :as path]))

(set! *warn-on-reflection* true)

(def ^:private ^:const attempts 10)

(def ^:private ^:const ground-weight 10.0)

(defn rnd
  "Returns a number from 0 to 1 decided by tick t, mob eid and key k."
  (^double [t eid k] (random/of-longs (long t) (long eid) (hash k)))
  (^double [t eid k i]
   (random/of-longs (long t) (long eid) (hash k) (long i))))

(defn one-in?
  "Returns true with a chance of one in n."
  [t eid k ^long n]
  (zero? (long (* n (rnd t eid k)))))

(defn- state-at ^long [chunks [x y z]]
  (chunk/block-state chunks (long x) (long y) (long z)))

(defn- state-below ^long [chunks [x y z]]
  (chunk/block-state chunks (long x) (dec (long y)) (long z)))

(defn solid?
  "Tests whether the block filling the cell is solid."
  [chunks cell]
  (block/solid? (state-at chunks cell)))

(defn water?
  "Tests whether water stands in the cell."
  [chunks cell]
  (block/water? (state-at chunks cell)))

(defn outside-limits?
  "Tests whether the cell left the world."
  [[_ y _]]
  (not (chunk/in-range? (long y))))

(defn not-stable?
  "Tests whether nothing solid holds the cell up."
  [chunks cell]
  (not (block/solid-render? (state-below chunks cell))))

(defn has-malus?
  "Tests whether the cell costs the mob anything.
  Any malus but a plain zero turns the cell down, so water, honey
  and the neighbourhood of lava are never strolled to."
  [chunks [x y z]]
  (not (zero? (path/path-type-malus
                path/cow (path/type-static chunks x y z)))))

(defn direction
  "Returns an offset up to h blocks away and v up or down.
  The x, the y and the z are drawn in that order."
  [t eid k i h v]
  (let [draw (fn [part n]
               (let [n (long n) span (inc (* 2 n))]
                 (- (long (* span (rnd t eid [k part] i))) n)))]
    [(draw :x h) (draw :y v) (draw :z h)]))

(defn pos-toward-direction
  "Returns the cell the offset lands on from where the mob stands."
  [pos [dx dy dz]]
  [(long (Math/floor (+ (v/x pos) (double dx))))
   (long (Math/floor (+ (v/y pos) (double dy))))
   (long (Math/floor (+ (v/z pos) (double dz))))])

(defn move-up-out-of-solid
  "Returns the first cell at or above this one that is not solid."
  [chunks [x y z :as cell]]
  (if-not (solid? chunks cell)
    cell
    (loop [cy (inc (long y))]
      (if (and (<= cy chunk/max-y) (solid? chunks [x cy z]))
        (recur (inc cy))
        [x cy z]))))

(defn- light-cost
  "Returns what the light of the cell adds to its weight.
  The ambient light of the overworld is zero."
  ^double [world chunks [x y z]]
  (let [day (:time-of-day world 0)
        lit (weather/brightness world chunks x y z day)
        b (float (/ (float lit) (float 15.0)))
        denom (float (- (float 4.0) (float (* (float 3.0) b))))
        curved (float (/ b denom))]
    (double (float (- curved (float 0.5))))))

(defn walk-target-value
  "Returns the weight of a cell: ten on the ground the breed grazes,
  else the light of the cell less a half."
  ^double [world e cell]
  (let [chunks (:chunks world)
        ground (get-in mobs/types [(:type e) :ground] :grass-block)]
    (if (= ground (block/block-of (state-below chunks cell)))
      ground-weight
      (light-cost world chunks cell))))

(defn- bottom-centre [[x y z]]
  [(+ (double (long x)) 0.5) (double (long y))
   (+ (double (long z)) 0.5)])

(defn- best-pos
  "Returns the bottom centre of the heaviest of ten tried cells.
  The first of equals keeps it, and nil comes back when no try
  yielded a cell."
  [weight-of supply]
  (loop [i 0 best nil bw Double/NEGATIVE_INFINITY]
    (if (= i attempts)
      (when best (bottom-centre best))
      (let [cell (supply i)
            w (if cell (double (weight-of cell)) 0.0)]
        (if (and cell (> w bw))
          (recur (inc i) cell w)
          (recur (inc i) best bw))))))

(defn- stable-cell
  "Returns the cell of one try, nil when it left the world or hangs
  over nothing the mob may stand on."
  [chunks pos dir]
  (let [cell (pos-toward-direction pos dir)]
    (when-not (or (outside-limits? cell) (not-stable? chunks cell))
      cell)))

(defn- land-cell
  "Returns the cell lifted out of the ground, nil when it holds
  water or costs the mob anything."
  [chunks cell]
  (when cell
    (let [c (move-up-out-of-solid chunks cell)]
      (when-not (or (water? chunks c) (has-malus? chunks c)) c))))

(defn land-pos
  "Returns a walk goal on dry land that costs the mob nothing.
  It is lifted out of the ground, and nil comes back when ten tries
  found none."
  [world e t eid k h v]
  (let [chunks (:chunks world) pos (:pos e)]
    (best-pos (fn [c] (walk-target-value world e c))
              (fn [i]
                (let [d (direction t eid k i h v)]
                  (land-cell chunks (stable-cell chunks pos d)))))))

(defn default-pos
  "Returns a walk goal taken where it falls, costing the mob nothing.
  It is not lifted out of the ground."
  [world e t eid k h v]
  (let [chunks (:chunks world) pos (:pos e)]
    (best-pos (fn [c] (walk-target-value world e c))
              (fn [i]
                (let [d (direction t eid k i h v)
                      c (stable-cell chunks pos d)]
                  (when (and c (not (has-malus? chunks c))) c))))))
