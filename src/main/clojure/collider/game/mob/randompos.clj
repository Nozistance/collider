(ns collider.game.mob.randompos
  "Random walk goals of mobs and the tests on their cells."
  (:require [collider.game.clock :as clock]
            [collider.game.mob.mobs :as mobs]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.weather :as weather]
            [collider.world.space.path :as path])
  (:import (collider.game.mob Steer)))

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
  "Returns true when the block in the cell is solid."
  [chunks cell]
  (block/solid? (state-at chunks cell)))

(defn water?
  "Returns true when water stands in the cell."
  [chunks cell]
  (block/water? (state-at chunks cell)))

(defn outside-limits?
  "Returns true when the cell is outside the height of level lv."
  [lv [_ y _]]
  (not (chunk/in-level? lv (long y))))

(defn not-stable?
  "Returns true when nothing solid holds the cell up."
  [chunks cell]
  (not (block/solid-render? (state-below chunks cell))))

(defn has-malus?
  "Returns true when the cell of level lv costs a walker anything,
  a cow unless told. Mobs never stroll to the cells that cost."
  ([lv cell] (has-malus? lv path/cow cell))
  ([lv walker [x y z]]
   (not (zero? (path/path-type-malus
                 walker (path/type-static lv x y z))))))

(defn- offset [t eid k part i n]
  (let [n (long n) span (inc (* 2 n))]
    (- (long (* span (rnd t eid [k part] i))) n)))

(defn direction
  "Returns an offset up to h blocks away and v up or down.
  The x, the y and the z are drawn in that order."
  [t eid k i h v]
  [(offset t eid k :x i h) (offset t eid k :y i v)
   (offset t eid k :z i h)])

(defn pos-toward-direction
  "Returns the cell the offset lands on from where the mob stands."
  [pos [dx dy dz]]
  [(long (Math/floor (+ (v/x pos) (double dx))))
   (long (Math/floor (+ (v/y pos) (double dy))))
   (long (Math/floor (+ (v/z pos) (double dz))))])

(defn move-up-out-of-solid
  "Returns the first cell of level lv at or above this one that is not
  solid, or the one above the top of the level."
  [lv [x y z :as cell]]
  (let [chunks (:chunks lv) hi (chunk/level-max-y lv)]
    (if-not (solid? chunks cell)
      cell
      (loop [cy (inc (long y))]
        (if (and (<= cy hi) (solid? chunks [x cy z]))
          (recur (inc cy))
          [x cy z])))))

(defn- light-cost
  ^double [world chunks [x y z]]
  (let [day (clock/day-ticks world)
        lit (weather/brightness world chunks x y z day)
        b (float (/ (float lit) (float 15.0)))
        denom (float (- (float 4.0) (float (* (float 3.0) b))))
        curved (float (/ b denom))]
    (double (float (- curved (float 0.5))))))

(defn walk-target-value
  "Returns the weight of a cell.
  It is ten on the ground the breed grazes, else the light of the cell
  less a half."
  ^double [world e cell]
  (let [chunks (:chunks world)
        ground (get-in mobs/types [(:type e) :ground] :grass-block)]
    (if (= ground (block/block-of (state-below chunks cell)))
      ground-weight
      (light-cost world chunks cell))))

(defn- bottom-centre [[x y z]]
  [(+ (double (long x)) 0.5) (double (long y))
   (+ (double (long z)) 0.5)])

(defn- best-pos [weight-of supply]
  (loop [i 0 best nil bw Double/NEGATIVE_INFINITY]
    (if (= i attempts)
      (when best (bottom-centre best))
      (let [cell (supply i)
            w (if cell (double (weight-of cell)) 0.0)]
        (if (and cell (> w bw))
          (recur (inc i) cell w)
          (recur (inc i) best bw))))))

(defn- stable-cell [lv pos dir]
  (let [cell (pos-toward-direction pos dir)]
    (when-not (or (outside-limits? lv cell)
                  (not-stable? (:chunks lv) cell))
      cell)))

(defn- land-cell [lv walker cell]
  (when cell
    (let [c (move-up-out-of-solid lv cell)]
      (when-not (or (water? (:chunks lv) c) (has-malus? lv walker c))
        c))))

(defn land-pos
  "Returns a walk goal on dry land that costs the mob nothing.
  The goal is lifted out of the ground. Returns nil when ten tries
  find none."
  [world e t eid k h v]
  (let [pos (:pos e) w (mobs/walker (:type e))]
    (best-pos (fn [c] (walk-target-value world e c))
              (fn [i]
                (let [d (direction t eid k i h v)]
                  (land-cell world w (stable-cell world pos d)))))))

(defn default-pos
  "Returns a walk goal taken where it falls, costing the mob nothing.
  It is not lifted out of the ground."
  [world e t eid k h v]
  (let [pos (:pos e) w (mobs/walker (:type e))]
    (best-pos (fn [c] (walk-target-value world e c))
              (fn [i]
                (let [d (direction t eid k i h v)
                      c (stable-cell world pos d)]
                  (when (and c (not (has-malus? world w c))) c))))))

(def ^:private quarter-turn (double (float (/ Math/PI 2.0))))

(def ^:private sqrt-2 (double (float (Math/sqrt 2.0))))

(defn- away-direction
  "Returns an offset up to h blocks away and v up or down within a
  quarter turn of the direction dx dz, or nil."
  [t eid k i h v dx dz]
  (let [h (long h)
        c (- (Steer/atan2 (double dz) (double dx)) quarter-turn)
        f (float (rnd t eid [k :angle] i))
        a (+ c (* (double (float (- (float (* 2.0 f)) 1.0)))
                  quarter-turn))
        r (Math/sqrt (rnd t eid [k :dist] i))
        d (* (* r (double h)) sqrt-2)
        xt (- (* d (Math/sin a))) zt (* d (Math/cos a))]
    (when-not (or (> (Math/abs xt) h) (> (Math/abs zt) h))
      [(long (Math/floor xt)) (offset t eid k :y i v)
       (long (Math/floor zt))])))

(defn pos-away
  "Returns a walk goal up to h blocks away and v up or down within a
  quarter turn of the direction dx dz. It costs the mob nothing."
  [world e t eid k h v dx dz]
  (let [pos (:pos e) w (mobs/walker (:type e))]
    (best-pos (fn [c] (walk-target-value world e c))
              (fn [i]
                (when-let [d (away-direction t eid k i h v dx dz)]
                  (let [c (stable-cell world pos d)]
                    (when (and c (not (has-malus? world w c)))
                      c)))))))
