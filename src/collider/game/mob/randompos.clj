(ns collider.game.mob.randompos
  "Where a mob picks a place to walk to.
  RandomPos with its land and default flavours, and the GoalUtils
  filters that turn a cell down."
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

;;; GoalUtils

(defn solid?
  "GoalUtils.isSolid: whether the block filling the cell is solid."
  [chunks cell]
  (block/solid? (state-at chunks cell)))

(defn water?
  "GoalUtils.isWater: whether water stands in the cell."
  [chunks cell]
  (block/water? (state-at chunks cell)))

(defn outside-limits?
  "GoalUtils.isOutsideLimits: whether the cell left the world."
  [[_ y _]]
  (not (chunk/in-range? (long y))))

(defn not-stable?
  "GoalUtils.isNotStable: whether nothing solid holds the cell up."
  [chunks [x y z]]
  (not (block/solid-render?
         (chunk/block-state chunks (long x) (dec (long y))
                            (long z)))))

(defn has-malus?
  "GoalUtils.hasMalus: whether the cell costs the mob anything.
  Not «costs too much»: any malus but a plain zero turns the cell
  down, so water, honey and the neighbourhood of lava are never
  strolled to."
  [chunks [x y z]]
  (not (zero? (path/path-type-malus
                path/cow (path/type-static chunks x y z)))))

;;; RandomPos

(defn direction
  "RandomPos.generateRandomDirection: an offset up to h blocks away
  and v up or down, drawn x, then y, then z."
  [t eid k i h v]
  (let [draw (fn [part n]
               (let [n (long n) span (inc (* 2 n))]
                 (- (long (* span (rnd t eid [k part] i))) n)))]
    [(draw :x h) (draw :y v) (draw :z h)]))

(defn pos-toward-direction
  "RandomPos.generateRandomPosTowardDirection: the offset laid on
  the mob. A mob with a home would pull it homeward; ours has none."
  [pos [dx dy dz]]
  [(long (Math/floor (+ (v/x pos) (double dx))))
   (long (Math/floor (+ (v/y pos) (double dy))))
   (long (Math/floor (+ (v/z pos) (double dz))))])

(defn move-up-out-of-solid
  "RandomPos.moveUpOutOfSolid: the first cell above the solid ones."
  [chunks [x y z :as cell]]
  (if-not (solid? chunks cell)
    cell
    (loop [cy (inc (long y))]
      (if (and (<= cy chunk/max-y) (solid? chunks [x cy z]))
        (recur (inc cy))
        [x cy z]))))

;;; The weight of a cell

(defn- light-cost
  "LevelReader.getPathfindingCostFromLightLevels, in the float the
  game would hold. The ambient light of the overworld is zero."
  ^double [world chunks [x y z]]
  (let [lit (weather/brightness world chunks x y z
                                (:time-of-day world 0))
        b (float (/ (float lit) (float 15.0)))
        curved (float (/ b (float (- (float 4.0)
                                     (float (* (float 3.0) b))))))]
    (double (float (- curved (float 0.5))))))

(defn walk-target-value
  "Animal.getWalkTargetValue: ten on the ground the breed grazes,
  else the light of the cell less a half."
  ^double [world e [x y z :as cell]]
  (let [chunks (:chunks world)
        ground (get-in mobs/types [(:type e) :ground] :grass-block)]
    (if (= ground (block/block-of
                    (chunk/block-state chunks (long x) (dec (long y))
                                       (long z))))
      ground-weight
      (light-cost world chunks cell))))

(defn- bottom-centre [[x y z]]
  [(+ (double (long x)) 0.5) (double (long y))
   (+ (double (long z)) 0.5)])

(defn- best-pos
  "RandomPos.generateRandomPos: ten tries, the heaviest cell wins
  and the first of equals keeps it. The answer is its bottom centre."
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
  "The cell of one try, dropped when it left the world or hangs over
  nothing the mob may stand on."
  [chunks pos dir]
  (let [cell (pos-toward-direction pos dir)]
    (when-not (or (outside-limits? cell) (not-stable? chunks cell))
      cell)))

(defn- land-cell
  "LandRandomPos.movePosUpOutOfSolid: out of the ground, and never
  into water or a cell that costs the mob anything."
  [chunks cell]
  (when cell
    (let [c (move-up-out-of-solid chunks cell)]
      (when-not (or (water? chunks c) (has-malus? chunks c)) c))))

(defn land-pos
  "LandRandomPos.getPos: a walk goal on dry land, out of the ground
  and off anything that costs the mob. Nil when ten tries found none."
  [world e t eid k h v]
  (let [chunks (:chunks world) pos (:pos e)]
    (best-pos (fn [c] (walk-target-value world e c))
              (fn [i]
                (let [d (direction t eid k i h v)]
                  (land-cell chunks (stable-cell chunks pos d)))))))

(defn default-pos
  "DefaultRandomPos.getPos: a walk goal taken where it falls, with
  no climbing out of the ground, but costing the mob nothing."
  [world e t eid k h v]
  (let [chunks (:chunks world) pos (:pos e)]
    (best-pos (fn [c] (walk-target-value world e c))
              (fn [i]
                (let [d (direction t eid k i h v)
                      c (stable-cell chunks pos d)]
                  (when (and c (not (has-malus? chunks c))) c))))))
