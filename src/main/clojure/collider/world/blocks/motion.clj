(ns collider.world.blocks.motion
  "What the blocks an entity touches do to the speed it keeps."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const deflate 1.0E-5)

(def ^:private web-speed [0.25 0.05000000074505806 0.25])

(def ^:private ^:const step-offset 0.2)

(def ^:private ^:const slow-fall 0.1)

(def ^:private ^:const step-base 0.4)

(def ^:private ^:const step-slope 0.2)

(defn- lo ^long [^double v] (long (Math/floor (+ v deflate))))

(defn- hi ^long [^double v] (long (Math/floor (- v deflate))))

(defn- state-at ^long [chunks ^long x ^long y ^long z]
  (if (chunk/in-range? y) (chunk/block-state chunks x y z) 0))

(defn- web? [chunks ^long x ^long y ^long z]
  (= :web (block/type-of (state-at chunks x y z))))

(defn- webbed? [chunks [x y z] ^double half ^double height]
  (let [x (double x) y (double y) z (double z)
        x1 (hi (+ x half)) y0 (lo y) y1 (hi (+ y height))
        z0 (lo (- z half)) z1 (hi (+ z half))]
    (loop [bx (lo (- x half)) by y0 bz z0]
      (cond (> bx x1) false
            (> by y1) (recur (inc bx) y0 z0)
            (> bz z1) (recur bx (inc by) z0)
            (web? chunks bx by bz) true
            :else (recur bx by (inc bz))))))

(defn stuck-speed
  "Returns what the blocks that a box at pos stands in multiply the
  next move by. Returns nil when none of them holds it."
  [chunks pos half height]
  (when (webbed? chunks pos (double half) (double height))
    web-speed))

(defn- below-of ^long [chunks [x y z]]
  (state-at chunks (long (Math/floor (double x)))
            (long (Math/floor (- (double y) step-offset)))
            (long (Math/floor (double z)))))

(defn- on-slime? [chunks pos]
  (= :slime (block/type-of (below-of chunks pos))))

(defn stepped-speed [chunks pos vel]
  (let [ay (Math/abs (double (nth vel 1)))]
    (if (and (on-slime? chunks pos) (< ay slow-fall))
      (let [s (+ step-base (* ay step-slope))]
        [(* (double (nth vel 0)) s) (nth vel 1)
         (* (double (nth vel 2)) s)])
      vel)))

(defn- motion-table ^doubles [k ^double default]
  (let [a (double-array (data/block-state-count) default)]
    (doseq [[_ b] (data/blocks)
            :let [v (k b)] :when v
            i (range (reduce * 1 (map count (vals (:props b)))))]
      (aset a (+ (long (:first b)) (long i))
            (double (float (double v)))))
    a))

(def ^:private ^:table frictions
  (delay (motion-table :friction (double (float 0.6)))))

(def ^:private ^:table speed-factors
  (delay (motion-table :speed-factor 1.0)))

(def ^:private ^:table jump-factors
  (delay (motion-table :jump-factor 1.0)))

(defn- factor-of ^double [^doubles a ^long st]
  (if (and (< -1 st) (< st (alength a))) (aget a st) (aget a 0)))

(defn friction
  "Returns the friction of the block of state st, a float."
  ^double [st]
  (factor-of @frictions (long st)))

(defn speed-factor
  "Returns the speed factor of the block of state st, a float."
  ^double [st]
  (factor-of @speed-factors (long st)))

(defn jump-factor
  "Returns the jump factor of the block of state st, a float."
  ^double [st]
  (factor-of @jump-factors (long st)))

(def ^:private ^:const below-offset (double (float 0.500001)))

(defn- state-table ^booleans [ok?]
  (let [a (boolean-array (data/block-state-count))]
    (dotimes [st (alength a)] (aset a st (boolean (ok? st))))
    a))

(def ^:private ^:table supports
  (delay (state-table #(#{:wall :fence-gate} (block/type-of %)))))

(def ^:private ^:table watery
  (delay (state-table
           #(#{:water :bubble-column} (block/block-of %)))))

(defn- flagged? [^booleans a ^long st]
  (and (< -1 st) (< st (alength a)) (aget a st)))

(defn- holds-support? [st]
  (flagged? @supports (long st)))

(defn- floor-of ^long [^double v] (long (Math/floor v)))

(defn below-state
  "Returns the state of the block under a body at pos that sets its
  friction and speed, as Entity.getBlockPosBelowThatAffectsMyMovement
  finds it. sup is the block the body rests on, or nil. offset is
  how far below the body Entity.getOnPos looks."
  (^long [chunks pos sup] (below-state chunks pos sup below-offset))
  (^long [chunks pos sup ^double offset]
   (let [y (floor-of (- (v/y pos) offset))
         st (when sup (chunk/at chunks sup))
         xz (or sup pos)
         x (floor-of (v/x xz))
         z (floor-of (v/z xz))]
     (if (and sup (holds-support? st))
       st
       (state-at chunks x y z)))))

(defn- feet-state ^long [chunks pos]
  (state-at chunks (floor-of (v/x pos)) (floor-of (v/y pos))
            (floor-of (v/z pos))))

(defn block-speed-factor
  "Returns what the blocks at and under a body at pos multiply its
  horizontal speed by after a move, as Entity.getBlockSpeedFactor.
  sup is the block the body rests on, or nil."
  ^double [chunks pos sup]
  (let [st (feet-state chunks pos)
        here (speed-factor st)]
    (if (or (not (== here 1.0))
            (flagged? @watery st))
      here
      (speed-factor (below-state chunks pos sup)))))
