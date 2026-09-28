(ns collider.world.blocks.motion
  "What the blocks an entity touches do to the speed it keeps."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const deflate 1.0E-5)

(def ^:private web-speed [0.25 0.05000000074505806 0.25])

(def ^:private ^:const step-offset 0.2)

(def ^:private ^:const slow-fall 0.1)

(def ^:private ^:const step-base 0.4)

(def ^:private ^:const step-slope 0.2)

(defn- lo ^long [^double v] (long (Math/floor (+ v deflate))))

(defn- hi ^long [^double v] (long (Math/floor (- v deflate))))

(defn- states-in [chunks [x y z] ^double half ^double height]
  (let [x (double x) y (double y) z (double z)]
    (for [bx (range (lo (- x half)) (inc (hi (+ x half))))
          by (range (lo y) (inc (hi (+ y height))))
          bz (range (lo (- z half)) (inc (hi (+ z half))))]
      (chunk/at chunks [bx by bz]))))

(defn stuck-speed
  "Returns what the blocks that a box at pos stands in multiply the
  next move by. Returns nil when none of them holds it."
  [chunks pos half height]
  (when (some (fn [st] (= :web (block/type-of (long st))))
              (states-in chunks pos half height))
    web-speed))

(defn- below-of [chunks [x y z]]
  (chunk/at chunks [(long (Math/floor (double x)))
                  (long (Math/floor (- (double y) step-offset)))
                  (long (Math/floor (double z)))]))

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
  (if (< -1 st (alength a)) (aget a st) (aget a 0)))

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

(defn- holds-support? [st]
  (contains? #{:wall :fence-gate} (block/type-of (long st))))

(defn- floor-of ^long [v] (long (Math/floor (double v))))

(defn below-state
  "Returns the state of the block under a body at pos that sets its
  friction and speed, as Entity.getBlockPosBelowThatAffectsMyMovement
  finds it. sup is the block the body rests on, or nil."
  ^long [chunks pos sup]
  (let [y (floor-of (- (double (nth pos 1)) below-offset))
        st (when sup (chunk/at chunks sup))
        x (floor-of (nth (or sup pos) 0))
        z (floor-of (nth (or sup pos) 2))]
    (if (and sup (holds-support? st))
      st
      (chunk/at chunks [x y z]))))

(defn- feet-state ^long [chunks pos]
  (chunk/at chunks [(floor-of (nth pos 0)) (floor-of (nth pos 1))
                    (floor-of (nth pos 2))]))

(defn block-speed-factor
  "Returns what the blocks at and under a body at pos multiply its
  horizontal speed by after a move, as Entity.getBlockSpeedFactor.
  sup is the block the body rests on, or nil."
  ^double [chunks pos sup]
  (let [st (feet-state chunks pos)
        here (speed-factor st)]
    (if (or (not (== here 1.0))
            (contains? #{:water :bubble-column} (block/block-of st)))
      here
      (speed-factor (below-state chunks pos sup)))))
