(ns collider.world.blocks.motion
  "What the blocks an entity touches do to the speed it keeps."
  (:require [collider.world.block :as block]
            [collider.world.gen :as gen]))

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
      (gen/at chunks [bx by bz]))))

(defn stuck-speed
  "Returns what the blocks a box at pos stands in multiply its next move by,
   nil when none of them holds it."
  [chunks pos half height]
  (when (some (fn [st] (= :web (block/type-of (long st))))
              (states-in chunks pos half height))
    web-speed))

(defn- below-of [chunks [x y z]]
  (gen/at chunks [(long (Math/floor (double x)))
                  (long (Math/floor (- (double y) step-offset)))
                  (long (Math/floor (double z)))]))

(defn stepped-speed [chunks pos vel]
  (let [ay (Math/abs (double (nth vel 1)))]
    (if (and (= :slime (block/type-of (below-of chunks pos))) (< ay slow-fall))
      (let [s (+ step-base (* ay step-slope))]
        [(* (double (nth vel 0)) s) (nth vel 1) (* (double (nth vel 2)) s)])
      vel)))
