(ns collider.world.space.column
  "The heights of a block column. Its surface, its motion blocking top
  and its floor."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const none (dec chunk/min-y))

(defn state-at
  "Returns the block state at x y z, air outside the world height."
  [chunks x y z]
  (if (chunk/in-range? (long y))
    (chunk/block-state chunks x y z)
    0))

(defn fluid?
  "Returns true when st holds a fluid."
  [^long st] (some? (block/liquid-class st)))

(defn- air? [^long st] (block/air-type? st))

(defn- motion-blocking? [^long st]
  (or (block/blocks-motion? st) (fluid? st)))

(defn- blank-at? [chunks ^long x ^long y ^long z]
  (let [cx (chunk/block->chunk x) cz (chunk/block->chunk z)
        c (get chunks (chunk/pos->id cx cz))
        s (when c (chunk/chunk-section c (chunk/section-index y)))]
    (or (nil? s) (identical? s chunk/empty-section))))

(defn column-heights
  "Returns [surface motion floor], the y of the highest block that is
  not air, of the highest block or fluid that blocks motion, and of the
  highest block that blocks motion, in the column at x z."
  [chunks x z]
  (loop [y (long chunk/max-y) surface none motion none floor none]
    (cond
      (or (< y (long chunk/min-y)) (not= floor none))
      [surface motion floor]
      (blank-at? chunks x y z)
      (recur (dec (bit-and y -16)) surface motion floor)
      :else
      (let [st (long (state-at chunks x y z))
            top? (and (= motion none) (motion-blocking? st))]
        (recur (dec y)
               (if (and (= surface none) (not (air? st))) y surface)
               (if top? y motion)
               (if (block/blocks-motion? st) y floor))))))

(defn surface-top
  "Returns the y of the highest block of the column at x z that is
  not air, one below the lowest y when there is none."
  ^long [chunks x z]
  (long (nth (column-heights chunks x z) 0)))

(defn motion-blocking-height
  "Returns the y just above the top of the column at x z.
  The top is its highest block or fluid."
  ^long [chunks x z]
  (let [[_ motion _] (column-heights chunks x z)]
    (if (= (long motion) (long none))
      (long chunk/min-y)
      (inc (long motion)))))
