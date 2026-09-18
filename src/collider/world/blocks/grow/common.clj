(ns collider.world.blocks.grow.common
  "Tests and helpers shared by the growth ticks."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.light :as light]))

(set! *warn-on-reflection* true)

(defn air-at? [chunks [_ y _ :as p]] (and (chunk/in-range? y) (zero? (chunk/at chunks p))))
(defn lit? [chunks [x y z] ^long n] (>= (long (light/light-at chunks x y z)) n))
(defn chance? [roll salt ^long n] (< (double (roll salt)) (/ 1.0 n)))
(defn pick ^long [roll salt ^long n] (long (Math/floor (* (double (roll salt)) n))))
(defn water? [st] (and (pos? st) (or (= :water (block/liquid-class st)) (block/waterlogged? st))))
(defn flag [b] (if b :true :false))
(defn with
  "Returns st with the properties in kvs set. A value that is not a keyword
   becomes the keyword of its printed form."
  [st & kvs]
  (block/state (block/block-of st) (apply assoc (block/props-of st) (map-indexed (fn [i v] (if (and (odd? i) (not (keyword? v))) (keyword (str v)) v)) kvs))))
(defn age ^long [st] (block/prop-long st :age))
(defn aged ^long [st ^long n] (with st :age n))

(defn height-below ^long [chunks p self ^long cap]
  (loop [h 0]
    (if (and (< h cap) (= self (block/block-of (chunk/at chunks (mapv + p [0 (- (inc h)) 0])))))
      (recur (inc h))
      h)))

(defn height-above ^long [chunks p self ^long cap]
  (loop [h 0]
    (if (and (< h cap) (= self (block/block-of (chunk/at chunks (mapv + p [0 (inc h) 0])))))
      (recur (inc h))
      h)))

(defn crowd ^long [chunks [x y z] self]
  (count (for [dx (range -4 5) dy [-1 0 1] dz (range -4 5)
               :when (= self (block/block-of (chunk/at chunks [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)])))]
           1)))
