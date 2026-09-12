(ns collider.world.blocks.grow.common
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.light :as light]
            [collider.world.blocks.liquid :as liquid]))

(set! *warn-on-reflection* true)

(defn air-at? [chunks [_ y _ :as p]] (and (chunk/in-range? y) (zero? (gen/at chunks p))))
(defn lit? [chunks [x y z] ^long n] (>= (long (light/light-at chunks gen/flat-chunk x y z)) n))
(defn chance? [roll salt ^long n] (< (double (roll salt)) (/ 1.0 n)))
(defn pick ^long [roll salt ^long n] (long (Math/floor (* (double (roll salt)) n))))
(defn water? [st] (and (pos? st) (or (= :water (liquid/liquid-class st)) (block/waterlogged? st))))
(defn flag [b] (if b :true :false))
(defn with [st & kvs]
  (block/state (block/block-of st) (apply assoc (block/props-of st) (map-indexed (fn [i v] (if (and (odd? i) (not (keyword? v))) (keyword (str v)) v)) kvs))))
(defn age ^long [st] (block/prop-long st :age))
(defn aged ^long [st ^long n] (with st :age n))

(defn height-below ^long [chunks p self ^long cap]
  (loop [h 0]
    (if (and (< h cap) (= self (block/block-of (gen/at chunks (mapv + p [0 (- (inc h)) 0])))))
      (recur (inc h))
      h)))

(defn height-above ^long [chunks p self ^long cap]
  (loop [h 0]
    (if (and (< h cap) (= self (block/block-of (gen/at chunks (mapv + p [0 (inc h) 0])))))
      (recur (inc h))
      h)))

(defn crowd ^long [chunks [x y z] self]
  (count (for [dx (range -4 5) dy [-1 0 1] dz (range -4 5)
               :when (= self (block/block-of (gen/at chunks [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)])))]
           1)))
