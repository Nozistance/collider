(ns collider.world.blocks.grow.common
  "Tests and helpers shared by the growth ticks."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.light :as light]
            [collider.world.blocks.liquid :as liquid]))

(set! *warn-on-reflection* true)

(defn air-at?
  "Returns true when there is air at p."
  [chunks [_ y _ :as p]] (and (chunk/in-range? y) (zero? (gen/at chunks p))))
(defn lit?
  "Returns true when the light at the position is n or more."
  [chunks [x y z] ^long n] (>= (long (light/light-at chunks gen/flat-chunk x y z)) n))
(defn chance?
  "Returns true one time in n, drawn from roll at salt."
  [roll salt ^long n] (< (double (roll salt)) (/ 1.0 n)))
(defn pick
  "Returns a number from 0 up to but not including n, drawn from roll at salt."
  ^long [roll salt ^long n] (long (Math/floor (* (double (roll salt)) n))))
(defn water?
  "Returns true when st is water or holds water."
  [st] (and (pos? st) (or (= :water (liquid/liquid-class st)) (block/waterlogged? st))))
(defn flag
  "Returns the block property keyword for a boolean."
  [b] (if b :true :false))
(defn with
  "Returns st with those properties set; a value that is not a keyword is named
   by its printed form."
  [st & kvs]
  (block/state (block/block-of st) (apply assoc (block/props-of st) (map-indexed (fn [i v] (if (and (odd? i) (not (keyword? v))) (keyword (str v)) v)) kvs))))
(defn age
  "Returns how far st has grown."
  ^long [st] (block/prop-long st :age))
(defn aged
  "Returns st at age n."
  ^long [st ^long n] (with st :age n))

(defn height-below
  "Returns how many blocks of self stand in a row right below p, at most cap."
  ^long [chunks p self ^long cap]
  (loop [h 0]
    (if (and (< h cap) (= self (block/block-of (gen/at chunks (mapv + p [0 (- (inc h)) 0])))))
      (recur (inc h))
      h)))

(defn height-above
  "Returns how many blocks of self stand in a row right above p, at most cap."
  ^long [chunks p self ^long cap]
  (loop [h 0]
    (if (and (< h cap) (= self (block/block-of (gen/at chunks (mapv + p [0 (inc h) 0])))))
      (recur (inc h))
      h)))

(defn crowd
  "Returns how many blocks of self stand within four blocks of the position, one
   level up or down."
  ^long [chunks [x y z] self]
  (count (for [dx (range -4 5) dy [-1 0 1] dz (range -4 5)
               :when (= self (block/block-of (gen/at chunks [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)])))]
           1)))
