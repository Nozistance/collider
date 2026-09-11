(ns collider.world.grass
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]))

(set! *warn-on-reflection* true)

(def grass-state (block/state :grass-block))
(def dirt-state  (block/state :dirt))
(defn short-grass? [st] (= :short-grass (block/block-of (long st))))
(defn- block-or-zero ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y)
    (chunk/chunks-get-block chunks gen/flat-chunk p)
    0))

(def ^:private neighborhood
  (vec (for [dy [-1 0 1] [dx dz] [[1 0] [-1 0] [0 1] [0 -1]]] [dx dy dz])))

(defn grass-neighbor? [chunks [x y z]]
  (boolean
    (some (fn [[dx dy dz]]
            (= grass-state
               (block-or-zero chunks [(+ (long x) (long dx))
                                      (+ (long y) (long dy))
                                      (+ (long z) (long dz))])))
          neighborhood)))

(defn regrowable-dirt? [chunks p]
  (let [[x y z] p]
    (and (= dirt-state (block-or-zero chunks p))
         (zero? (block-or-zero chunks [x (inc (long y)) z]))
         (grass-neighbor? chunks p))))

(def rule
  {:name  :grass
   :match? (fn [chunks st p] (and (= dirt-state st) (regrowable-dirt? chunks p)))
   :wake  (fn [_chunks tick p _old _self?]
            (+ (long tick) 1200 (mod (long (hash [p tick])) 2400)))
   :due   (fn [_chunks p _rules] [[p grass-state]])})

(defn can-stay-alive? [chunks ^long st [x y z]]
  (let [a (block-or-zero chunks [(long x) (inc (long y)) (long z)])]
    (cond
      (and (= :snow-layer (block/type-of a))
           (= :1 (:layers (block/props-of a)))) true
      (and (liquid/liquid-state? a) (liquid/source-state? a)) false
      :else (< (block/light-dampening-into st a :up (block/dampening a)) 15))))

(defn smothered? [chunks p]
  (and (= grass-state (block-or-zero chunks p))
       (not (can-stay-alive? chunks grass-state p))))

(def smother-rule
  {:name   :grass-smothered
   :match? (fn [chunks st p] (and (= grass-state st) (smothered? chunks p)))
   :wake   (fn [_chunks tick p _old _self?]
             (+ (long tick) 1200 (mod (long (hash [p tick])) 2400)))
   :due    (fn [_chunks p _rules] [[p dirt-state]])})
