(ns collider.world.grass

  (:require [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.light :as light]))

(set! *warn-on-reflection* true)

(def grass-state 0x20)
(def dirt-state  0x30)
(def tallgrass-id 31)
(defn- block-or-zero ^long [chunks [_ y _ :as p]]
  (if (or (neg? (long y)) (> (long y) 255))
    0
    (chunk/chunks-get-block chunks gen/flat-chunk p)))

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
   :due   (fn [_chunks p] [[p grass-state]])})

(defn smothered? [chunks [x y z]]
  (let [above [(long x) (inc (long y)) (long z)]]
    (and (= grass-state (block-or-zero chunks [(long x) (long y) (long z)]))
         (light/blocks-light? (block-or-zero chunks above))
         (< (light/light-at chunks gen/flat-chunk (above 0) (above 1) (above 2)) 4))))

(def smother-rule
  {:name   :grass-smothered
   :match? (fn [chunks st p] (and (= grass-state st) (smothered? chunks p)))
   :wake   (fn [_chunks tick p _old _self?]
             (+ (long tick) 1200 (mod (long (hash [p tick])) 2400)))
   :due    (fn [_chunks p] [[p dirt-state]])})
