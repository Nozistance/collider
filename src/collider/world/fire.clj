(ns collider.world.fire

  (:require [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(def fire-id 51)
(def ^:private tnt-id 46)
(defn fire-state? [st] (= fire-id (bit-shift-right (long st) 4)))

(def ^:private sides
  [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn- neighbor-id? [chunks [x y z] ^long id]
  (boolean
    (some (fn [[dx dy dz]]
            (let [ny (+ (long y) (long dy))]
              (and (<= 0 ny 255)
                   (= id (bit-shift-right
                           (chunk/chunks-get-block
                             chunks gen/flat-chunk
                             [(+ (long x) (long dx)) ny (+ (long z) (long dz))])
                           4)))))
          sides)))

(def rule
  {:name   :fire
   :match? (fn [_chunks st _p] (fire-state? st))
   :wake   (fn [chunks tick p _old _self?]
             (if (support/supported? chunks gen/flat-chunk p
                                     (chunk/chunks-get-block chunks gen/flat-chunk p))
               (if (neighbor-id? chunks p tnt-id)
                 (+ (long tick) 30 (mod (long (hash [p tick])) 10))
                 (+ (long tick) 90 (mod (long (hash [p tick])) 30)))
               (inc (long tick))))
   :due    (fn [chunks p]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
               (cond
                 (not (support/supported? chunks gen/flat-chunk p st)) [[p 0]]
                 (> (bit-and (long st) 15) 3)                          [[p 0]]
                 :else                                                 [[p (inc (long st))]])))})
