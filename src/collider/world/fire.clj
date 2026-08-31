(ns collider.world.fire
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(def ^:const ages 16)
(defn fire-state? [st] (block/fire? (long st)))
(defn fire-state ^long [^long age] (block/state :fire {:age (keyword (str age))}))
(defn age ^long [st] (quot (- (long st) (block/state :fire {:age :0})) 32))

(def ^:private sides
  [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn- tnt-neighbor? [chunks [x y z]]
  (boolean
    (some (fn [[dx dy dz]]
            (let [ny (+ (long y) (long dy))]
              (and (chunk/in-range? ny)
                   (block/tnt? (chunk/chunks-get-block
                                chunks gen/flat-chunk
                                [(+ (long x) (long dx)) ny (+ (long z) (long dz))])))))
          sides)))

(def rule
  {:name   :fire
   :match? (fn [_chunks st _p] (fire-state? st))
   :wake   (fn [chunks tick p _old _self?]
             (if (support/supported? chunks gen/flat-chunk p
                                     (chunk/chunks-get-block chunks gen/flat-chunk p))
               (if (tnt-neighbor? chunks p)
                 (+ (long tick) 30 (mod (long (hash [p tick])) 10))
                 (+ (long tick) 90 (mod (long (hash [p tick])) 30)))
               (inc (long tick))))
   :due    (fn [chunks p]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
               (cond
                 (not (support/supported? chunks gen/flat-chunk p st)) [[p 0]]
                 (> (age st) 3)                                        [[p 0]]
                 :else                                                 [[p (fire-state (inc (age st)))]])))})
