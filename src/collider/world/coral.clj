(ns collider.world.coral
  (:require [collider.rnd :as rnd]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]))

(set! *warn-on-reflection* true)

(def ^:private around6 [[0 -1 0] [0 1 0] [0 0 -1] [0 0 1] [-1 0 0] [1 0 0]])

(defn- at ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks gen/flat-chunk p) 0))

(defn- water? [st] (and (pos? st) (or (= :water (liquid/liquid-class st)) (block/waterlogged? st))))

(defn wet?
  [chunks p st]
  (or (block/waterlogged? st)
      (boolean (some #(water? (at chunks (mapv + p %))) around6))))

(def rule
  {:name   :coral
   :match? (fn [_chunks st _p] (contains? block/coral-types (block/type-of st)))
   :wake   (fn [chunks tick p _old _self?]
             (when-not (wet? chunks p (at chunks p))
               (+ (long tick) 60 (long (Math/floor (* 40.0 (rnd/rnd [tick p :coral])))))))
   :due    (fn [chunks p _ctx]
             (let [st (at chunks p)]
               (when-not (wet? chunks p st)
                 [[p (block/dead-coral st)]])))})
