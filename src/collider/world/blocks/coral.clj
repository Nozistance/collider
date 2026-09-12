(ns collider.world.blocks.coral
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.gen :as gen]
            [collider.world.blocks.liquid :as liquid]))

(set! *warn-on-reflection* true)

(def ^:private around6 [[0 -1 0] [0 1 0] [0 0 -1] [0 0 1] [-1 0 0] [1 0 0]])
(defn- water? [st] (and (pos? st) (or (= :water (liquid/liquid-class st)) (block/waterlogged? st))))
(defn wet? [chunks p st]
  (or (block/waterlogged? st)
      (boolean (some #(water? (gen/at chunks (mapv + p %))) around6))))

(def rule
  {:name   :coral
   :match? (fn [_chunks st _p] (contains? block/coral-types (block/type-of st)))
   :wake   (fn [chunks tick p _old _self?]
             (when-not (wet? chunks p (gen/at chunks p))
               (+ (long tick) 60 (long (Math/floor (* 40.0 (random/of-key [tick p :coral])))))))
   :due    (fn [chunks p _ctx]
             (let [st (gen/at chunks p)]
               (when-not (wet? chunks p st)
                 [[p (block/dead-coral st)]])))})
