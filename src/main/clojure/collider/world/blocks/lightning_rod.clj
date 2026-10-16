(ns collider.world.blocks.lightning-rod
  "Lightning rod, powered for a while when struck or set powered."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as direction]))

(set! *warn-on-reflection* true)

(def ^:const powered-ticks
  "The ticks a rod stays powered."
  8)

(defn rod?
  [^long st]
  (contains? #{:lightning-rod :weathering-lightning-rod}
             (block/type-of st)))

(defn powered
  ^long [^long st on?]
  (block/with st :powered (block/flag on?)))

(defn axis-id
  "Returns 0, 1 or 2 for the axis x, y or z the rod st points along."
  ^long [^long st]
  ({:x 0 :y 1 :z 2} (direction/axis (:facing (block/props-of st)))))

(defn- wake [chunks _dim tick p old side]
  (let [st (chunk/at chunks p)]
    (when (and (nil? side)
               (not= (block/block-of st) (block/block-of old))
               (= :true (:powered (block/props-of st))))
      (+ (long tick) powered-ticks))))

(def rule
  "The block rule that ends the power of a rod set powered or struck."
  {:name   :lightning-rod
   :match? (fn [_chunks st _p] (rod? st))
   :wake   wake
   :due    (fn [chunks p _ctx]
             (let [st (chunk/at chunks p)]
               (when (rod? st) [[p (powered st false)]])))})
