(ns collider.world.blocks.falling
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn free-below? [chunks template [x y z]]
  (let [y' (dec (long y))]
    (and (chunk/in-range? y')
         (block/free? (chunk/chunks-get-block chunks template [x y' z])))))

(def rule
  {:name   :falling
   :match? (fn [_chunks st _p] (and (block/falls? st) (not= :scaffolding (block/type-of st))))
   :wake   (fn [_chunks tick _p _old _self?] (+ (long tick) 2))
   :due    (fn [chunks p _ctx]
             (when (free-below? chunks gen/flat-chunk p)
               [[p (block/emptied (chunk/chunks-get-block chunks gen/flat-chunk p))]]))})
