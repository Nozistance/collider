(ns collider.world.scaffold
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(def rule
  {:name   :scaffold
   :match? (fn [_chunks st _p] (= :scaffolding (block/type-of st)))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p _ctx]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)
                   st' (support/scaffold-state chunks gen/flat-chunk p st)]
               (cond
                 (= :7 (:distance (block/props-of st'))) [[p (block/emptied st)]]
                 (not= st' st) [[p st']])))})
