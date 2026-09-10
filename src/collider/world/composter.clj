(ns collider.world.composter
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn level ^long [^long st] (Long/parseLong (name (:level (block/props-of st)))))

(def rule
  {:name   :composter
   :match? (fn [_chunks st _p] (= :composter (block/type-of st)))
   :wake   (fn [chunks tick p _old self?]
             (when (and self? (= 7 (level (chunk/chunks-get-block chunks gen/flat-chunk p))))
               (+ (long tick) 20)))
   :due    (fn [chunks p _ctx]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
               (when (= 7 (level st))
                 [[p (block/state :composter {:level :8})]])))})
