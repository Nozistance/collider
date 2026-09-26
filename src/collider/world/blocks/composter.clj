(ns collider.world.blocks.composter
  "Composter: its fill level, and the settling of a full one."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn level ^long [^long st] (block/prop-long st :level))

(def rule
  {:name   :composter
   :match? (fn [_chunks st _p] (= :composter (block/type-of st)))
   :wake   (fn [chunks _dim tick p _old self?]
             (let [st (chunk/chunks-get-block chunks p)]
               (when (and self? (= 7 (level st)))
                 (+ (long tick) 20))))
   :due    (fn [chunks p _ctx]
             (let [st (chunk/chunks-get-block chunks p)]
               (when (= 7 (level st))
                 [[p (block/state :composter {:level :8})]])))})
