(ns collider.world.blocks.composter
  "Composter fill levels and the settling of a full one."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const full-level 7)

(def ^:private ^:const settle-delay 20)

(defn- level ^long [^long st] (block/prop-long st :level))

(defn- full? [^long st] (= full-level (level st)))

(def rule
  "The block rule that makes a full composter ready after a while."
  {:name   :composter
   :match? (fn [_chunks st _p] (= :composter (block/type-of st)))
   :wake   (fn [chunks _dim tick p _old side]
             (let [st (chunk/chunks-get-block chunks p)]
               (when (and (nil? side) (full? st))
                 (+ (long tick) settle-delay))))
   :due    (fn [chunks p _ctx]
             (let [st (chunk/chunks-get-block chunks p)]
               (when (full? st)
                 [[p (block/state :composter {:level :8})
                   [[:sound :block.composter.ready 1.0 1.0]]]])))})
