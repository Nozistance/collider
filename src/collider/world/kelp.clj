(ns collider.world.kelp
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(def ^:private kelp-types #{:kelp :kelp-plant})
(defn kelp? [st] (contains? kelp-types (block/type-of (long st))))
(defn head-state ^long [tick pos]
  (block/state :kelp {:age (support/plant-age tick pos)}))

(defn- above [chunks [x y z]]
  (let [y (inc (long y))]
    (if (chunk/in-range? y) (chunk/chunks-get-block chunks gen/flat-chunk [x y z]) 0)))

(def rule
  {:name   :kelp
   :match? (fn [_chunks st _p] (kelp? st))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p ctx]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)
                   up? (kelp? (above chunks p))]
               (or (seq ((:due support/rule) chunks p nil))
                   (concat
                    (case (block/type-of st)
                      :kelp (when up? [[p (block/state :kelp-plant)]])
                      :kelp-plant (when-not up? [[p (head-state (:tick ctx) p)]])
                      nil)
                    (liquid/update-cell chunks gen/flat-chunk p (:rules ctx))))))})
