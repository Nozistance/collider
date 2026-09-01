(ns collider.world.support
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn needs-support? [st] (block/needs-support? (long st)))
(defn replaceable? [st] (block/replaceable? (long st)))

(defn- state-at ^long [chunks template [_ y _ :as pos]]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (chunk/chunks-get-block chunks template pos)
      -1)))

(defn- solid-at? [chunks template pos]
  (let [st (state-at chunks template pos)]
    (or (neg? st) (block/solid? st))))

(def ^:private kelp-types #{:kelp :kelp-plant})

(defn- kelp-supported? [chunks template pos]
  (let [below (state-at chunks template (mapv + pos [0 -1 0]))]
    (or (contains? kelp-types (block/type-of below))
        (and (block/face-sturdy? below :up) (not= :magma (block/type-of below))))))

(defn supported? [chunks template pos st]
  (cond
    (contains? kelp-types (block/type-of (long st))) (kelp-supported? chunks template pos)
    :else (if-let [off (block/support-offset (long st))]
            (solid-at? chunks template (mapv + pos off))
            true)))

(def rule
  {:name   :support
   :match? (fn [_chunks st _p] (needs-support? st))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
               (when-not (supported? chunks gen/flat-chunk p st)
                 [[p (block/emptied st)]])))})
