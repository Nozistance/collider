(ns collider.world.blocks.lectern
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def ^:const impulse-ticks 2)

(defn lectern? [^long st] (= :lectern (block/type-of st)))
(defn has-book? [^long st] (= :true (:has-book (block/props-of st))))
(defn powered? [^long st] (= :true (:powered (block/props-of st))))
(defn facing [^long st] (:facing (block/props-of st)))

(defn- with-props [^long st m]
  (block/state (block/block-of st) (merge (block/props-of st) m)))

(defn reset-state
  [^long st book?]
  (with-props st {:powered :false :has-book (if book? :true :false)}))

(defn powered-state
  [^long st on?]
  (with-props st {:powered (if on? :true :false)}))

(def rule
  {:name   :lectern
   :match? (fn [_chunks st _p] (lectern? st))
   :wake   (fn [_chunks _tick _p _old _self?] nil)
   :due    (fn [chunks p _ctx]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
               (when (and (lectern? st) (powered? st))
                 [[p (powered-state st false)]])))})
