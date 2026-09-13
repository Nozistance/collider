(ns collider.world.blocks.lectern
  "Lectern: its book and its redstone pulse."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def ^:const impulse-ticks 2)

(defn lectern?
  "Returns true when st is a lectern."
  [^long st] (= :lectern (block/type-of st)))
(defn has-book?
  "Returns true when the lectern st holds a book."
  [^long st] (= :true (:has-book (block/props-of st))))
(defn powered?
  "Returns true when the lectern st is giving out a signal."
  [^long st] (= :true (:powered (block/props-of st))))
(defn facing
  "Returns the direction the lectern st faces."
  [^long st] (:facing (block/props-of st)))

(defn- with-props [^long st m]
  (block/state (block/block-of st) (merge (block/props-of st) m)))

(defn reset-state
  "Returns st unpowered, holding a book when book? is true."
  [^long st book?]
  (with-props st {:powered :false :has-book (if book? :true :false)}))

(defn powered-state
  "Returns st giving out a signal when on? is true, silent otherwise."
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
