(ns collider.world.sponge
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]))

(set! *warn-on-reflection* true)

(def ^:private around6 [[0 -1 0] [0 1 0] [0 0 -1] [0 0 1] [-1 0 0] [1 0 0]])
(def ^:private plants #{:kelp :kelp-plant :seagrass :tall-seagrass})

(defn- at ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks gen/flat-chunk p) 0))

(defn- dried
  [st]
  (cond
    (contains? plants (block/type-of st)) 0
    (= :true (:waterlogged (block/props-of st))) (block/without-water st)
    (= :water (liquid/liquid-class st)) 0))

(defn absorbed
  [chunks pos]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [pos 0]) seen #{pos} acc []]
    (if (or (empty? queue) (>= (count acc) 64))
      (when (seq acc) (conj acc [pos (block/state :wet-sponge)]))
      (let [[p ^long d] (peek queue)
            wet (when (< d 6)
                  (for [o around6 :let [q (mapv + p o)] :when (and (not (seen q)) (some? (dried (at chunks q))))] q))]
        (recur (into (pop queue) (map (fn [q] [q (inc d)]) wet))
               (into seen wet)
               (into acc (map (fn [q] [q (dried (at chunks q))]) wet)))))))

(def rule
  {:name   :sponge
   :match? (fn [_chunks st _p] (= :sponge (block/type-of st)))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p _ctx] (absorbed chunks p))})
