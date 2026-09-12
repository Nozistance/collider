(ns collider.world.blocks.water
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.support :as support])
  (:import (clojure.lang PersistentQueue)))

(set! *warn-on-reflection* true)

(def ^:private around6 [[0 -1 0] [0 1 0] [0 0 -1] [0 0 1] [-1 0 0] [1 0 0]])
(def ^:private kelp-types #{:kelp :kelp-plant})
(def ^:private plants #{:kelp :kelp-plant :seagrass :tall-seagrass})

(defn- water? [st] (and (pos? st) (or (= :water (liquid/liquid-class st)) (block/waterlogged? st))))

(defn coral-wet? [chunks p st]
  (or (block/waterlogged? st)
      (boolean (some #(water? (gen/at chunks (mapv + p %))) around6))))

(def coral-rule
  {:name   :coral
   :match? (fn [_chunks st _p] (contains? block/coral-types (block/type-of st)))
   :wake   (fn [chunks tick p _old _self?]
             (when-not (coral-wet? chunks p (gen/at chunks p))
               (+ (long tick) 60 (long (Math/floor (* 40.0 (random/of-key [tick p :coral])))))))
   :due    (fn [chunks p _ctx]
             (let [st (gen/at chunks p)]
               (when-not (coral-wet? chunks p st)
                 [[p (block/dead-coral st)]])))})

(defn kelp? [st] (contains? kelp-types (block/type-of (long st))))

(defn kelp-head-state ^long [tick pos]
  (block/state :kelp {:age (support/plant-age tick pos)}))

(defn- above [chunks [x y z]]
  (let [y (inc (long y))]
    (if (chunk/in-range? y) (chunk/chunks-get-block chunks gen/flat-chunk [x y z]) 0)))

(def kelp-rule
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
                       :kelp-plant (when-not up? [[p (kelp-head-state (:tick ctx) p)]])
                       nil)
                     (liquid/update-cell chunks gen/flat-chunk p (:rules ctx))))))})

(defn- dried [st]
  (cond
    (contains? plants (block/type-of st)) 0
    (= :true (:waterlogged (block/props-of st))) (block/without-water st)
    (= :water (liquid/liquid-class st)) 0))

(defn absorbed [chunks pos]
  (loop [queue (conj PersistentQueue/EMPTY [pos 0]) seen #{pos} acc []]
    (if (or (empty? queue) (>= (count acc) 64))
      (when (seq acc) (conj acc [pos (block/state :wet-sponge)]))
      (let [[p ^long d] (peek queue)
            wet (when (< d 6)
                  (for [o around6 :let [q (mapv + p o)] :when (and (not (seen q)) (some? (dried (gen/at chunks q))))] q))]
        (recur (into (pop queue) (map (fn [q] [q (inc d)]) wet))
               (into seen wet)
               (into acc (map (fn [q] [q (dried (gen/at chunks q))]) wet)))))))

(def sponge-rule
  {:name   :sponge
   :match? (fn [_chunks st _p] (= :sponge (block/type-of st)))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p _ctx] (absorbed chunks p))})
