(ns collider.world.blocks.water
  "What water does to the blocks in it: coral, kelp and sponges."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.support :as support])
  (:import (clojure.lang PersistentQueue)))

(set! *warn-on-reflection* true)

(def ^:private around6
  [[0 -1 0] [0 1 0] [0 0 -1] [0 0 1] [-1 0 0] [1 0 0]])

(def ^:private kelp-types #{:kelp :kelp-plant})

(def ^:private plants #{:kelp :kelp-plant :seagrass :tall-seagrass})

(defn- water? [st] (block/water? st))

(defn coral-wet? [chunks p st]
  (or (block/waterlogged? st)
      (boolean
        (some #(water? (chunk/at chunks (mapv + p %))) around6))))

(defn- coral-wake [chunks _dim tick p _old _self?]
  (when-not (coral-wet? chunks p (chunk/at chunks p))
    (let [roll (random/of-key tick p :coral)]
      (+ (long tick) 60 (long (Math/floor (* 40.0 roll)))))))

(defn- coral-dies [chunks p _ctx]
  (let [st (chunk/at chunks p)]
    (when-not (coral-wet? chunks p st)
      [[p (block/dead-coral st)]])))

(def coral-rule
  {:name   :coral
   :match? (fn [_chunks st _p]
             (contains? block/coral-types (block/type-of st)))
   :wake   coral-wake
   :due    coral-dies})

(defn kelp? [st] (contains? kelp-types (block/type-of (long st))))

(defn kelp-head-state ^long [tick pos]
  (block/state :kelp {:age (support/plant-age tick pos)}))

(defn- above [chunks [x y z]]
  (let [y (inc (long y))]
    (if (chunk/in-range? y)
      (chunk/chunks-get-block chunks [x y z])
      0)))

(defn- kelp-grown [chunks p ctx]
  (let [st (chunk/chunks-get-block chunks p)
        up? (kelp? (above chunks p))]
    (case (block/type-of st)
      :kelp (when up? [[p (block/state :kelp-plant)]])
      :kelp-plant (when-not up? [[p (kelp-head-state (:tick ctx) p)]])
      nil)))

(defn- kelp-due [chunks p ctx]
  (or (seq ((:due support/rule) chunks p nil))
      (concat (kelp-grown chunks p ctx)
              (liquid/update-cell chunks p ctx))))

(def kelp-rule
  {:name   :kelp
   :match? (fn [_chunks st _p] (kelp? st))
   :wake   (fn [_chunks _dim tick _p _old _self?] (inc (long tick)))
   :due    kelp-due})

(defn- dried [st]
  (cond
    (contains? plants (block/type-of st)) 0
    (= :true (:waterlogged (block/props-of st)))
    (block/without-water st)
    (block/water? st) 0))

(defn- wet-around [chunks seen p]
  (for [o around6
        :let [q (mapv + p o)]
        :when (and (not (seen q))
                   (some? (dried (chunk/at chunks q))))]
    q))

(defn absorbed
  "Returns the changes of a sponge at pos soaking up water."
  [chunks pos]
  (loop [queue (conj PersistentQueue/EMPTY [pos 0])
         seen #{pos}
         acc []]
    (if (or (empty? queue) (>= (count acc) 64))
      (when (seq acc) (conj acc [pos (block/state :wet-sponge)]))
      (let [[p ^long d] (peek queue)
            wet (when (< d 6) (wet-around chunks seen p))]
        (recur (into (pop queue) (map (fn [q] [q (inc d)]) wet))
               (into seen wet)
               (into acc (map #(vector % (dried (chunk/at chunks %))))
                     wet))))))

(def sponge-rule
  {:name   :sponge
   :match? (fn [_chunks st _p] (= :sponge (block/type-of st)))
   :wake   (fn [_chunks _dim tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p _ctx] (absorbed chunks p))})
