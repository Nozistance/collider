(ns collider.world.blocks.water
  "What water does to the blocks in it: coral, kelp and sponges."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.support :as support]
            [collider.world.direction :as dir]
            [collider.world.env.attribute :as attribute])
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

(defn- held-side
  "Returns the side a coral stands on, or nil for a coral block."
  [st]
  (case (block/type-of st)
    (:coral-plant :coral-fan) :down
    :coral-wall-fan (dir/opposite (block/facing-of st))
    nil))

(defn- die-tick ^long [tick p]
  (let [roll (random/of-key tick p :coral)]
    (+ (long tick) 60 (long (Math/floor (* 40.0 roll))))))

(defn- coral-wake [chunks _dim tick p _old side]
  (let [st (chunk/at chunks p)]
    (cond
      (and side (= side (held-side st))
           (not (support/supported? chunks p st))) :neighbor
      (not (coral-wet? chunks p st)) (die-tick tick p))))

(defn- coral-dies [chunks p _ctx]
  (let [st (chunk/at chunks p)]
    (when-not (coral-wet? chunks p st)
      [[p (block/dead-coral st)]])))

(def coral-rule
  {:name    :coral
   :match?  (fn [_chunks st _p]
              (contains? block/coral-types (block/type-of st)))
   :wake    coral-wake
   :reshape (:due support/rule)
   :due     coral-dies})

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
      :kelp-plant (when-not up?
                    [[p (kelp-head-state (:tick ctx) p)]])
      nil)))

(defn- kelp-wake
  "GrowingPlantHeadBlock and GrowingPlantBodyBlock: a tick when the
  block below no longer holds it, a new head or stem when the block
  above or below changes. The change at pos itself asks for the tick
  too: here a new head or stem comes a tick late, after the change
  below, and the tick it had was for the old type."
  [chunks _dim tick p _old side]
  (let [held? (support/supported? chunks p (chunk/at chunks p))]
    (cond
      (and (contains? #{nil :down} side) (not held?))
      (inc (long tick))
      (contains? #{nil :up :down} side) :neighbor)))

(def kelp-rule
  {:name    :kelp
   :match?  (fn [_chunks st _p] (kelp? st))
   :wake    kelp-wake
   :reshape kelp-grown
   :due     (:due support/rule)})

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

(defn- dried-at
  "SpongeBlock.removeWaterBreadthFirstSearch: a plant drops."
  [chunks p]
  (let [st (chunk/at chunks p)]
    (if (contains? plants (block/type-of st))
      [p (dried st) [[:drop st]]]
      [p (dried st)])))

(def ^:private absorb-sound [:sound :block.sponge.absorb 1.0 1.0])

(defn- soaked
  "SpongeBlock.tryAbsorbWater. Where water evaporates the wet
  sponge dries at once, WetSpongeBlock.onPlace."
  [pos dim]
  (if (attribute/water-evaporates? dim)
    [pos (block/state :sponge) [:dry absorb-sound]]
    [pos (block/state :wet-sponge) [absorb-sound]]))

(defn absorbed
  "Returns the changes of a sponge at pos in dim soaking up water."
  [chunks pos dim]
  (loop [queue (conj PersistentQueue/EMPTY [pos 0])
         seen #{pos}
         acc []]
    (if (or (empty? queue) (>= (count acc) 64))
      (when (seq acc) (conj acc (soaked pos dim)))
      (let [[p ^long d] (peek queue)
            wet (when (< d 6) (wet-around chunks seen p))]
        (recur (into (pop queue) (map (fn [q] [q (inc d)]) wet))
               (into seen wet)
               (into acc (map #(dried-at chunks %)) wet))))))

(def sponge-rule
  {:name    :sponge
   :match?  (fn [_chunks st _p] (= :sponge (block/type-of st)))
   :wake    (fn [_chunks _dim _tick _p _old _side] :neighbor)
   :reach   6
   :reshape (fn [chunks p ctx] (absorbed chunks p (:dim ctx)))})
