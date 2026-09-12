(ns collider.world.blocks.grow.bamboo
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]
            [collider.world.blocks.grow.common :refer [age air-at? chance? height-above height-below lit? with]]))

(set! *warn-on-reflection* true)

(defn- grown [chunks p st roll height]
  (let [below (gen/at chunks (dir/down p)) two (gen/at chunks (dir/down (dir/down p)))
        bamboo? (fn [s] (= :bamboo (block/block-of s)))
        leaves (if (or (not (bamboo? below)) (= :none (:leaves (block/props-of below)))) :small :large)
        shift (when (and (= leaves :large) (bamboo? two))
                [[(dir/down p) (with below :leaves :small)] [(dir/down (dir/down p)) (with two :leaves :none)]])
        a (if (and (not= 1 (age st)) (not (bamboo? two))) 0 1)
        height (long height)
        stage (if (or (and (>= height 11) (< (double (roll :stage)) 0.25)) (= height 15)) 1 0)]
    (conj (vec shift) [(dir/up p) (block/state :bamboo {:age (keyword (str a)) :leaves leaves :stage (keyword (str stage))})])))

(defn tick [chunks p st roll _time _ctx]
  (when (and (= 0 (block/prop-long st :stage)) (chance? roll :gate 3) (air-at? chunks (dir/up p)) (lit? chunks (dir/up p) 9))
    (let [height (inc (height-below chunks p :bamboo 16))]
      (when (< height 16)
        (grown chunks p st roll height)))))

(defn sapling-tick [chunks p _st roll _time _ctx]
  (when (and (chance? roll :gate 3) (air-at? chunks (dir/up p)) (lit? chunks (dir/up p) 9))
    [[(dir/up p) (block/state :bamboo {:leaves :small})]]))

(defn sapling-meal [chunks p _st _roll]
  (when (air-at? chunks (dir/up p)) {:changes [[(dir/up p) (block/state :bamboo {:leaves :small})]]}))

(defn meal [chunks p _st roll]
  (let [above (height-above chunks p :bamboo 16)
        below (height-below chunks p :bamboo 16)
        top (mapv + p [0 above 0])
        top-st (gen/at chunks top)
        target (dir/up top)]
    (when (and (< (+ above below 1) 16)
               (not= 1 (block/prop-long top-st :stage))
               (chunk/in-range? (long (target 1)))
               (air-at? chunks target))
      {:changes (grown chunks top top-st roll (+ above below 1))})))
