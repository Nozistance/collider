(ns collider.world.blocks.campfire
  "Campfire placement, signal smoke, and dowsing."
  (:require [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn campfire? [^long st] (= :campfire (block/type-of st)))

(defn- smoke-source? [^long st] (= :hay-block (block/block-of st)))

(defn- at [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks p) 0))

(defn- with-props [^long st m]
  (block/state (block/block-of st) (merge (block/props-of st) m)))

(defn- flag [x] (if x :true :false))

(defn placed [chunks pos st yaw]
  (let [water? (= :water (liquid/liquid-class (at chunks pos)))]
    (with-props st {:facing      (dir/player-direction yaw)
                    :waterlogged (flag water?)
                    :lit         (flag (not water?))
                    :signal-fire (flag (smoke-source? (at chunks (dir/down pos))))})))

(defn updated [self ^long st at]
  (block/state self (assoc (block/props-of st)
                      :signal-fire (flag (smoke-source? (at [0 -1 0]))))))

(defn dowsed [^long st]
  (when (and (campfire? st) (= :true (:lit (block/props-of st))))
    (with-props st {:lit :false})))

(defn drowned [^long st]
  (when (and (campfire? st) (= :false (:waterlogged (block/props-of st))))
    (with-props st {:waterlogged :true :lit :false})))
