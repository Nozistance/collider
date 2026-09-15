(ns collider.world.blocks.campfire
  "Campfires: how they face when placed, when they smoke high, and what
   lights them or puts them out."
  (:require [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn campfire?
  "Returns true when st is a campfire."
  [^long st] (= :campfire (block/type-of st)))

(defn- smoke-source? [^long st] (= :hay-block (block/block-of st)))

(defn- at [chunks template [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks template p) 0))

(defn- with-props [^long st m]
  (block/state (block/block-of st) (merge (block/props-of st) m)))

(defn- flag [x] (if x :true :false))

(defn placed
  "Returns the state a campfire takes when placed at pos by a player looking
   along yaw."
  [chunks template pos st yaw]
  (let [water? (= :water (liquid/liquid-class (at chunks template pos)))]
    (with-props st {:facing      (dir/player-direction yaw)
                    :waterlogged (flag water?)
                    :lit         (flag (not water?))
                    :signal-fire (flag (smoke-source? (at chunks template (dir/down pos))))})))

(defn updated
  "Returns the state st takes from the block below it."
  [self ^long st at]
  (block/state self (assoc (block/props-of st)
                      :signal-fire (flag (smoke-source? (at [0 -1 0]))))))

(defn dowsed
  "Returns st put out, nil when it is not burning."
  [^long st]
  (when (and (campfire? st) (= :true (:lit (block/props-of st))))
    (with-props st {:lit :false})))

(defn drowned
  "Returns st waterlogged and out, nil when it takes no water."
  [^long st]
  (when (and (campfire? st) (= :false (:waterlogged (block/props-of st))))
    (with-props st {:waterlogged :true :lit :false})))
