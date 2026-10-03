(ns collider.world.blocks.campfire
  "Campfire placement, signal smoke, and dowsing."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn campfire?
  "Returns true when st is a campfire of any kind."
  [^long st]
  (= :campfire (block/type-of st)))

(defn- smoke-source? [^long st] (= :hay-block (block/block-of st)))

(defn- at [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/at chunks p) 0))

(defn placed
  "Returns the campfire st placed at pos by a player facing yaw."
  [chunks pos st yaw]
  (let [water? (block/water? (at chunks pos))
        signal? (smoke-source? (at chunks (dir/down pos)))]
    (block/with st
                :facing (dir/player-direction yaw)
                :waterlogged (block/flag water?)
                :lit (block/flag (not water?))
                :signal-fire (block/flag signal?))))

(defn updated
  "Returns the campfire st as block self with its signal smoke set
  from the block under it. near gives the block at an offset."
  [self ^long st near]
  (let [signal? (smoke-source? (near [0 -1 0]))]
    (block/state self (assoc (block/props-of st)
                        :signal-fire (block/flag signal?)))))

(defn dowsed
  "Returns the lit campfire st put out. Returns nil when st is no
  lit campfire."
  [^long st]
  (when (and (campfire? st) (= :true (:lit (block/props-of st))))
    (block/with st :lit :false)))

(defn drowned
  "Returns the campfire st filled with water and put out, or nil
  when st is no dry campfire."
  [^long st]
  (when (and (campfire? st)
             (= :false (:waterlogged (block/props-of st))))
    (block/with st :waterlogged :true :lit :false)))
