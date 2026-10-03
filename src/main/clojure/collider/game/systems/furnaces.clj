(ns collider.game.systems.furnaces
  "Furnaces, blast furnaces and smokers cooking."
  (:require [collider.game.block.furnace :as furnace]
            [collider.game.changes :as changes]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- lit? [^long st] (= :true (:lit (block/props-of st))))

(defn tick-deltas
  [world [pos e]]
  (let [st (chunk/at (:chunks world) pos)
        [e' burns?] (furnace/tick e)]
    (concat (when (not= e e') [[:set-block-entity pos e']])
            (when (not= burns? (lit? st))
              (let [st' (block/with st :lit (block/flag burns?))]
                (changes/set-deltas world [[pos st']]))))))
