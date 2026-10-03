(ns collider.game.systems.furnaces
  "Furnaces, blast furnaces and smokers cooking."
  (:require [collider.game.block.furnace :as furnace]
            [collider.game.changes :as changes]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- with-lit ^long [^long st lit?]
  (block/state (block/block-of st)
               (assoc (block/props-of st)
                      :lit (if lit? :true :false))))

(defn tick-deltas
  "Returns the deltas of one tick of the furnace e at pos."
  [world [pos e]]
  (let [st (chunk/at (:chunks world) pos)
        [e' lit?] (furnace/tick e)]
    (concat (when (not= e e') [[:set-block-entity pos e']])
            (when (not= lit? (= :true (:lit (block/props-of st))))
              (let [st' (with-lit st lit?)]
                (changes/set-deltas world [[pos st']]))))))
