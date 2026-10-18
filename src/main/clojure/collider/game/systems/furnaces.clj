(ns collider.game.systems.furnaces
  "Furnaces, blast furnaces and smokers cooking."
  (:require [collider.game.block.furnace :as furnace]
            [collider.game.changes :as changes]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- lit? [^long st] (= :true (:lit (block/props-of st))))

(defn tick-deltas
  "Returns the deltas of one tick of the furnace e at pos. When it
  lights, goes out, burns fuel or cooks, it wakes the comparators
  that read it after it shows whether it is lit."
  [world [pos e]]
  (let [st (chunk/at (:chunks world) pos)
        [e' burns?] (furnace/tick e)
        lit (when (not= burns? (lit? st))
              [[pos (block/with st :lit (block/flag burns?))]])
        ops (cond-> lit
              (or lit (not= (:items e) (:items e')))
              (conj [:output-signal pos]))]
    (changes/entities-changed-deltas
      world (when (not= e e') [[:set-block-entity pos e']]) ops
      (:tick world))))
