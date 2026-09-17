(ns collider.game.systems.furnaces
  "Furnaces, blast furnaces and smokers cooking in ticking chunks."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.furnace :as furnace]
            [collider.game.state :as state]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- furnaces [world]
  (let [active (state/active-chunks world)]
    (for [[cid entries] (:block-entities world)
          :when (contains? active cid)
          [pos e] entries
          :when (contains? be/furnace-kinds (:kind e))]
      [pos e])))

(defn- with-lit ^long [^long st lit?]
  (block/state (block/block-of st)
               (assoc (block/props-of st)
                      :lit (if lit? :true :false))))

(defn- furnace-deltas [world [pos e]]
  (let [st (chunk/chunks-get-block (:chunks world) pos)
        [e' lit?] (furnace/tick e)]
    (concat (when (not= e e') [[:set-block-entity pos e']])
            (when (not= lit? (= :true (:lit (block/props-of st))))
              [[:set-blocks [[pos (with-lit st lit?)]]]]))))

(defn furnace-cooking [world _d]
  [#(mapcat (partial furnace-deltas world) (furnaces world))])
