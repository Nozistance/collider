(ns collider.game.systems.block.events
  "Block events: what a block queues to do later in its tick."
  (:require [collider.game.deltas :as deltas]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def triggers
  "The block event of each block: level, pos, action and param to
  deltas, as BlockBehaviour.triggerEvent."
  {})

(defn- run-event [w [pos b action param]]
  (let [st (chunk/chunks-get-block (:chunks w) pos)
        f (triggers b)]
    (when (and f (= b (block/block-of st)))
      (f w pos action param))))

(defn block-events
  "Runs the block events queued in the level, first in first out."
  {:wake {:keys [:block-events]}}
  [w _d]
  (deltas/of-vec
    (when-let [q (seq (:block-events w))]
      (into [[:block-events-run]] (mapcat #(run-event w %)) q))))
