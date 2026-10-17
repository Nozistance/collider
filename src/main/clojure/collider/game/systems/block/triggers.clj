(ns collider.game.systems.block.triggers
  "The block events of a level, run after its block ticks."
  (:require [collider.game.areas :as areas]
            [collider.game.block.instrument :as instrument]
            [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private taken
  "What a block does on its event, by type. It gives the deltas, or
  nil when the block takes no event."
  {:note (fn [pos st _ _] (instrument/sounded pos st))
   :bell (fn [_ _ _ _] [])})

(defn- ran [w [pos b x y]]
  (let [st (chunk/at (:chunks w) pos)
        f (taken (block/type-of st))]
    (when (and f (= b (block/block-of st)))
      (when-let [ds (f pos st x y)]
        (conj ds (out/all (out/block-event pos x y)))))))

(defn block-events
  "Returns the deltas of the block events queued in w, in order, and
  keeps those whose chunk does not tick."
  {:wake {:keys [:block-events]}}
  [w _d]
  (deltas/of-vec
    (when-let [es (seq (:block-events w))]
      (let [active (areas/ticking-chunks w)
            {now true later false}
            (group-by #(boolean (areas/active-at? active (first %))) es)]
        (into [[:block-events-ran (vec later)]] (mapcat #(ran w %)) now)))))
