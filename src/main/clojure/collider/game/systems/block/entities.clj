(ns collider.game.systems.block.entities
  "The tick of block entities. They tick one after another in the
  order they joined the level, and each sees the writes of the ones
  before it. One that joins during the tick waits for the next tick."
  (:require [collider.game.apply :as apply]
            [collider.game.areas :as areas]
            [collider.game.block.blockentity :as be]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.systems.brewing :as brewing]
            [collider.game.systems.campfires :as campfires]
            [collider.game.systems.furnaces :as furnaces]
            [collider.game.systems.geysers :as geysers]
            [collider.game.systems.jukebox :as jukebox]
            [collider.game.systems.signs :as signs]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- ticker [kind]
  (case kind
    (:furnace :blast-furnace :smoker) furnaces/tick-deltas
    :campfire campfires/tick-deltas
    :brewing-stand brewing/tick-deltas
    :potent-sulfur geysers/tick-deltas
    :jukebox jukebox/tick-deltas
    (:sign :hanging-sign) signs/tick-deltas
    nil))

(defn- due? [w active turn pos]
  (and (= turn (get-in w [:tickers :turn-of pos]))
       (contains? active (chunk/block-chunk pos))))

(defn- ticked [active [w :as acc] [turn pos]]
  (let [e (be/at w pos)
        f (ticker (:kind e))]
    (if (and f (due? w active turn pos))
      (apply/then acc (delta/authored (f w [pos e]) nil (:kind e)))
      acc)))

(defn block-entities
  "Returns the deltas of the tick of every ticking block entity."
  {:wake {:keys [[:tickers :turns]] :types #{:player}}}
  [world _d]
  (let [active (areas/active-chunks world)
        acc (apply/then [world deltas/empty-deltas]
                        (geysers/unsynced world))
        tick #(ticked active %1 %2)]
    (second (reduce tick acc (:turns (:tickers world))))))
