(ns collider.game.systems.blocks.redstone
  "Turning redstone wire from a cross to a dot by hand."
  (:require [collider.game.changes :as changes]
            [collider.game.mode :as game-mode]
            [collider.world.blocks.wire :as wire]))

(set! *warn-on-reflection* true)

(defn turned-deltas
  "Returns the deltas of player eid turning the wire at pos, nil when
  it does not turn."
  [world eid pos]
  (let [chunks (:chunks world)
        st (changes/block-at world pos)]
    (when (and (wire/wire? st)
               (game-mode/may-build? (get-in world [:entities eid])))
      (when-let [st' (wire/toggled chunks pos st)]
        (changes/change-deltas
          world (into [[pos st' nil 3]] (wire/told-on-turn chunks pos st st')))))))
