(ns collider.game.systems.blocks.redstone
  "Turning redstone wire from a cross to a dot and stepping the delay
  of a repeater by hand."
  (:require [collider.game.changes :as changes]
            [collider.game.mode :as game-mode]
            [collider.world.block :as block]
            [collider.world.blocks.diode :as diode]
            [collider.world.blocks.wire :as wire]))

(set! *warn-on-reflection* true)

(defn turned-deltas
  "Returns the deltas of player eid turning the wire or the repeater
  at pos, nil when it does not turn."
  [world eid pos]
  (let [chunks (:chunks world)
        st (changes/block-at world pos)]
    (when (game-mode/may-build? (get-in world [:entities eid]))
      (cond
        (wire/wire? st)
        (when-let [st' (wire/toggled chunks pos st)]
          (changes/change-deltas
            world (into [[pos st' nil 3]]
                        (wire/told-on-turn chunks pos st st'))))
        (= :repeater (block/type-of st))
        (changes/change-deltas world [[pos (diode/stepped st) nil 3]])))))
