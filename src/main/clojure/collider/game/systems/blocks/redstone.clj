(ns collider.game.systems.blocks.redstone
  "Turning redstone wire from a cross to a dot, stepping the delay of
  a repeater and switching the mode of a comparator by hand."
  (:require [collider.game.changes :as changes]
            [collider.game.level :as level]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.blocks.comparator :as comparator]
            [collider.world.blocks.diode :as diode]
            [collider.world.blocks.wire :as wire]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- refreshed [world pos]
  (fn [chunks]
    (let [st (chunk/at chunks pos)]
      (if (comparator/comparator? st)
        (comparator/refreshed chunks (level/frame-signals world) pos st)
        (throw (ex-info "the switched comparator is gone"
                        {:pos pos :state st}))))))

(defn- switched-deltas [world eid pos st]
  (let [st' (comparator/stepped st)
        pitch (if (= :subtract (:mode (block/props-of st'))) 0.55 0.5)]
    (into [(out/except eid (out/block-sound :block.comparator.click pos
                                            0.3 pitch))]
          (changes/change-deltas world [[pos st' nil 2] (refreshed world pos)]))))

(defn turned-deltas
  "Returns the deltas of player eid turning the wire, the repeater or
  the comparator at pos, nil when it does not turn."
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
        (changes/change-deltas world [[pos (diode/stepped st) nil 3]])
        (comparator/comparator? st) (switched-deltas world eid pos st)))))
