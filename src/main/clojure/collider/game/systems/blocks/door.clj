(ns collider.game.systems.blocks.door
  "Opening and closing doors, trapdoors and fence gates."
  (:require [collider.data :as data]
            [collider.game.changes :as changes]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:private openable-types
  (into #{:fence-gate}
        (concat block/door-types block/trapdoor-types)))

(defn opens?
  "Returns true when a hand opens and closes the block at pos."
  [world pos]
  (let [cur (changes/block-at world pos)]
    (and (contains? openable-types (block/type-of cur))
         (data/by-hand? (block/block-of cur)))))

(def ^:private ^:const shaped-only
  (bit-or update/clients update/immediate))

(defn- flipped [st]
  (let [open? (= :true (:open (block/props-of st)))]
    (block/with st :open (block/flag (not open?)))))

(defn- gate-toggled [world eid pos state]
  (let [{:keys [facing open]} (block/props-of state)
        yaw (get-in world [:entities eid :yaw] 0.0)
        side (dir/player-direction yaw)
        turn? (and (= :false open) (= facing (dir/opposite side)))
        facing' (if turn? side facing)]
    (block/with (flipped state) :facing facing')))

(defn- trapdoor-toggled [pos state]
  (let [st' (flipped state)
        wet? (= :true (:waterlogged (block/props-of st')))]
    [pos st' (when wet? [:fluid-tick]) update/clients]))

(defn- toggled [world eid pos state]
  (case (block/type-of state)
    (:door :weathering-copper-door)
    [pos (flipped state) nil shaped-only]
    (:trapdoor :weathering-copper-trapdoor) (trapdoor-toggled pos state)
    :fence-gate [pos (gate-toggled world eid pos state) nil shaped-only]))

(defn- open-sound [world pos state open?]
  (let [kind (data/open-sound (block/block-of state) open?)
        pitch (random/hinge-pitch [(:tick world) pos :door])]
    (out/block-sound kind pos 1.0 pitch)))

(defn toggle-deltas
  "Returns the deltas of player eid using the door, trapdoor or gate
  state at pos."
  [world eid pos state]
  (let [c (toggled world eid pos state)
        open? (= :true (:open (block/props-of (c 1))))]
    (concat (changes/change-deltas world [c])
            [(out/except eid (open-sound world pos state open?))]
            (signal/game-event (if open? :block-open :block-close)
                               pos eid))))
