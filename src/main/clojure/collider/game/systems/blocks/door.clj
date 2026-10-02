(ns collider.game.systems.blocks.door
  "Opening and closing doors, trapdoors and fence gates."
  (:require [collider.data :as data]
            [collider.game.changes :as changes]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private openable-types
  (into #{:fence-gate}
        (concat block/door-types block/trapdoor-types)))

(defn opens? [world pos]
  (let [cur (changes/block-at world pos)]
    (and (contains? openable-types (block/type-of cur))
         (data/by-hand? (block/block-of cur)))))

(defn- flipped [st & kvs]
  (let [props (block/props-of st)
        open (if (= :true (:open props)) :false :true)]
    (block/state (block/block-of st)
                 (apply assoc props :open open kvs))))

(defn- door-toggled [world pos state]
  (let [st' (flipped state)]
    (if-let [[ppos pst] (connect/partner (:chunks world) pos state)]
      [[pos st'] [ppos (flipped pst)]]
      [[pos st']])))

(defn- gate-toggled [world eid pos state]
  (let [{:keys [facing open]} (block/props-of state)
        yaw (get-in world [:entities eid :yaw] 0.0)
        dir (dir/player-direction yaw)
        turn? (and (= :false open) (= facing (dir/opposite dir)))]
    [[pos (flipped state :facing (if turn? dir facing))]]))

(defn- toggled [world eid pos state]
  (case (block/type-of state)
    (:door :weathering-copper-door) (door-toggled world pos state)
    (:trapdoor :weathering-copper-trapdoor) [[pos (flipped state)]]
    :fence-gate (gate-toggled world eid pos state)))

(defn- open-sound [world pos state open?]
  (let [kind (data/open-sound (block/block-of state) open?)
        pitch (random/hinge-pitch [(:tick world) pos :door])]
    (out/block-sound kind pos 1.0 pitch)))

(defn toggle-deltas [world eid pos state]
  (let [changes (toggled world eid pos state)
        st' (second (first changes))
        open? (= :true (:open (block/props-of st')))]
    (conj (changes/change-deltas world changes)
          (out/except eid (open-sound world pos state open?)))))
