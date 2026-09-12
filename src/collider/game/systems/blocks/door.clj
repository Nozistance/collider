(ns collider.game.systems.blocks.door
  (:require [collider.data :as data]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private openable-types (into #{:fence-gate} (concat block/door-types block/trapdoor-types)))

(defn opens? [world eid pos item use-item?]
  (let [cur (edit/block-at world pos)]
    (and (not use-item?)
         (contains? openable-types (block/type-of cur))
         (data/by-hand? (block/block-of cur))
         (not (and item (get-in world [:entities eid :sneaking?]))))))

(defn- toggled [world eid pos state]
  (let [props (block/props-of state)
        self (block/block-of state)
        open? (= :true (:open props))]
    (case (block/type-of state)
      (:door :weathering-copper-door)
      (let [st' (block/state self (assoc props :open (if open? :false :true)))]
        (if-let [[ppos pst] (connect/partner (:chunks world) pos state)]
          [[pos st'] [ppos (block/state self (assoc (block/props-of pst) :open (if open? :false :true)))]]
          [[pos st']]))
      (:trapdoor :weathering-copper-trapdoor) [[pos (block/state self (assoc props :open (if open? :false :true)))]]
      :fence-gate (let [dir (dir/player-direction (get-in world [:entities eid :yaw] 0.0))
                        facing (if (and (not open?) (= (:facing props) (dir/opposite dir))) dir (:facing props))]
                    [[pos (block/state self (assoc props :open (if open? :false :true) :facing facing))]]))))

(defn toggle-deltas [world eid pos state]
  (let [changes (toggled world eid pos state)
        open? (= :true (:open (block/props-of (second (first changes)))))]
    (conj (edit/change-deltas world changes)
          (out/except eid (out/sound (data/open-sound (block/block-of state) open?) pos 1.0
                                     (random/hinge-pitch [(:tick world) pos :door]))))))
