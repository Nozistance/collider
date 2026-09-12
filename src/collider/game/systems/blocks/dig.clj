(ns collider.game.systems.blocks.dig
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.container :as container]
            [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.fire :as fire]))

(set! *warn-on-reflection* true)

(defn- bed-head-effect [world eid pos old]
  (when (= :foot (:part (block/props-of old)))
    (when-let [[hpos head] (connect/partner (:chunks world) pos old)]
      (out/except eid (out/break-effect hpos head)))))

(defn- door-partner-effect [world eid pos old]
  (when (contains? block/door-types (block/type-of old))
    (when-let [[ppos partner] (connect/partner (:chunks world) pos old)]
      (if (= :lower (:half (block/props-of old)))
        (out/all (out/break-effect ppos partner))
        (out/except eid (out/break-effect ppos partner))))))

(defn- jukebox-break-deltas [world pos]
  (let [e (be/at world pos)]
    (when (and (= :jukebox (:kind e)) (:record e))
      [[:spawn-entity (items/popped world (mapv + pos [0 1 0]) (:record e) :jukebox)]
       (out/all (out/level-event 1011 pos 0))])))

(defn- shulker-break-deltas [world pos]
  (let [e (be/at world pos)
        [x y z] pos]
    (when (= :shulker-box (:kind e))
      (concat
        (when (container/animation world pos) [[:shulker-anim pos nil]])
        (when (some some? (:items e))
          (let [item (block/block-of (edit/block-at world pos))]
            [[:spawn-entity (entity/item [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]
                                         (entity/pop-velocity [(:tick world) pos :shulker])
                                         (be/to-stack item e))]]))))))

(defn- lectern-break-deltas [world pos]
  (for [e (container/dropped-book world pos)] [:spawn-entity e]))

(defn- spill-deltas [world pos]
  (let [e (be/at world pos)]
    (when (contains? be/spill-kinds (:kind e))
      (mapcat (fn [[i stack]]
                (when stack
                  (map (fn [part] [:spawn-entity (items/popped world pos part [:spill i])])
                       (items/split-drop world pos stack [:spill i]))))
              (map-indexed vector (:items e))))))

(defn dig-deltas [world [eid status pos _face]]
  (let [old (edit/block-at world pos)]
    (when (or (= 0 status) (= 2 status))
      (if (pos? old)
        (into (vec (concat (jukebox-break-deltas world pos)
                           (shulker-break-deltas world pos)
                           (lectern-break-deltas world pos)
                           (spill-deltas world pos)))
              (concat (edit/change-deltas world [[pos (block/emptied old)]])
                      [(out/except eid (out/break-effect pos old))]
                      (when (fire/fire-state? old) [(out/all (out/extinguish pos))])
                      (keep identity [(bed-head-effect world eid pos old)
                                      (door-partner-effect world eid pos old)])))
        [(edit/own-change world eid pos)]))))
