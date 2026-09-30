(ns collider.game.systems.compasses
  "Lodestone compasses that lose their lodestone."
  (:require [collider.game.deltas :as deltas]
            [collider.game.stack :as stack]
            [collider.game.state :as state]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ticked-slots (range 5 46))

(def ^:private ^:const world-edge 30000000)

(defn- in-bounds? [world [x y z]]
  (and (chunk/in-level? world y)
       (<= (- world-edge) (long x)) (< (long x) world-edge)
       (<= (- world-edge) (long z)) (< (long z) world-edge)))

(defn- gone? [world pos]
  (let [chunks (:chunks world)]
    (and (contains? chunks (chunk/block-chunk pos))
         (not= :lodestone (block/block-of (chunk/at chunks pos))))))

(defn- lost? [world {:keys [target tracked]}]
  (when (and tracked target (= (:dim world) (:dimension target)))
    (let [pos (:pos target)]
      (or (not (in-bounds? world pos)) (gone? world pos)))))

(defn- slot-delta [world eid inv slot]
  (let [s (get inv slot)]
    (when (= :compass (:item s))
      (when (lost? world (stack/component s :lodestone-tracker))
        [:set-slot eid slot
         (stack/put s :lodestone-tracker
                    {:target nil :tracked true})]))))

(defn- player-deltas [world [eid e]]
  (let [inv (:inventory e)]
    (keep #(slot-delta world eid inv %) ticked-slots)))

(defn compasses
  "Returns the deltas that clear the target of each compass in a
  player inventory whose lodestone is gone."
  {:wake {:types #{:player}}}
  [world _]
  (deltas/of-vec (into [] (mapcat #(player-deltas world %))
                       (state/of-types world [:player]))))
