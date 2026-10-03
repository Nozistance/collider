(ns collider.game.systems.compasses
  "Lodestone compasses that lose their lodestone."
  (:require [collider.game.slots :as slots]
            [collider.game.stack :as stack]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ticked-slots
  (range (:head slots/armor) (inc slots/offhand)))

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

(defn player-deltas
  "Returns the deltas that clear the target of each compass in the
  inventory of player eid whose lodestone is gone."
  [world [eid e]]
  (let [inv (:inventory e)]
    (keep #(slot-delta world eid inv %) ticked-slots)))
