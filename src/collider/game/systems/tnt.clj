(ns collider.game.systems.tnt
  "Primed TNT fuse, motion and blast."
  (:require [collider.game.state :as state]
            [collider.game.block.tnt :as tnt]
            [collider.vec :as v]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.phys :as phys])
  (:import (collider.world.phys Move)))

(set! *warn-on-reflection* true)

(def ^:private ^:const tnt-half 0.49)

(def ^:private ^:const tnt-height 0.98)

(defn- liquid-push [world pos vel]
  (liquid/entity-push (:chunks world) pos tnt-half tnt-height vel))

(defn- unblock-deltas
  "Returns the deltas that cut fresh TNT loose from its origin block."
  [eid e]
  [[:merge-entity eid {:origin nil :fuse (dec (long (:fuse e)))}]])

(defn- stepped-vel [world pos [mx my mz] on-ground]
  (let [gf (if on-ground 0.7 1.0)
        v' [(* (double mx) 0.98 gf) (* (double my) 0.98) (* (double mz) 0.98 gf)]]
    (v/+ v' (liquid-push world pos v'))))

(defn- step-deltas [world eid e]
  (let [kb (:kb e) stuck (:stuck e)
        [vx vy vz] (v/+ (:vel e) (or kb [0.0 0.0 0.0]))
        drift [(double vx) (- (double vy) 0.04) (double vz)]
        ^Move mv (phys/move (:chunks world) (:pos e)
                            (if stuck (mapv * drift stuck) drift) tnt-half tnt-height)
        pos (.pos mv) on-ground (.on-ground mv)
        moved (cond stuck [0.0 0.0 0.0]
                    on-ground (motion/stepped-speed (:chunks world) pos (.vel mv))
                    :else (.vel mv))
        stuck' (motion/stuck-speed (:chunks world) pos tnt-half tnt-height)]
    (cond-> [[:merge-entity eid (cond-> {:pos       pos
                                         :vel       (stepped-vel world pos moved on-ground)
                                         :on-ground on-ground
                                         :fuse      (dec (long (:fuse e)))}
                                        (or stuck' stuck) (assoc :stuck stuck'))]]
            kb (conj [:push eid (mapv - kb)]))))

(defn- moved-pos [world e]
  (let [[vx vy vz] (v/+ (:vel e) (or (:kb e) [0.0 0.0 0.0]))]
    (.pos ^Move (phys/move (:chunks world) (:pos e)
                           [(double vx) (- (double vy) 0.04) (double vz)]
                           tnt-half tnt-height))))

(defn- within? [p [cx cy cz] ^double reach]
  (let [dx (- (v/x p) (double cx)) dy (- (v/y p) (double cy)) dz (- (v/z p) (double cz))]
    (< (+ (* dx dx) (* dy dy) (* dz dz)) (* reach reach))))

(defn- later-positions
  "Returns the positions of the nearby entities that step later.
  They step after this TNT in the tick. The blast pushes them
  from these positions."
  [world eid center]
  (let [reach (+ (* 2.0 tnt/power) 2.0)]
    (into {}
          (keep (fn [[oid o]]
                  (when (and (> (long oid) (long eid)) (within? (:pos o) center reach))
                    [oid (:pos o)])))
          (:entities world))))

(defn- explode-deltas [world eid e]
  (let [[x y z] (moved-pos world e)
        center [(double x) (+ (double y) (/ tnt-height 16.0)) (double z)]]
    (cond-> [[:remove-entity eid]]
            (get-in world [:rules :tnt-explodes] true)
            (conj [:explode {:center center
                             :power  tnt/power
                             :source :tnt
                             :fire?  false
                             :by     eid
                             :later  (later-positions world eid center)}]))))

(defn first-step
  "Returns the deltas for the TNT lit this tick."
  [world _d]
  (let [active (state/active-chunks world)]
    (into []
          (comp (filter (fn [[_ e]] (and (= :tnt (:type e)) (:origin e) (state/active-at? active (:pos e)))))
                (mapcat (fn [[eid e]] (into (unblock-deltas eid e) (step-deltas world eid e)))))
          (sort-by key (:entities world)))))

(defn tnt-system [world _d]
  (let [tnts (into [] (filter (fn [[_ e]] (= :tnt (:type e)))) (sort-by key (:entities world)))
        fresh (filterv (fn [[_ e]] (:origin e)) tnts)
        armed (into [] (remove (fn [[_ e]] (:origin e))) tnts)
        due (filterv (fn [[_ e]] (<= (long (:fuse e)) 1)) armed)
        moving (filterv (fn [[_ e]] (> (long (:fuse e)) 1)) armed)]
    (-> []
        (into (map (fn [[eid e]] #(unblock-deltas eid e))) fresh)
        (cond-> (seq due)
                (conj #(into [] (mapcat (fn [[eid e]] (explode-deltas world eid e))) due)))
        (into (map (fn [[eid e]] #(step-deltas world eid e))) moving))))
