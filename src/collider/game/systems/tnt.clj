(ns collider.game.systems.tnt
  "Primed TNT fuse, motion and blast."
  (:require [collider.game.block.tnt :as tnt]
            [collider.game.state :as state]
            [collider.vec :as v]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.phys :as phys])
  (:import (collider.world.phys Move)))

(set! *warn-on-reflection* true)

(def ^:private ^:const tnt-half (double (float 0.49)))

(def ^:private ^:const tnt-height (double (float 0.98)))

(defn- liquid-push [world pos vel]
  (liquid/entity-push (:chunks world) pos tnt-half tnt-height vel))

(defn- stuck-now [world pos]
  (motion/stuck-speed (:chunks world) pos tnt-half tnt-height))

(defn- unblock-deltas
  "Returns the deltas that cut fresh TNT loose from its origin block."
  [eid e]
  [[:merge-entity eid {:origin nil :fuse (dec (long (:fuse e)))}]])

(defn- stepped-vel [world pos [mx my mz] on-ground]
  (let [gf (if on-ground 0.7 1.0)
        v' [(* (double mx) 0.98 gf) (* (double my) 0.98)
            (* (double mz) 0.98 gf)]]
    (v/+ v' (liquid-push world pos v'))))

(defn- drift [e]
  (let [[vx vy vz] (v/+ (:vel e) (or (:kb e) [0.0 0.0 0.0]))]
    [(double vx) (- (double vy) 0.04) (double vz)]))

(defn- tnt-move ^Move [world e]
  (let [stuck (:stuck e)
        d (drift e)]
    (phys/move (:chunks world) (:pos e)
               (if stuck (mapv * d stuck) d) tnt-half tnt-height)))

(defn- moved-speed [world mv pos on-ground stuck]
  (let [vel (phys/vel mv)]
    (cond stuck [0.0 0.0 0.0]
          on-ground (motion/stepped-speed (:chunks world) pos vel)
          :else vel)))

(defn- stepped-fields [world mv e stuck']
  (let [pos (phys/pos mv)
        on-ground (phys/on-ground? mv)
        moved (moved-speed world mv pos on-ground (:stuck e))]
    (cond-> {:pos pos
             :vel (stepped-vel world pos moved on-ground)
             :on-ground on-ground
             :fuse (dec (long (:fuse e)))}
            (or stuck' (:stuck e)) (assoc :stuck stuck'))))

(defn- step-deltas [world eid e]
  (let [^Move mv (tnt-move world e)
        stuck' (stuck-now world (phys/pos mv))]
    (cond-> [[:merge-entity eid (stepped-fields world mv e stuck')]]
            (:kb e) (conj [:push eid (mapv - (:kb e))]))))

(defn- moved-pos [world e]
  (phys/pos (phys/move (:chunks world) (:pos e) (drift e)
                       tnt-half tnt-height)))

(defn- within? [p [cx cy cz] ^double reach]
  (let [dx (- (v/x p) (double cx))
        dy (- (v/y p) (double cy))
        dz (- (v/z p) (double cz))]
    (< (+ (* dx dx) (* dy dy) (* dz dz)) (* reach reach))))

(defn- later-positions
  "Returns the positions of the nearby entities that step later.
  They step after this TNT in the tick. The blast pushes them
  from these positions."
  [world eid center]
  (let [reach (+ (* 2.0 tnt/power) 2.0)
        later? (fn [oid o]
                 (and (> (long oid) (long eid))
                      (within? (:pos o) center reach)))]
    (into {}
          (keep (fn [[oid o]] (when (later? oid o) [oid (:pos o)])))
          (:entities world))))

(defn- explode-request [world eid center]
  {:center center
   :power tnt/power
   :source :tnt
   :fire? false
   :by eid
   :later (later-positions world eid center)})

(defn- explode-deltas [world eid e]
  (let [[x y z] (moved-pos world e)
        center [(double x) (+ (double y) (/ tnt-height 16.0))
                (double z)]]
    (cond-> [[:remove-entity eid]]
            (get-in world [:rules :tnt-explodes] true)
            (conj [:explode (explode-request world eid center)]))))

(defn- lit-deltas [world eid e]
  (into (unblock-deltas eid e) (step-deltas world eid e)))

(defn- tnt-entries [world]
  (let [active (state/active-chunks world)
        ticks? (fn [[_ e]]
                 (and (= :tnt (:type e))
                      (state/active-at? active (:pos e))))]
    (into [] (filter ticks?) (sort-by key (:entities world)))))

(defn- explode-step [world due]
  #(into [] (mapcat (fn [[eid e]] (explode-deltas world eid e)))
         due))

(defn- due? [[_ e]]
  (and (not (:origin e)) (<= (long (:fuse e)) 1)))

(defn- tnt-step [world [eid e]]
  (if (:origin e)
    #(lit-deltas world eid e)
    #(step-deltas world eid e)))

(defn tnt-system
  "Returns a step for every primed TNT this tick.
  A TNT lit this tick is cut loose from its block and steps
  like the rest."
  [world _d]
  (let [tnts (tnt-entries world)
        due (filterv due? tnts)]
    (into (if (seq due) [(explode-step world due)] [])
          (comp (remove due?) (map #(tnt-step world %)))
          tnts)))
