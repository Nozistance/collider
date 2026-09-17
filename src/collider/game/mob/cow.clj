(ns collider.game.mob.cow
  "Cow milking."
  (:require [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]))

(set! *warn-on-reflection* true)

(def ^:private spec (animal/spec animal/goals))

(defn brain [world eid e t tempters] (animal/brain spec world eid e t tempters))

(defn- has? [p item]
  (some #(= item (:item %)) (vals (:inventory p))))

(defn- milked [peid p e]
  (cons (out/except peid (out/sound :cow/milk (:pos e) 1.0 1.0))
        (when-not (has? p :milk-bucket)
          (for [[slot stack] (first (items/add-stack (or (:inventory p) {}) {:item :milk-bucket :count 1}))]
            [:set-slot peid slot stack]))))

(defn milk-deltas
  "Returns the deltas for players who milk a cow. A player gets a milk bucket
   only when the inventory has none yet. Nothing drops when it is full."
  [world events _]
  (animal/on-interact world events
                      (fn [peid p _ e]
                        (when (and (= :cow (:type e)) (not (mobs/baby? e))
                                   (contains? (sense/hands-of p) :bucket))
                          (milked peid p e)))))
