(ns collider.game.mob.cow
  "What makes a cow a cow: milk."
  (:require [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]))

(set! *warn-on-reflection* true)

(def ^:private spec (animal/spec {}))

(defn brain [world eid e t tempters] (animal/brain spec world eid e t tempters))

(defn- has? [p item]
  (some #(= item (:item %)) (vals (:inventory p))))

(defn- milked [world peid p e]
  (cons (out/except peid (out/sound :cow/milk (:pos e) 1.0 1.0))
        (when-not (has? p :milk-bucket)
          (let [[changes _] (items/add-stack (or (:inventory p) {}) {:item :milk-bucket :count 1})]
            (map (fn [[slot stack]] [:set-slot peid slot stack]) changes)))))

(defn milk-deltas [world events _]
  (mapcat (fn [[tag peid target]]
            (when (= :interact tag)
              (let [e (get-in world [:entities target])
                    p (get-in world [:entities peid])]
                (when (and (= :cow (:type e)) (not (mobs/baby? e))
                           (contains? (sense/hands-of p) :bucket))
                  (milked world peid p e)))))
          events))
