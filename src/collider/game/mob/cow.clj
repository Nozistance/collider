(ns collider.game.mob.cow
  "Cow milking and calf variants."
  (:require [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]))

(set! *warn-on-reflection* true)

(defn- calf-variant [t eid a b]
  (if (< (animal/rnd t eid :variant) 0.5) (:color a) (:color b)))

(def spec
  "The cow's goals; a calf takes the coat of either parent."
  (animal/spec animal/goals calf-variant))

(defn brain [world eid e t tempters]
  (animal/brain spec world eid e t tempters))

(def ^:private milk {:item :milk-bucket :count 1})

(defn- milked [world peid p]
  (cons (out/except peid (out/sound :cow/milk (:pos p) 1.0 1.0))
        (items/filled-result-deltas world peid milk)))

(defn- milkable? [p e]
  (and (= :cow (:type e)) (not (mobs/baby? e))
       (contains? (sense/hands-of p) :bucket)))

(defn milk-deltas
  "Returns the deltas for players who milk a cow with a bucket.
  A calf gives no milk."
  [world events _]
  (let [f (fn [peid p _ e]
            (when (milkable? p e) (milked world peid p)))]
    (animal/on-interact world events f)))
