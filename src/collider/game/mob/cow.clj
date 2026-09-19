(ns collider.game.mob.cow
  "Cow milking and calf variants."
  (:require [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]))

(set! *warn-on-reflection* true)

(defn- calf-variant [t eid a b]
  (if (< (animal/rnd t eid :variant) 0.5) (:color a) (:color b)))

(def spec
  "The cow's goals; a calf takes the coat of either parent."
  (animal/spec animal/goals calf-variant))

(defn brain
  "Returns the cow's next state and deltas for one tick."
  [world eid e t tempters]
  (animal/brain spec world eid e t tempters))

(def ^:private milk {:item :milk-bucket :count 1})

(defn- milked [world peid p hand]
  (cons (out/except peid (out/sound :cow/milk (:pos p) 1.0 1.0))
        (items/filled-result-deltas world peid milk false hand)))

(defn milk-result
  "Returns what a bucket does to a grown cow: it fills with milk.
  A calf gives none."
  [{:keys [world peid p hand e item]}]
  (when (and (= :bucket item) (not (mobs/baby? e)))
    {:result :success :deltas (milked world peid p hand)}))
