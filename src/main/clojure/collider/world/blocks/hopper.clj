(ns collider.world.blocks.hopper
  "A hopper stops while a neighbour powers it."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.signal :as signal]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(defn- open? [^long st]
  (= :true (:enabled (block/props-of st))))

(defn- wanted? [chunks p]
  (not (signal/has-neighbor-signal? chunks p)))

(defn- changes [chunks p _]
  (let [st (chunk/at chunks p)
        on (wanted? chunks p)]
    (when (not= on (open? st))
      [[p (block/with st :enabled (block/flag on)) []
        update/clients]])))

(defn- wake [chunks _ _ p _ _]
  (when (not= (wanted? chunks p) (open? (chunk/at chunks p)))
    :neighbor))

(def rule
  {:name   :hopper
   :match? (fn [_ st _] (= :hopper (block/type-of st)))
   :pass   :neighbor
   :place? false
   :wake   wake
   :reshape changes})
