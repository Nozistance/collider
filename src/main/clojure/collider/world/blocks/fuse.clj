(ns collider.world.blocks.fuse
  "TNT that a neighbour's signal primes."
  (:require [collider.world.block :as block]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn- primed [chunks p {:keys [pass old rules]}]
  (when (and (not (and (= :place pass) (block/tnt? (long old))))
             (signal/has-neighbor-signal? chunks p)
             (:tnt-explodes rules true))
    [[p 0 [:prime]]]))

(def rule
  "The block rule of TNT."
  {:name    :tnt
   :match?  (fn [_chunks st _p] (block/tnt? st))
   :pass    :neighbor
   :wake    (fn [chunks _dim _tick p _old _side]
              (when (signal/has-neighbor-signal? chunks p) :neighbor))
   :reshape primed})
