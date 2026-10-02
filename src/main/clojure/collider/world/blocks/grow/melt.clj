(ns collider.world.blocks.grow.melt
  "Ice and snow melting in bright light."
  (:require [collider.world.block :as block]
            [collider.world.env.attribute :as attribute]
            [collider.world.light :as light]))

(set! *warn-on-reflection* true)

(defn- block-light ^long [chunks p]
  (long (light/block-light-at chunks (p 0) (p 1) (p 2))))

(defn ice-tick
  "Returns the water that ice at p melts into in bright light.
  Where water evaporates, the ice leaves air."
  [chunks p st _roll _time world]
  (when (> (block-light chunks p) (- 11 (block/dampening st)))
    (if (attribute/water-evaporates? (:dim world))
      [[p 0]]
      [[p (block/state :water)]])))

(defn snow-tick
  "Returns the change that melts snow at p in bright light."
  [chunks p _st _roll _time _world]
  (when (> (block-light chunks p) 11)
    [[p 0]]))
