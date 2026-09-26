(ns collider.world.env.attribute
  "Gameplay attributes of a dimension, like fast lava."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(defn value?
  "Tests whether the attribute attr of dimension dim is on.
  An attribute the dimension does not set is off."
  [dim attr]
  (true? (get-in (data/dimension-type dim) [:attributes attr])))

(defn fast-lava? [dim] (value? dim :gameplay/fast-lava))

(defn water-evaporates? [dim]
  (value? dim :gameplay/water-evaporates))
