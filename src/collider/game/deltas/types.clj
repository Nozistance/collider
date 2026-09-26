(ns collider.game.deltas.types
  "The record of the deltas of one tick.")

(defrecord Deltas [world entities out input])
