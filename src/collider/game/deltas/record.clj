(ns collider.game.deltas.record
  "The record of the deltas of one tick.")

(defrecord Deltas [world entities out input])
