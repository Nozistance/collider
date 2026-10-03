(ns collider.world.env.signal
  "Redstone signal at a block, and game events a world hears.")

(set! *warn-on-reflection* true)

(defn has-neighbor-signal?
  "Returns true when a neighbour of p powers it."
  [_chunks _p]
  false)

(defn game-event
  "Returns the deltas of a game event heard at pos."
  [_kind _pos _source]
  nil)
