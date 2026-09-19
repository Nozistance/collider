(ns collider.world.env.signal
  "Redstone signal at a block, and game events a world hears.")

(set! *warn-on-reflection* true)

(defn has-neighbor-signal?
  "Tests whether a neighbour of p powers it, none do yet."
  [_chunks _p]
  false)

(defn game-event
  "Returns the deltas of a game event heard at pos, none yet.
  Vibrations and sculk are ticket 036."
  [_kind _pos _source]
  nil)
