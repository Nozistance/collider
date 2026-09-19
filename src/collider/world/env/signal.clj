(ns collider.world.env.signal
  "Redstone signal reaching a block, and the game events a world hears.")

(set! *warn-on-reflection* true)

(defn has-neighbor-signal? [_chunks _p] false)

(defn game-event
  "Returns the deltas of a game event heard at pos, none yet.
  Vibrations and sculk are ticket 036; the callers are vanilla and
  wait for the listeners."
  [_kind _pos _source]
  nil)
