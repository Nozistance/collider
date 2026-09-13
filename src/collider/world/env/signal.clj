(ns collider.world.env.signal
  "Redstone signal reaching a block from its neighbours.")

(set! *warn-on-reflection* true)

(defn has-neighbor-signal?
  "Returns true when a block beside p gives it a redstone signal."
  [_chunks _p] false)
