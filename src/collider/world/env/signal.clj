(ns collider.world.env.signal
  "Redstone signal reaching a block from its neighbours.")

(set! *warn-on-reflection* true)

(defn has-neighbor-signal? [_chunks _p] false)
