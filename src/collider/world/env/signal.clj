(ns collider.world.env.signal)

(set! *warn-on-reflection* true)

(defn has-neighbor-signal? [_chunks _p] false)

(defn signal ^long [_chunks _p _side] 0)

(defn best-neighbor-signal ^long [_chunks _p] 0)
