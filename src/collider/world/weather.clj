(ns collider.world.weather)

(set! *warn-on-reflection* true)

(defn raining? [_ctx] false)

(defn thundering? [_ctx] false)

(defn raining-at? [_chunks _p] false)
