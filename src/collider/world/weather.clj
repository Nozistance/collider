(ns collider.world.weather)

(set! *warn-on-reflection* true)

(defn raining? [_ctx] false)

(defn thundering? [_ctx] false)

(defn raining-at? [_chunks _p] false)

(defn rain-level ^double [_ctx] 0.0)

(defn thunder-level ^double [_ctx] 0.0)
