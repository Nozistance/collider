(ns collider.persist.snapshot.store
  "Stores of a world."
  (:refer-clojure :exclude [load]))

(defprotocol Store
  (get-chunk [this dim id])
  (commit! [this m chunks-by-dim])
  (load [this])
  (close! [this]))

(defrecord FileStore [dir levels]
  Object
  (toString [_] (str dir)))
