(ns collider.persist.snapshot.store
  "Stores of a world."
  (:refer-clojure :exclude [load]))

(defprotocol Store
  (put-chunk! [this dim id payload])
  (get-chunk [this dim id])
  (put-meta! [this m])
  (load [this])
  (flush! [this]))

(defrecord FileStore [dir]
  Object
  (toString [_] (str dir)))
