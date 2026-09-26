(ns collider.persist.snapshot.store
  "Where a world is kept: the protocol of a store and the record of
  the one on disk."
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
