(ns collider.persist.store.records
  "Store types."
  (:refer-clojure :exclude [load]))

(defprotocol Store
  (get-chunk [this dim id]
    "Returns the saved chunk id of level dim, or nil.")
  (commit! [this m chunks-by-dim]
    "Writes the meta m and the chunks of each level as one commit.")
  (load [this]
    "Returns the meta of the last commit, or nil when there is none.")
  (close! [this]
    "Lets go of the files the store holds open."))

(defrecord FileStore [dir levels]
  Object
  (toString [_] (str dir)))
