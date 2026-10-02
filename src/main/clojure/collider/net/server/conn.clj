(ns collider.net.server.conn
  "A connection of a player."
  (:import (java.net Socket)
           (java.util.concurrent BlockingQueue)
           (java.util.concurrent.atomic AtomicBoolean)))

(defrecord Conn
  [^Socket sock ^BlockingQueue queue props ^AtomicBoolean closing])
