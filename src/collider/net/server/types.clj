(ns collider.net.server.types
  "A connection of a player."
  (:import (java.net Socket)
           (java.util.concurrent BlockingQueue)
           (java.util.concurrent.atomic AtomicBoolean)))

(defrecord Conn
  [^Socket sock ^BlockingQueue q st ^AtomicBoolean closing])
