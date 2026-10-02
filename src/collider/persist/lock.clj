(ns collider.persist.lock
  "The lock that keeps a saved world to one process."
  (:require [clojure.java.io :as io])
  (:import (java.io Closeable File)
           (java.nio ByteBuffer)
           (java.nio.channels
             FileChannel OverlappingFileLockException)
           (java.nio.charset StandardCharsets)
           (java.nio.file OpenOption StandardOpenOption)))

(set! *warn-on-reflection* true)

(def ^:private file-name "session.lock")

(def ^:private snowman
  (.getBytes "\u2603" StandardCharsets/UTF_8))

(def ^:private elsewhere
  "Stop it first, or point :save-dir at another world.")

(defn- held [dir ^File f]
  (ex-info "world locked"
           {:what (str "the world in " dir " is in use")
            :why [(str "Another server or command holds "
                       (.getPath f) ".")]
            :command elsewhere}))

(def ^:private create-write
  (into-array OpenOption
              [StandardOpenOption/CREATE StandardOpenOption/WRITE]))

(defn- opened ^FileChannel [^File f]
  (io/make-parents f)
  (FileChannel/open (.toPath f) ^"[Ljava.nio.file.OpenOption;"
                    create-write))

(defn- try-lock [^FileChannel ch]
  (try (.tryLock ch)
       (catch OverlappingFileLockException _ nil)))

(defn lock!
  "Takes the lock of the world in dir, as the vanilla session.lock,
  and returns it to close. Throws with words when another holds it."
  ^Closeable [dir]
  (let [f (io/file dir file-name)
        ch (opened f)]
    (when-not (try-lock ch)
      (.close ch)
      (throw (held dir f)))
    (.write ch (ByteBuffer/wrap snowman))
    (.force ch true)
    ch))
