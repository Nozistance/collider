(ns collider.persist.atomic-file
  "A file replaced whole. After a crash it holds the old bytes or the
  new ones, never a mix."
  (:import (java.nio ByteBuffer)
           (java.nio.channels FileChannel)
           (java.nio.file CopyOption Files OpenOption Path)
           (java.nio.file StandardCopyOption StandardOpenOption)
           (java.nio.file.attribute FileAttribute)))

(set! *warn-on-reflection* true)

(def ^:private windows?
  (.startsWith (System/getProperty "os.name") "Windows"))

(defn- open ^FileChannel [^Path p & opts]
  (FileChannel/open p ^"[Ljava.nio.file.OpenOption;"
                    (into-array OpenOption opts)))

(defn- write-all! [^Path p ^bytes data]
  (with-open [c (open p StandardOpenOption/CREATE
                      StandardOpenOption/WRITE
                      StandardOpenOption/TRUNCATE_EXISTING)]
    (let [b (ByteBuffer/wrap data)]
      (while (.hasRemaining b) (.write c b)))
    (.force c true)))

(defn- sync-dir!
  "Makes the entries of directory dir durable. Windows cannot open a
  directory, and its file system journals the entries."
  [^Path dir]
  (when-not windows?
    (with-open [c (open dir StandardOpenOption/READ)]
      (.force c true))))

(defn- tmp-of ^Path [^Path target]
  (.resolveSibling target (str (.getFileName target) ".tmp")))

(def ^:private replacing
  [StandardCopyOption/ATOMIC_MOVE
   StandardCopyOption/REPLACE_EXISTING])

(defn- move! [^Path from ^Path to]
  (Files/move from to ^"[Ljava.nio.file.CopyOption;"
              (into-array CopyOption replacing)))

(defn put!
  "Replaces file target with data. After a crash the file is the old
  one or the new one. The new one is durable on return."
  [^Path target ^bytes data]
  (let [dir (.getParent (.toAbsolutePath target))
        tmp (tmp-of target)]
    (Files/createDirectories dir (make-array FileAttribute 0))
    (try (write-all! tmp data)
         (catch Exception e
           (Files/deleteIfExists tmp)
           (throw e)))
    (move! tmp target)
    (sync-dir! dir)))
