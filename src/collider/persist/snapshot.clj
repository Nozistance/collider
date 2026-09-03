(ns collider.persist.snapshot
  "Snapshot of the world: chunks, non-player entities, block ticks, profiles,
   time of day. `Store` is where it lives; `FileStore` keeps it in one nippy
   file. The saver writes on request from its own thread."
  (:require [clojure.java.io :as io]
            [collider.game.schema :as schema]
            [collider.log :as log]
            [collider.world.chunk :as chunk]
            [taoensso.nippy :as nippy])
  (:import (collider.world.chunk Section)
           (java.io File)
           (java.nio.file CopyOption Files OpenOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute)))

(set! *warn-on-reflection* true)

(def ^:const format-version 5)

(nippy/extend-freeze Section ::section [^Section s out]
  (nippy/freeze-to-out! out (.blocks s))
  (nippy/freeze-to-out! out (.block-light s))
  (nippy/freeze-to-out! out (.sky-light s)))

(nippy/extend-thaw ::section [in]
  (let [blocks      (nippy/thaw-from-in! in)
        block-light (nippy/thaw-from-in! in)
        sky-light   (nippy/thaw-from-in! in)]
    (chunk/->Section blocks block-light sky-light)))

(def ^:private freeze-opts {:compressor nippy/lz4-compressor})

(defprotocol Store
  "Where the snapshot of the world lives. put! gets the snapshot map (see
   `snapshot`) from the saver thread, never concurrently; fetch returns the
   last one, or nil."
  (put! [this snapshot])
  (fetch [this]))

(defn- write-atomically! [file ^bytes data]
  (let [^Path target (.toPath (io/file file))
        dir (or (.getParent target) (.toPath (io/file ".")))]
    (Files/createDirectories dir (make-array FileAttribute 0))
    (let [tmp (Files/createTempFile dir "world-" ".tmp" (make-array FileAttribute 0))
          ^"[Ljava.nio.file.OpenOption;" open-opts (make-array OpenOption 0)]
      (try
        (Files/write tmp data open-opts)
        (Files/move tmp target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                       StandardCopyOption/REPLACE_EXISTING]))
        (catch Throwable t
          (Files/deleteIfExists tmp)
          (throw t))))))

(defrecord FileStore [file]
  Store
  (put! [_ snap] (write-atomically! file (nippy/freeze snap freeze-opts)))
  (fetch [_] (let [^File f (io/file file)]
               (when (.isFile f) (nippy/thaw (Files/readAllBytes (.toPath f))))))
  Object
  (toString [_] (str file)))

(defn file-store [file] (->FileStore file))

(defn snapshot
  "Snapshot map of the world with the format version, see collider.game.schema."
  [world]
  (assoc (schema/snapshot world) :format format-version))

(defn write-snapshot! [store snap]
  (put! store snap))

(def world-of
  "World fields from a snapshot map, to merge over initial-world."
  schema/world-of)

(defn load-snapshot [store]
  (try
    (when-let [m (fetch store)]
      (if (= format-version (:format m))
        (world-of m)
        (do (log/info "snapshot: unknown format" (:format m) "- world not loaded")
            nil)))
    (catch Throwable t
      (log/info "snapshot: read failed" (str store) "-" (.getMessage t))
      nil)))

(defn start-saver []
  (agent {:snap nil :writes 0} :error-mode :continue))

(defn- same-but-time? [a b]
  (and (identical? (:chunks a) (:chunks b))
       (= (:block-ticks a) (:block-ticks b))
       (= (:profiles a) (:profiles b))
       (= (:entities a) (:entities b))))

(defn- save! [state store world]
  (let [snap (snapshot world)]
    (if (and (same-but-time? world (:snap state))
             (= (:time-of-day world) (:time-of-day (:snap state))))
      state
      (try
        (do (write-snapshot! store snap)
            (when-not (same-but-time? world (:snap state))
              (log/info "snapshot: saved" (count (:chunks snap)) "chunks,"
                        (count (:entities snap)) "entities to" (str store)))
            (-> state (assoc :snap world) (update :writes inc)))
        (catch Throwable t
          (log/info "snapshot: write failed -" (.getMessage t))
          state)))))

(defn request-save!
  "Asks the saver to write world to store. Returns at once; a save already
   in progress finishes first."
  [saver store world]
  (when saver
    (send-off saver save! store world)
    true))

(defn await-saver!
  ([saver] (await-saver! saver 2000))
  ([saver ms] (if saver (await-for ms saver) true)))

(defn stop-saver! [saver store world]
  (request-save! saver store world)
  (await-saver! saver))
