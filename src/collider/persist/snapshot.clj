(ns collider.persist.snapshot
  (:refer-clojure :exclude [load])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [collider.game.schema :as schema]
            [collider.log :as log]
            [collider.world.chunk :as chunk]
            [taoensso.nippy :as nippy])
  (:import (collider.world.chunk Section)
           (java.io File)
           (java.nio.file CopyOption Files LinkOption OpenOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute)))

(set! *warn-on-reflection* true)

(def ^:const format-version 7)
(nippy/extend-freeze Section ::section [^Section s out]
                     (nippy/freeze-to-out! out (.blocks s))
                     (nippy/freeze-to-out! out (.block-light s))
                     (nippy/freeze-to-out! out (.sky-light s)))

(nippy/extend-thaw ::section [in]
                   (let [blocks (nippy/thaw-from-in! in)
                         block-light (nippy/thaw-from-in! in)
                         sky-light (nippy/thaw-from-in! in)]
                     (chunk/->Section blocks block-light sky-light)))

(def ^:private freeze-opts {:compressor nippy/lz4-compressor})
(defprotocol Store
  (put-chunk! [this id chunk])
  (del-chunk! [this id])
  (get-chunk [this id])
  (put-meta! [this m])
  (load [this])
  (flush! [this]))

(defn- write-atomically! ^long [file ^bytes data]
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
          (throw t))))
    (alength data)))

(defn- chunk-file ^File [dir id]
  (let [[cx cz] (chunk/id->pos id)]
    (io/file dir "chunks" (str cx "_" cz ".chunk"))))

(defn- meta-file ^File [dir]
  (io/file dir "meta.edn"))

(defn- backup-meta! [dir]
  (let [^Path f (.toPath (meta-file dir))]
    (when (Files/isRegularFile f (make-array LinkOption 0))
      (Files/copy f (.resolveSibling f "meta.edn.bak")
                  ^"[Ljava.nio.file.CopyOption;"
                  (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING])))))

(defn- read-frozen [^File f]
  (when (.isFile f)
    (nippy/thaw (Files/readAllBytes (.toPath f)))))

(defn- read-edn [^File f]
  (when (.isFile f)
    (edn/read-string (slurp f))))

(defn- spaces ^String [^long n] (apply str (repeat n " ")))

(defn- edn-lines [^StringBuilder sb v ^long col]
  (if (map? v)
    (let [keys (mapv pr-str (keys v))
          width (long (reduce max 0 (map count keys)))]
      (.append sb "{")
      (doseq [[i [k x]] (map-indexed vector (map vector keys (vals v)))]
        (when (pos? (long i)) (.append sb "\n") (.append sb (spaces (inc col))))
        (.append sb ^String k)
        (.append sb (spaces (inc (- width (count k)))))
        (edn-lines sb x (+ col 2 width)))
      (.append sb "}"))
    (.append sb (pr-str v))))

(defn- edn-bytes ^bytes [m]
  (let [sb (StringBuilder.)]
    (binding [*print-length* nil *print-level* nil] (edn-lines sb m 0))
    (.append sb "\n")
    (.getBytes (str sb) "UTF-8")))

(defn- chunk-id-of [^File f]
  (let [[cx cz] (.split (subs (.getName f) 0 (- (count (.getName f)) 6)) "_")]
    (chunk/pos->id (parse-long cx) (parse-long cz))))

(defn- chunk-files [dir]
  (filter #(re-matches #"-?\d+_-?\d+\.chunk" (.getName ^File %))
          (or (.listFiles (io/file dir "chunks")) (make-array File 0))))

(defn- read-chunks [dir]
  (into {} (keep (fn [f] (when-let [c (read-frozen f)] [(chunk-id-of f) c])))
        (chunk-files dir)))

(defn- read-store [dir]
  (let [^File d (io/file dir)]
    (cond
      (.isFile d) (log/info "snapshot:" (str d) "is a single-file world of an older layout"
                            "- starting fresh")
      (.isDirectory d) (when-let [m (read-edn (meta-file d))]
                         (assoc m :chunks (read-chunks d))))))

(defrecord FileStore [dir]
  Store
  (put-chunk! [_ id c] (write-atomically! (chunk-file dir id) (nippy/freeze c freeze-opts)))
  (del-chunk! [_ id] (Files/deleteIfExists (.toPath (chunk-file dir id))))
  (get-chunk [_ id] (read-frozen (chunk-file dir id)))
  (put-meta! [_ m] (backup-meta! dir) (write-atomically! (meta-file dir) (edn-bytes m)))
  (load [_] (read-store dir))
  (flush! [_] nil)
  Object
  (toString [_] (str dir)))

(defn file-store [dir] (->FileStore dir))
(defn snapshot [world]
  (assoc (schema/snapshot world) :format format-version))

(defn- meta-of [snap]
  (assoc (dissoc snap :chunks) :format format-version))

(defn- written [n]
  (if (number? n) (long n) 0))

(defn- write-chunks! [store chunks]
  (reduce + 0 (map (fn [[id c]] (written (put-chunk! store id c))) chunks)))

(defn write-snapshot! [store snap]
  (+ (long (write-chunks! store (:chunks snap)))
     (written (put-meta! store (meta-of snap)))))

(def world-of
  schema/world-of)

(defn- check-format! [store m]
  (when-not (= format-version (:format m))
    (throw (ex-info (str "snapshot " store " has format " (pr-str (:format m))
                         ", this server writes format " format-version
                         " - move the world aside or start with a fresh save directory")
                    {:found (:format m) :expected format-version :store (str store)}))))

(defn load-snapshot [store]
  (when-let [m (try (load store)
                    (catch Throwable t
                      (log/info "snapshot: read failed" (str store) "-" (.getMessage t))
                      nil))]
    (check-format! store m)
    (world-of m)))

(defn start-saver []
  (agent {:chunks nil :meta nil :writes 0} :error-mode :continue))

(defn changed-chunks [old new]
  (remove (fn [[k v]] (identical? v (get old k))) new))

(defn- dropped-chunks [old new]
  (remove #(contains? new %) (keys old)))

(def ^:private clock-keys [:tick :time-ms :time-of-day])

(defn- timeless [m]
  (-> (apply dissoc m clock-keys)
      (update :block-ticks #(into #{} (mapcat second) %))))

(defn- meta-changed? [a b]
  (not= (timeless a) (timeless b)))

(defn- write-changes! [state store snap m changed gone]
  (try
    (let [n (+ (long (write-chunks! store changed)) (written (put-meta! store m)))]
      (run! #(del-chunk! store %) gone)
      (log/info "snapshot: saved" (count changed) "chunks," (log/human-bytes n)
                "to" (str store))
      (-> state (assoc :chunks (:chunks snap) :meta m) (update :writes inc)))
    (catch Throwable t
      (log/info "snapshot: write failed -" (.getMessage t))
      state)))

(defn- save! [state store world]
  (let [snap (snapshot world)
        m (meta-of snap)
        changed (changed-chunks (:chunks state) (:chunks snap))
        gone (dropped-chunks (:chunks state) (:chunks snap))]
    (if (and (empty? changed) (empty? gone) (not (meta-changed? m (:meta state))))
      state
      (write-changes! state store snap m changed gone))))

(defn request-save! [saver store world]
  (when saver
    (send-off saver save! store world)
    true))

(defn await-saver!
  ([saver] (await-saver! saver 2000))
  ([saver ms] (if saver (await-for ms saver) true)))

(defn stop-saver! [saver store world]
  (request-save! saver store world)
  (let [ok (await-saver! saver)]
    (flush! store)
    ok))
