(ns collider.persist.snapshot
  "Saving and loading the world."
  (:refer-clojure :exclude [load])
  (:require [clojure.data.int-map :as i]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.game.schema :as schema]
            [collider.log :as log]
            [collider.world.chunk :as chunk]
            [malli.core :as m]
            [malli.error :as me]
            [taoensso.nippy :as nippy]
            [taoensso.nippy.compression :refer [lz4-compressor]])
  (:import (collider.java Chunk)
           (java.io DataInput DataOutput File)
           (java.nio.file CopyOption Files LinkOption OpenOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute)))

(set! *warn-on-reflection* true)

(def ^:const format-version 10)

(nippy/extend-freeze Chunk ::chunk [^Chunk c ^DataOutput out]
  (chunk/save-chunk! c out))

(nippy/extend-thaw ::chunk [^DataInput in]
  (chunk/load-chunk in))

(def ^:private freeze-opts {:compressor lz4-compressor})

(defprotocol Store
  (put-chunk! [this id payload])
  (get-chunk [this id])
  (put-meta! [this m])
  (load [this])
  (flush! [this]))

(defn- write-atomically! ^long [file ^bytes data]
  (let [^Path target (.toPath (io/file file))
        dir (or (.getParent target) (.toPath (io/file ".")))]
    (Files/createDirectories dir (make-array FileAttribute 0))
    (let [tmp (Files/createTempFile dir "world-" ".tmp" (make-array FileAttribute 0))
          ^OpenOption/1 open-opts (make-array OpenOption 0)]
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
                  ^CopyOption/1
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

(defn- stored-ids [dir]
  (into (i/int-set) (map chunk-id-of) (chunk-files dir)))

(defn- read-store [dir]
  (let [^File d (io/file dir)]
    (when (.isDirectory d)
      (when-let [m (read-edn (meta-file d))]
        (assoc m :stored (stored-ids d))))))

(defrecord FileStore [dir]
  Store
  (put-chunk! [_ id payload] (write-atomically! (chunk-file dir id) (nippy/freeze payload freeze-opts)))
  (get-chunk [_ id] (read-frozen (chunk-file dir id)))
  (put-meta! [_ m] (backup-meta! dir) (write-atomically! (meta-file dir) (edn-bytes m)))
  (load [_] (read-store dir))
  (flush! [_] nil)
  Object
  (toString [_] (str dir)))

(defn file-store [dir] (->FileStore dir))

(defn snapshot
  "Returns the world as a store keeps it.
  It holds every loaded chunk and what belongs to it."
  [world]
  (let [entry (fn [id] [id (schema/chunk-payload world id)])]
    (assoc (schema/snapshot world)
      :format format-version
      :chunks (into {} (map entry) (keys (:chunks world))))))

(defn- meta-of [snap]
  (assoc (dissoc snap :chunks) :format format-version))

(defn- written [n]
  (if (number? n) (long n) 0))

(defn- write-chunks! [store chunks]
  (reduce + 0 (map (fn [[id c]] (written (put-chunk! store id c))) chunks)))

(defn write-snapshot!
  "Writes a snapshot to a store and returns how many bytes it took."
  [store snap]
  (+ (long (write-chunks! store (:chunks snap)))
     (written (put-meta! store (meta-of snap)))))

(def ^:private chunk-keys
  [:chunks :entities :block-ticks :block-entities])

(defn world-of
  "Returns the world a snapshot holds.
  It holds its chunks and what belongs to them."
  [snap]
  (let [empty-parts (select-keys schema/initial-world chunk-keys)
        base (schema/world-of (dissoc snap :chunks :stored))]
    (reduce-kv schema/with-chunk
               (merge empty-parts base)
               (:chunks snap))))

(defn- check-format! [store m]
  (when-not (= format-version (:format m))
    (throw (ex-info (str "snapshot " store " has format " (pr-str (:format m))
                         ", this server writes format " format-version
                         " - move the world aside or start with a fresh save directory")
                    {:found (:format m) :expected format-version :store (str store)}))))

(defn- complaint [m [k msgs]]
  (str k " " (str/join ", " msgs)
       (when (contains? m k) (str ", got " (pr-str (get m k))))))

(defn- check-meta! [store m]
  (when-let [errors (me/humanize (m/explain schema/Meta m))]
    (throw (ex-info (str "snapshot " store " has bad meta")
                    {:what (str "snapshot " store " has bad meta")
                     :why  (map #(complaint m %) errors)}))))

(defn- read-meta [store]
  (try (load store)
       (catch Throwable t
         (log/warn "snapshot: read failed" (str store)
                   "-" (.getMessage t))
         nil)))

(defn load-snapshot [store]
  (when-let [m (read-meta store)]
    (check-format! store m)
    (check-meta! store m)
    (assoc (world-of (dissoc m :chunks))
           :stored (or (:stored m) (i/int-set)))))

(defn start-saver []
  (agent {:chunks nil :meta nil :writes 0} :error-mode :continue))

(defn changed-chunks [old new]
  (remove (fn [[k v]] (= v (get old k))) new))

(def ^:private clock-keys [:tick :time-ms :time-of-day])

(defn- timeless [m]
  (-> (apply dissoc m clock-keys)
      (update :block-ticks #(into #{} (mapcat second) %))))

(defn- meta-changed? [a b]
  (not= (timeless a) (timeless b)))

(defn- write-changes! [state store snap m changed]
  (try
    (let [n (+ (long (write-chunks! store changed))
               (written (put-meta! store m)))]
      (log/info "snapshot: saved" (count changed) "chunks,"
                (log/human-bytes n) "to" (str store))
      (-> state
          (assoc :chunks (:chunks snap) :meta m)
          (update :writes inc)))
    (catch Throwable t
      (log/warn "snapshot: write failed -" (.getMessage t))
      state)))

(defn- save! [state store world]
  (let [snap (snapshot world)
        m (meta-of snap)
        changed (changed-chunks (:chunks state) (:chunks snap))]
    (if (and (empty? changed) (not (meta-changed? m (:meta state))))
      state
      (write-changes! state store snap m changed))))

(defn- chunk-name ^String [id]
  (let [[cx cz] (chunk/id->pos id)] (str "chunk " cx "," cz)))

(defn- stored! [state store id payload]
  (try
    (put-chunk! store id payload)
    (update state :chunks dissoc id)
    (catch Throwable t
      (log/warn (chunk-name id) "was unloaded but not saved,"
                "its changes are lost -" (.getMessage t))
      state)))

(defn store-chunk!
  "Saves an unloaded chunk.
  The save waits for every save and read asked for before it."
  [saver store id payload]
  (send-off saver stored! store id payload))

(defn- read-chunk [store id]
  (try (get-chunk store id)
       (catch Throwable t
         (log/warn (chunk-name id) "could not be read and starts over"
                   "as a new chunk -" (.getMessage t))
         nil)))

(defn- fetched! [state store id deliver]
  (deliver (read-chunk store id))
  state)

(defn fetch-chunk!
  "Reads a saved chunk and gives it to deliver.
  The read waits for every save asked for before it. deliver
  gets nil when the chunk cannot be read."
  [saver store id deliver]
  (send-off saver fetched! store id deliver))

(defn fetch-chunk-now!
  "Returns a saved chunk on the calling thread.
  Every save asked for before it lands first. nil comes back
  when the chunk cannot be read."
  [saver store id]
  (await saver)
  (read-chunk store id))

(defn request-save! [saver store world]
  (when saver
    (send-off saver save! store world)
    true))

(defn await-saver!
  ([saver] (await-saver! saver 2000))
  ([saver ms] (if saver (await-for ms saver) true)))

(defn stop-saver!
  "Writes the world one last time and waits for the saver. Returns
  true when it finished in time."
  [saver store world]
  (request-save! saver store world)
  (let [ok (await-saver! saver)]
    (flush! store)
    ok))
