(ns collider.persist.snapshot
  "Saving and loading the world."
  (:refer-clojure :exclude [load])
  (:require [clojure.data.int-map :as i]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.game.schema :as schema]
            [collider.game.state :as state]
            [collider.log :as log]
            [collider.world.chunk :as chunk]
            [malli.core :as m]
            [malli.error :as me]
            [taoensso.nippy :as nippy]
            [taoensso.nippy.compression :refer [lz4-compressor]])
  (:import (collider.java Chunk)
           (java.io DataInput DataOutput File)
           (java.nio.file CopyOption Files LinkOption)
           (java.nio.file OpenOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute)))

(set! *warn-on-reflection* true)

(nippy/extend-freeze Chunk ::chunk [^Chunk c ^DataOutput out]
  (chunk/save-chunk! c out))

(nippy/extend-thaw ::chunk [^DataInput in]
  (chunk/load-chunk in))

(def ^:private freeze-opts {:compressor lz4-compressor})

(defprotocol Store
  (put-chunk! [this dim id payload])
  (get-chunk [this dim id])
  (put-meta! [this m])
  (load [this])
  (flush! [this]))

(def ^:private no-attrs (make-array FileAttribute 0))

(def ^:private no-links (make-array LinkOption 0))

(defn- temp-file ^Path [^Path dir]
  (Files/createTempFile dir "world-" ".tmp" no-attrs))

(defn- write-bytes! [^Path p ^bytes data]
  (let [^OpenOption/1 opts (make-array OpenOption 0)]
    (Files/write p data opts)))

(defn- move-atomically! [^Path tmp ^Path target]
  (let [opts [StandardCopyOption/ATOMIC_MOVE
              StandardCopyOption/REPLACE_EXISTING]]
    (Files/move tmp target (into-array CopyOption opts))))

(defn- parent-dir ^Path [^Path target]
  (or (.getParent target) (.toPath (io/file "."))))

(defn- write-atomically! ^long [file ^bytes data]
  (let [^Path target (.toPath (io/file file))
        dir (parent-dir target)]
    (Files/createDirectories dir no-attrs)
    (let [tmp (temp-file dir)]
      (try
        (write-bytes! tmp data)
        (move-atomically! tmp target)
        (catch Throwable t
          (Files/deleteIfExists tmp)
          (throw t))))
    (alength data)))

(defn- dim-dir ^File [dir dim]
  (io/file dir (name dim)))

(defn- chunk-file ^File [dir dim id]
  (let [[cx cz] (chunk/id->pos id)]
    (io/file (dim-dir dir dim) "chunks" (str cx "_" cz ".chunk"))))

(defn- meta-file ^File [dir]
  (io/file dir "meta.edn"))

(defn- backup-meta! [dir]
  (let [^Path f (.toPath (meta-file dir))
        opts [StandardCopyOption/REPLACE_EXISTING]]
    (when (Files/isRegularFile f no-links)
      (Files/copy f (.resolveSibling f "meta.edn.bak")
                  ^CopyOption/1 (into-array CopyOption opts)))))

(defn- read-frozen [^File f]
  (when (.isFile f)
    (nippy/thaw (Files/readAllBytes (.toPath f)))))

(defn- read-edn [^File f]
  (when (.isFile f)
    (edn/read-string (slurp f))))

(defn- spaces ^String [^long n] (apply str (repeat n " ")))

(defn- edn-lines [^StringBuilder sb v ^long col]
  (if-not (map? v)
    (.append sb (pr-str v))
    (let [ks (mapv pr-str (keys v))
          width (long (reduce max 0 (map count ks)))]
      (.append sb "{")
      (doseq [[i k x] (map vector (range) ks (vals v))]
        (when (pos? (long i))
          (.append sb "\n")
          (.append sb (spaces (inc col))))
        (.append sb ^String k)
        (.append sb (spaces (inc (- width (count k)))))
        (edn-lines sb x (+ col 2 width)))
      (.append sb "}"))))

(defn- edn-bytes ^bytes [m]
  (let [sb (StringBuilder.)]
    (binding [*print-length* nil *print-level* nil]
      (edn-lines sb m 0))
    (.append sb "\n")
    (.getBytes (str sb) "UTF-8")))

(defn- chunk-id-of [^File f]
  (let [n (.getName f)
        [cx cz] (.split (subs n 0 (- (count n) 6)) "_")]
    (chunk/pos->id (parse-long cx) (parse-long cz))))

(defn- chunk-file? [^File f]
  (re-matches #"-?\d+_-?\d+\.chunk" (.getName f)))

(defn- chunk-files [dir dim]
  (let [fs (.listFiles (io/file (dim-dir dir dim) "chunks"))]
    (filter chunk-file? (or fs (make-array File 0)))))

(defn- stored-ids [dir dim]
  (into (i/int-set) (map chunk-id-of) (chunk-files dir dim)))

(defn- with-stored [dir levels]
  (into {} (for [[dim lm] levels]
             [dim (assoc lm :stored (stored-ids dir dim))])))

(defn- read-store [dir]
  (let [^File d (io/file dir)]
    (when (.isDirectory d)
      (when-let [m (read-edn (meta-file d))]
        (update m :levels #(with-stored d %))))))

(defrecord FileStore [dir]
  Store
  (put-chunk! [_ dim id payload]
    (write-atomically! (chunk-file dir dim id)
                       (nippy/freeze payload freeze-opts)))
  (get-chunk [_ dim id] (read-frozen (chunk-file dir dim id)))
  (put-meta! [_ m]
    (backup-meta! dir)
    (write-atomically! (meta-file dir) (edn-bytes m)))
  (load [_] (read-store dir))
  (flush! [_] nil)
  Object
  (toString [_] (str dir)))

(defn file-store
  "Returns a store that keeps a world in directory dir."
  [dir]
  (->FileStore dir))

(defn- level-snapshot [world dim]
  (let [lv (state/level world dim)
        entry (fn [id] [id (schema/chunk-payload lv id)])]
    (assoc (schema/snapshot lv :level)
      :chunks (into {} (map entry) (keys (:chunks lv))))))

(defn snapshot
  "Returns the world as a store keeps it.
  Each level holds every chunk it has loaded and what belongs to it;
  the shared part holds the players of every level."
  [world]
  (assoc (schema/snapshot world :shared)
    :levels (into {} (for [dim (keys (:levels world))]
                       [dim (level-snapshot world dim)]))))

(defn- per-level [f levels]
  (into {} (for [[dim lm] levels] [dim (f lm)])))

(defn- meta-of [snap]
  (update snap :levels #(per-level (fn [lm] (dissoc lm :chunks)) %)))

(defn- written [n]
  (if (number? n) (long n) 0))

(defn- write-chunks! [store dim chunks]
  (let [write! (fn [[id c]] (written (put-chunk! store dim id c)))]
    (reduce + 0 (map write! chunks))))

(defn- write-levels! [store chunks-by-dim]
  (reduce + 0 (for [[dim ch] chunks-by-dim]
                (write-chunks! store dim ch))))

(defn write-snapshot!
  "Writes a snapshot to a store and returns how many bytes it took."
  [store snap]
  (let [chunks (per-level :chunks (:levels snap))]
    (+ (long (write-levels! store chunks))
       (written (put-meta! store (meta-of snap))))))

(def ^:private chunk-keys
  [:chunks :entities :block-ticks :fluid-ticks :block-entities])

(def ^:private level-defaults
  (get-in schema/initial-world [:levels :overworld]))

(defn- level-world-of [tick lm]
  (let [empty-parts (select-keys level-defaults chunk-keys)
        bare (dissoc lm :chunks :stored)
        base (assoc (schema/level-of bare) :tick tick)
        start (merge empty-parts base)]
    (-> (reduce-kv schema/with-chunk start (:chunks lm))
        (dissoc :tick)
        (assoc :stored (or (:stored lm) (i/int-set))))))

(defn world-of
  "Returns the world a snapshot holds.
  Each level holds its chunks and what belongs to them; a level
  the snapshot lacks is empty."
  [snap]
  (let [shared (schema/shared-of (dissoc snap :levels))
        tick (long (:tick shared 0))]
    (assoc shared :levels
           (into (:levels schema/initial-world)
                 (for [[dim lm] (:levels snap)]
                   [dim (level-world-of tick lm)])))))

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

(defn load-snapshot
  "Returns the world store holds, or nil when it holds none.
  Throws when its meta breaks the schema."
  [store]
  (when-let [m (read-meta store)]
    (check-meta! store m)
    (world-of m)))

(defn start-saver
  "Returns an agent that writes chunks and meta in the background.
  Its meta holds the chunks handed to it but not written yet."
  []
  (agent {:chunks nil :meta nil :writes 0}
         :error-mode :continue
         :meta {:pending (atom {})}))

(defn- pending-of [saver]
  (:pending (meta saver)))

(defn- hold! [saver dim id payload]
  (when-let [p (pending-of saver)]
    (swap! p assoc [dim id] payload)))

(defn- release! [saver dim id payload]
  (when-let [p (pending-of saver)]
    (swap! p (fn [m]
               (if (identical? payload (get m [dim id]))
                 (dissoc m [dim id])
                 m)))))

(defn changed-chunks
  "Returns the entries of chunk map new that differ from old."
  [old new]
  (remove (fn [[k v]] (= v (get old k))) new))

(defn- changed-levels [old-chunks levels]
  (into {} (for [[dim lm] levels
                 :let [old (get old-chunks dim)
                       ch (changed-chunks old (:chunks lm))]
                 :when (seq ch)]
             [dim ch])))

(def ^:private clock-keys [:tick :time-ms :time-of-day])

(defn- timeless [m]
  (apply dissoc m clock-keys))

(defn- meta-changed? [a b]
  (not= (timeless a) (timeless b)))

(defn- write-changes! [state store snap m changed]
  (try
    (let [n (+ (long (write-levels! store changed))
               (written (put-meta! store m)))
          n-chunks (reduce + 0 (map count (vals changed)))]
      (log/info "snapshot: saved" n-chunks "chunks,"
                (log/human-bytes n) "to" (str store))
      (-> state
          (assoc :chunks (per-level :chunks (:levels snap))
                 :meta m)
          (update :writes inc)))
    (catch Throwable t
      (log/warn "snapshot: write failed -" (.getMessage t))
      state)))

(defn- save! [state store world]
  (let [snap (snapshot world)
        m (meta-of snap)
        changed (changed-levels (:chunks state) (:levels snap))]
    (if (and (empty? changed) (not (meta-changed? m (:meta state))))
      state
      (write-changes! state store snap m changed))))

(defn- chunk-name ^String [id]
  (let [[cx cz] (chunk/id->pos id)] (str "chunk " cx "," cz)))

(defn- stored! [state saver store dim id payload]
  (try
    (put-chunk! store dim id payload)
    (update-in state [:chunks dim] dissoc id)
    (catch Throwable t
      (log/warn (chunk-name id) "was unloaded but not saved,"
                "its changes are lost -" (.getMessage t))
      state)
    (finally (release! saver dim id payload))))

(defn store-chunk!
  "Saves an unloaded chunk.
  A read sees it at once; the write waits for every save and
  read asked for before it."
  [saver store dim id payload]
  (hold! saver dim id payload)
  (send-off saver stored! saver store dim id payload))

(defn- read-stored [store dim id]
  (try (get-chunk store dim id)
       (catch Throwable t
         (log/warn (chunk-name id) "could not be read and"
                   "starts over as a new chunk -" (.getMessage t))
         nil)))

(defn- read-chunk [saver store dim id]
  (if-let [payload (some-> (pending-of saver) deref (get [dim id]))]
    payload
    (read-stored store dim id)))

(defn- fetched! [state saver store dim id deliver]
  (deliver (read-chunk saver store dim id))
  state)

(defn fetch-chunk!
  "Reads a saved chunk and gives it to deliver.
  The read waits for every save asked for before it. deliver
  gets nil when the chunk cannot be read."
  [saver store dim id deliver]
  (send-off saver fetched! saver store dim id deliver))

(defn fetch-chunk-now!
  "Returns a saved chunk on the calling thread.
  A chunk still waiting to be written comes back as it was
  given. nil comes back when the chunk cannot be read."
  [saver store dim id]
  (read-chunk saver store dim id))

(defn request-save!
  "Asks the saver to write world to store in the background.
  Returns nil when there is no saver."
  [saver store world]
  (when saver
    (send-off saver save! store world)
    true))

(defn await-saver!
  "Waits up to ms for the saver to finish what it was given."
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
