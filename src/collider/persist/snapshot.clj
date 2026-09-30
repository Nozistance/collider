(ns collider.persist.snapshot
  "Saving and loading the world."
  (:refer-clojure :exclude [load])
  (:require [clojure.data.int-map :as i]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.schedule :as schedule]
            [collider.game.schema :as schema]
            [collider.game.level :as level]
            [collider.log :as log]
            [collider.persist.region :as region]
            [collider.persist.snapshot.store :as types
             :refer [Store get-chunk commit! load close!]]
            [collider.world.chunk :as chunk]
            [malli.core :as m]
            [malli.error :as me]
            [taoensso.nippy :as nippy]
            [taoensso.nippy.compression :refer [lz4-compressor]])
  (:import (collider.world Chunk)
           (collider.persist.snapshot.store FileStore)
           (java.io DataInput DataOutput File)
           (java.nio.channels ClosedChannelException)
           (java.nio.file Files)
           (java.util.zip DataFormatException)))

(set! *warn-on-reflection* true)

(nippy/extend-freeze Chunk ::chunk [^Chunk c ^DataOutput out]
  (chunk/save-chunk! c out))

(nippy/extend-thaw ::chunk [^DataInput in]
  (chunk/load-chunk in))

(def ^:private freeze-opts {:compressor lz4-compressor})

(defn- dim-dir ^File [dir dim]
  (io/file dir (name dim)))

(defn- meta-file ^File [dir]
  (io/file dir "meta.edn"))

(defn- bak-file ^File [dir]
  (io/file dir "meta.edn.bak"))

(defn- edn-of [^bytes data]
  (let [v (edn/read-string (String. data "UTF-8"))]
    (if (map? v)
      v
      (throw (ex-info (str "holds " (pr-str (type v)) " not a map")
                      {})))))

(defn- read-edn [^File f]
  (edn-of (Files/readAllBytes (.toPath f))))

(defn- readable? [^File f]
  (and (.isFile f)
       (try (read-edn f) true
            (catch Throwable _ false))))

(defn- edn-bytes ^bytes [m]
  (binding [*print-length* nil
            *print-level* nil
            *print-namespace-maps* false
            pp/*print-right-margin* 80]
    (let [s (with-out-str (pp/pprint m))]
      (.getBytes ^String s "UTF-8"))))

(defn- bak-line [dir]
  (let [f (bak-file dir)]
    (cond (readable? f) "meta.edn.bak can be read"
          (.isFile f) "meta.edn.bak cannot be read either"
          :else "there is no meta.edn.bak")))

(defn- restore-command [dir]
  (if (readable? (bak-file dir))
    (str "Copy meta.edn.bak over meta.edn in " dir
         " to start one commit back, or move " dir " away")
    (str "Repair meta.edn in " dir " or move " dir " away")))

(defn- unreadable [dir why]
  (ex-info "unreadable meta"
           {:what    "the saved world cannot be read"
            :why     [(str "meta.edn in " dir " " why) (bak-line dir)
                      "nothing was changed on disk"]
            :command (restore-command dir)}))

(defn- saved-before? [dir]
  (or (.isFile (bak-file dir))
      (some #(.isDirectory (dim-dir dir %)) schema/dims)))

(defn- checked-meta [dir]
  (try (read-edn (meta-file dir))
       (catch Throwable t
         (let [why (str "cannot be read, " (.getMessage t))]
           (throw (unreadable dir why))))))

(defn- read-meta-file [dir]
  (cond (.isFile (meta-file dir)) (checked-meta dir)
        (saved-before? dir) (throw (unreadable dir "is missing"))
        :else nil))

(def ^:private stamp {:game data/game :layout data/layout})

(defn- stamp-line [{:keys [game layout]}]
  (str "tables " game " layout " layout))

(defn- other-tables [dir found]
  (let [saved (if found (stamp-line found) "no stamp")
        command (str "Start the build that saved it, with its"
                     " tables from: clojure -T:build tables,"
                     " or move " dir " away")]
    (ex-info "other tables"
             {:what    "the saved world needs other tables"
              :why     [(str dir " was saved with " saved)
                        (str "this server reads " (stamp-line stamp))
                        "block and item ids differ between them"
                        "nothing was changed on disk"]
              :command command})))

(defn- stamped [dir m]
  (if (= stamp (:stamp m))
    m
    (throw (other-tables dir (:stamp m)))))

(defn- opened-level [dir m dim]
  (let [gen (long (get-in m [:levels dim :regions-gen] 0))
        d (dim-dir dir dim)]
    (region/sweep! d gen)
    {:gen     gen
     :regions (into {} (map (fn [r] [(region/key-of r) r]))
                    (region/open-all d gen))}))

(defn- open-levels! [{:keys [dir levels]} m]
  (let [open (fn [dim] [dim (opened-level dir m dim)])]
    (reset! levels (into {} (map open) schema/dims))))

(defn- stamped-meta [dir]
  (some->> (read-meta-file dir) (stamped dir)))

(defn- levels-of [{:keys [dir levels] :as store}]
  (or @levels (open-levels! store (stamped-meta dir))))

(defn- stored-ids [{:keys [regions]}]
  (into (i/int-set) (mapcat region/chunks) (vals regions)))

(defn- with-stored [levels opened]
  (into {} (for [[dim lm] levels]
             [dim (-> (dissoc lm :regions-gen)
                      (assoc :stored (stored-ids (opened dim))))])))

(defn- read-store [{:keys [dir] :as store}]
  (when-let [m (stamped-meta dir)]
    (let [opened (open-levels! store m)]
      (-> (dissoc m :stamp)
          (update :levels with-stored opened)))))

(defn- frozen ^bytes [payload]
  (if (bytes? payload) payload (nippy/freeze payload freeze-opts)))

(defn- appended [made dir gen old k chunks]
  (let [made! #(do (vswap! made conj %) %)
        r (if old (region/copy old) (made! (region/create dir k gen)))]
    (doseq [[id p] chunks] (region/append! r id (frozen p)))
    (if (region/sparse? r) (made! (region/compact r gen)) r)))

(defn- replaced? [old r]
  (and old (not= (region/gen-of old) (region/gen-of r))))

(defn- retired [regions news]
  (keep (fn [[k r]] (let [old (get regions k)]
                      (when (replaced? old r) old)))
        news))

(defn- by-region [chunks]
  (group-by #(region/of (first %)) chunks))

(defn- written-level [made dir {:keys [gen regions]} chunks]
  (let [g (inc (long gen))
        add (fn [[k cs]] [k (appended made dir g (get regions k) k cs)])
        news (into {} (map add) (by-region chunks))]
    {:gen g :regions (merge regions news) :news (vals news)
     :retired (retired regions news)}))

(defn- touched-levels [{:keys [dir] :as store} chunks-by-dim]
  (let [levels (levels-of store)
        made (volatile! [])
        level (fn [[dim chunks]]
                (let [lv (get levels dim {:gen 0})]
                  [dim (written-level made (dim-dir dir dim) lv chunks)]))]
    (try (into {} (comp (filter (comp seq val)) (map level))
               chunks-by-dim)
         (catch Throwable t
           (run! region/close! @made)
           (throw t)))))

(defn- abandoned! [touched]
  (doseq [{:keys [gen news]} (vals touched) r news
          :when (= gen (region/gen-of r))]
    (region/close! r)))

(defn- forced! [touched]
  (doseq [{:keys [news]} (vals touched)] (run! region/force! news)))

(defn- manifests! [{:keys [dir]} touched]
  (doseq [[dim {:keys [gen regions]}] touched]
    (region/write-manifest! (dim-dir dir dim) gen (vals regions))))

(defn- backup-meta! [dir]
  (let [f (meta-file dir)]
    (when (readable? f)
      (region/put! (bak-file dir) (Files/readAllBytes (.toPath f))))))

(defn- write-meta! [dir m]
  (let [data (edn-bytes m)]
    (edn-of data)
    (backup-meta! dir)
    (region/put! (meta-file dir) data)
    (alength data)))

(defn- with-gens [m levels]
  (reduce-kv (fn [m dim {:keys [gen]}]
               (if (pos? (long gen))
                 (assoc-in m [:levels dim :regions-gen] gen)
                 m))
             m levels))

(defn- root! [{:keys [dir] :as store} m touched]
  (let [levels (merge (levels-of store) touched)]
    (write-meta! dir (assoc (with-gens m levels) :stamp stamp))))

(defn- published! [{:keys [dir levels]} touched]
  (swap! levels merge
         (update-vals touched #(select-keys % [:gen :regions])))
  (doseq [[dim {:keys [gen retired]}] touched]
    (run! region/close! retired)
    (region/sweep! (dim-dir dir dim) gen)))

(defn- stored-bytes [store dim id]
  (some-> (get-in (levels-of store) [dim :regions (region/of id)])
          (region/chunk-bytes id)))

(defn- close-all! [{:keys [levels]}]
  (doseq [lv (vals @levels) r (vals (:regions lv))] (region/close! r))
  (reset! levels nil))

(extend-type FileStore
  Store
  (get-chunk [store dim id]
    (some-> (try (stored-bytes store dim id)
                 (catch ClosedChannelException _
                   (stored-bytes store dim id)))
            nippy/thaw))
  (commit! [store m chunks-by-dim]
    (let [touched (touched-levels store chunks-by-dim)
          n (try (forced! touched)
                 (manifests! store touched)
                 (root! store m touched)
                 (catch Throwable t
                   (abandoned! touched)
                   (throw t)))]
      (published! store touched)
      n))
  (load [store] (read-store store))
  (close! [store] (close-all! store)))

(defn file-store
  "Returns a store that keeps a world in directory dir."
  [dir]
  (types/->FileStore dir (atom nil)))

(defn- level-snapshot [world dim]
  (let [lv (level/level world dim)
        groups (schema/chunk-entities (:entities lv))
        entry (fn [id] [id (schema/chunk-payload lv id (groups id))])]
    (assoc (schema/snapshot lv :level)
      :chunks (into {} (map entry) (keys (:chunks lv))))))

(defn snapshot
  "Returns the world as a store keeps it.
  Each level holds every chunk it has loaded and what belongs to it.
  The shared part holds the players of every level."
  [world]
  (assoc (schema/snapshot world :shared)
    :levels (into {} (for [dim (keys (:levels world))]
                       [dim (level-snapshot world dim)]))))

(defn- meta-of-world [world]
  (let [level #(schema/snapshot (level/level world %) :level)]
    (assoc (schema/snapshot world :shared)
      :levels (into {} (map (fn [dim] [dim (level dim)]))
                    (keys (:levels world))))))

(defn- per-level [f levels]
  (into {} (for [[dim lm] levels] [dim (f lm)])))

(defn- meta-of [snap]
  (update snap :levels #(per-level (fn [lm] (dissoc lm :chunks)) %)))

(defn write-snapshot!
  "Commits a snapshot to a store."
  [store snap]
  (commit! store (meta-of snap) (per-level :chunks (:levels snap))))

(def ^:private chunk-keys
  [:chunks :entities :block-ticks :fluid-ticks
   :block-entities])

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
  "Returns the world that a snapshot holds.
  A level that the snapshot lacks is empty."
  [snap]
  (let [shared (schema/shared-of (dissoc snap :levels))
        tick (long (:tick shared 0))]
    (level/synced
      (assoc shared :levels
             (into (:levels schema/initial-world)
                   (for [[dim lm] (:levels snap)]
                     [dim (level-world-of tick lm)]))))))

(defn- complaint [m [k msgs]]
  (str k " " (str/join ", " msgs)
       (when (contains? m k) (str ", got " (pr-str (get m k))))))

(defn- check-meta! [store m]
  (when-let [errors (me/humanize (m/explain schema/Meta m))]
    (throw (ex-info (str "snapshot " store " has bad meta")
                    {:what (str "snapshot " store " has bad meta")
                     :why  (map #(complaint m %) errors)}))))

(defn load-snapshot
  "Returns the world store holds, or nil when it holds none.
  Throws when its meta cannot be read or breaks the schema."
  [store]
  (when-let [m (load store)]
    (check-meta! store m)
    (world-of m)))

(defn start-saver
  "Returns a saver of chunks and meta.
  Its meta holds the chunks unloaded since the last commit and
  whether a commit is wanted."
  []
  (agent {:last nil :meta nil :writes 0 :held 0 :built 0}
         :error-mode :continue
         :meta {:pending (atom {}) :wanted (atom false)}))

(defn- pending-of [saver]
  (:pending (meta saver)))

(def ^:private ^:const pending-limit (* 8 1024 1024))

(defn- held-bytes ^long [m]
  (transduce (keep (fn [[_ [_ v]]]
                     (when (bytes? v) (alength ^bytes v))))
             + 0 m))

(defn want-commit!
  "Asks for a commit at the start of the next tick."
  [saver]
  (some-> saver meta :wanted (reset! true)))

(defn- frozen! [state saver k payload]
  (let [data (frozen payload)
        n (+ (long (:held state)) (alength data))
        keep! (fn [m] (if (identical? payload (peek (get m k)))
                        (assoc-in m [k 1] data)
                        m))]
    (swap! (pending-of saver) keep!)
    (when (< pending-limit n) (want-commit! saver))
    (assoc state :held n)))

(defn store-chunk!
  "Holds a chunk unloaded at tick until the next commit.
  A read sees it at once."
  [saver dim id tick payload]
  (swap! (pending-of saver) assoc [dim id] [tick payload])
  (send-off saver frozen! saver [dim id] payload))

(defn- held-at? [^long tick [_ [t]]]
  (<= (long t) tick))

(defn- with-held [changed levels held]
  (reduce (fn [m [[dim id] [_ v]]]
            (if (contains? (get-in levels [dim :chunks]) id)
              m
              (update m dim (fnil conj []) [id v])))
          changed held))

(defn- released! [state saver tick]
  (let [m (swap! (pending-of saver)
                 #(into {} (remove (partial held-at? tick)) %))]
    (assoc state :held (held-bytes m))))

(defn- saved-ticks [old lv k t]
  (let [ticks (k lv)
        [was by] (get old k)]
    (if (and (identical? ticks was) (= t (:t old)))
      [was by]
      [ticks (schedule/saved-by-chunk ticks t)])))

(defn- level-parts [old world dim]
  (let [lv (level/level world dim)
        t (long (:tick world 0))]
    {:t           t
     :chunks      (:chunks lv)
     :bes         (:block-entities lv)
     :groups      (schema/chunk-entities (:entities lv))
     :block-ticks (saved-ticks old lv :block-ticks t)
     :fluid-ticks (saved-ticks old lv :fluid-ticks t)}))

(defn- same-entry? [a b]
  (and (= (key a) (key b)) (identical? (val a) (val b))))

(defn- same-entries? [a b]
  (or (identical? a b)
      (and (= (count a) (count b))
           (every? true? (map same-entry? a b)))))

(defn- by-id [parts k]
  (let [v (get parts k)] (if (vector? v) (peek v) v)))

(defn- differs [old new k same?]
  (let [a (by-id new k)
        b (by-id old k)]
    (when-not (identical? a b)
      (fn [id] (not (same? (get a id) (get b id)))))))

(defn- touched [old new moved?]
  (let [groups (:groups new)
        tests (->> [[:chunks identical?] [:bes identical?]
                    [:groups same-entries?] [:block-ticks =]
                    [:fluid-ticks =]]
                   (keep (fn [[k same?]] (differs old new k same?)))
                   vec)
        timed? #(some (comp entity/timed? val) (get groups %))]
    (fn [id]
      (or (some #(% id) tests)
          (and moved? (timed? id))))))

(defn- payload [new id t]
  (let [part #(get (by-id new %) id)]
    (schema/payload-of (part :chunks) (part :bes) (part :groups)
                       (part :block-ticks) (part :fluid-ticks) t)))

(defn- loaded-only [payloads chunks]
  (reduce-kv (fn [m id _] (if (contains? chunks id) m (dissoc m id)))
             payloads payloads))

(defn- built [old new moved? t]
  (into {} (comp (filter (touched old new moved?))
                 (map (fn [id] [id (payload new id t)])))
        (keys (:chunks new))))

(defn- level-changes [old world dim moved?]
  (let [new (level-parts old world dim)
        built (built old new moved? (long (:tick world 0)))
        before (:payloads old)
        payloads (merge (loaded-only before (:chunks new)) built)]
    {:parts   (assoc new :payloads payloads)
     :built   (count built)
     :changed (into [] (remove (fn [[id p]] (= p (get before id))))
                    built)}))

(def ^:private clock-keys [:tick :time-ms :clocks])

(defn- timeless [m]
  (apply dissoc m clock-keys))

(defn- meta-changed? [a b]
  (not= (timeless a) (timeless b)))

(defn- write-changes! [state store m changed]
  (try
    (commit! store m changed)
    (update state :writes inc)
    (catch Throwable t
      (log/warn "snapshot: commit failed, the next one retries -"
                (.getMessage t))
      nil)))

(defn- all-changes [state world moved?]
  (let [level (fn [dim]
                (let [old (get-in state [:last dim])]
                  [dim (level-changes old world dim moved?)]))]
    (into {} (map level) (keys (:levels world)))))

(defn- changed-chunks [levels]
  (into {} (keep (fn [[dim {c :changed}]] (when (seq c) [dim c])))
        levels))

(defn- save! [state saver store world]
  (let [tick (long (:tick world 0))
        moved? (not= tick (:tick state))
        m (meta-of-world world)
        levels (all-changes state world moved?)
        parts (update-vals levels :parts)
        held (filter (partial held-at? tick) @(pending-of saver))
        changed (with-held (changed-chunks levels) parts held)
        state' (if (and (empty? changed)
                        (not (meta-changed? m (:meta state))))
                 state
                 (write-changes! state store m changed))]
    (if state'
      (-> (assoc state' :last parts :tick tick :meta m
                 :built (reduce + (map :built (vals levels))))
          (released! saver tick))
      (assoc state :last nil :meta nil))))

(defn- chunk-name ^String [id]
  (let [[cx cz] (chunk/id->pos id)] (str "chunk " cx "," cz)))

(defn- read-stored [store dim id]
  (try (get-chunk store dim id)
       (catch DataFormatException e
         (log/warn (chunk-name id) "has no readable record and"
                   "starts over as a new chunk," (.getMessage e))
         nil)))

(defn- read-chunk [saver store dim id]
  (let [held (some-> (pending-of saver) deref (get [dim id]))
        v (if held (peek held) (read-stored store dim id))]
    (if (bytes? v) (nippy/thaw v) v)))

(declare fetch-chunk!)

(defn- retried! [saver store dim id deliver ^Throwable t]
  (Thread/interrupted)
  (log/warn (chunk-name id) "could not be read, reading it again"
            "in a second," (.getMessage t))
  (Thread/startVirtualThread
    #(do (Thread/sleep 1000)
         (fetch-chunk! saver store dim id deliver))))

(defn- fetched! [state saver store dim id deliver]
  (let [v (try (read-chunk saver store dim id)
               (catch Throwable t
                 (retried! saver store dim id deliver t)
                 ::failed))]
    (when-not (identical? ::failed v) (deliver v))
    state))

(defn fetch-chunk!
  "Reads a saved chunk and gives it to deliver.
  The read waits for every save asked for before it. deliver gets nil
  when no record of the chunk reads. A read that fails otherwise is
  tried again until it succeeds."
  [saver store dim id deliver]
  (send-off saver fetched! saver store dim id deliver))

(defn fetch-chunk-now!
  "Returns a saved chunk at once.
  A chunk that waits for its write comes back as it was given. Returns
  nil when no record of the chunk reads and throws when the read fails
  otherwise."
  [saver store dim id]
  (read-chunk saver store dim id))

(defn request-save!
  "Asks the saver to commit world to store.
  The commit holds the chunks unloaded up to the tick of world.
  Returns nil when there is no saver."
  [saver store world]
  (when saver
    (send-off saver save! saver store world)
    true))

(defn commit-due!
  "Asks the saver to commit the world of world-ref when a commit
  is wanted. Call it between two ticks."
  [saver store world-ref]
  (when (compare-and-set! (:wanted (meta saver)) true false)
    (request-save! saver store @world-ref)))

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
    (when ok (close! store))
    ok))
