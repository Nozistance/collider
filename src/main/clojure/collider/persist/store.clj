(ns collider.persist.store
  "Reading and committing the saved world."
  (:refer-clojure :exclude [load])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [collider.config :as config]
            [collider.data :as data]
            [collider.data.long-map :as lm]
            [collider.game.schema :as schema]
            [collider.persist.atomic-file :as atomic-file]
            [collider.persist.region :as region]
            [collider.persist.snapshot :as snapshot]
            [collider.persist.store.records :as records
             :refer [Store commit! load]])
  (:import (collider.persist.store.records FileStore)
           (java.io File)
           (java.nio.channels ClosedChannelException)
           (java.nio.file Files)))

(set! *warn-on-reflection* true)

(defn- dim-dir ^File [dir dim]
  (io/file dir (name dim)))

(defn- meta-file ^File [dir]
  (io/file dir "meta.edn"))

(defn- bak-file ^File [dir]
  (io/file dir "meta.edn.bak"))

(defn- file-bytes ^bytes [^File f]
  (Files/readAllBytes (.toPath f)))

(defn- put! [^File f ^bytes data]
  (atomic-file/put! (.toPath f) data))

(defn- edn-of [^bytes data]
  (let [v (edn/read-string (String. data "UTF-8"))]
    (if (map? v)
      v
      (throw (ex-info (str "holds " (pr-str (type v)) " not a map")
                      {})))))

(defn- read-edn [^File f]
  (edn-of (file-bytes f)))

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
         (let [why (str "cannot be read, " (ex-message t))]
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

(defn- opened-level! [dir m dim]
  (let [gen (long (get-in m [:levels dim :regions-gen] 0))
        d (dim-dir dir dim)]
    (region/sweep! d gen)
    {:gen     gen
     :regions (into {} (map (fn [r] [(region/key-of r) r]))
                    (region/open-all d gen))}))

(defn- open-levels! [{:keys [dir levels]} m]
  (let [open (fn [dim] [dim (opened-level! dir m dim)])]
    (reset! levels (into {} (map open) schema/dims))))

(defn- stamped-meta [dir]
  (some->> (read-meta-file dir) (stamped dir)))

(defn- levels-of! [{:keys [dir levels] :as store}]
  (or @levels (open-levels! store (stamped-meta dir))))

(defn- stored-ids [{:keys [regions]}]
  (into (lm/long-set) (mapcat region/chunks) (vals regions)))

(defn- with-stored [levels opened]
  (into {} (for [[dim lm] levels]
             [dim (-> (dissoc lm :regions-gen)
                      (assoc :stored (stored-ids (opened dim))))])))

(defn- read-store! [{:keys [dir] :as store}]
  (when-let [m (stamped-meta dir)]
    (let [opened (open-levels! store m)]
      (-> (dissoc m :stamp)
          (update :levels with-stored opened)))))

(defn- appended [made dir gen old k chunks]
  (let [made! #(do (vswap! made conj %) %)
        r (or (some-> old region/copy)
              (made! (region/create dir k gen)))]
    (doseq [[id p] chunks] (region/append! r id (snapshot/freeze p)))
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
        add (fn [[k cs]]
              [k (appended made dir g (get regions k) k cs)])
        news (into {} (map add) (by-region chunks))]
    {:gen g :regions (merge regions news) :news (vals news)
     :retired (retired regions news)}))

(defn- appended-levels! [{:keys [dir] :as store} chunks-by-dim]
  (let [levels (levels-of! store)
        made (volatile! [])
        level (fn [[dim chunks]]
                (let [lv (get levels dim {:gen 0})
                      d (dim-dir dir dim)]
                  [dim (written-level made d lv chunks)]))]
    (try (into {} (comp (filter (comp seq val)) (map level))
               chunks-by-dim)
         (catch Throwable t
           (run! region/close! @made)
           (throw t)))))

(defn- abandoned! [appended]
  (doseq [{:keys [gen news]} (vals appended) r news
          :when (= gen (region/gen-of r))]
    (region/close! r)))

(defn- forced! [appended]
  (doseq [{:keys [news]} (vals appended)] (run! region/force! news)))

(defn- manifests! [{:keys [dir]} appended]
  (doseq [[dim {:keys [gen regions]}] appended]
    (region/write-manifest! (dim-dir dir dim) gen (vals regions))))

(defn- backup-meta! [dir]
  (let [f (meta-file dir)]
    (when (readable? f)
      (put! (bak-file dir) (file-bytes f)))))

(defn- check-readable! [^bytes data]
  (edn-of data)
  nil)

(defn- write-meta! [dir m]
  (let [data (edn-bytes m)]
    (check-readable! data)
    (backup-meta! dir)
    (put! (meta-file dir) data)
    (alength data)))

(defn- with-gens [m levels]
  (reduce-kv (fn [m dim {:keys [gen]}]
               (if (pos? (long gen))
                 (assoc-in m [:levels dim :regions-gen] gen)
                 m))
             m levels))

(defn- root! [{:keys [dir] :as store} m appended]
  (let [levels (merge (levels-of! store) appended)]
    (write-meta! dir (assoc (with-gens m levels) :stamp stamp))))

(defn- published! [{:keys [dir levels]} appended]
  (swap! levels merge
         (update-vals appended #(select-keys % [:gen :regions])))
  (doseq [[dim {:keys [gen retired]}] appended]
    (run! region/close! retired)
    (region/sweep! (dim-dir dir dim) gen)))

(defn- stored-bytes [store dim id]
  (some-> (get-in (levels-of! store) [dim :regions (region/of id)])
          (region/chunk-bytes id)))

(defn- retired-bytes
  "Returns the bytes of chunk id, read again when a commit retired its
  region between the lookup and the read."
  [store dim id]
  (try (stored-bytes store dim id)
       (catch ClosedChannelException _
         (stored-bytes store dim id))))

(defn- close-all! [{:keys [levels]}]
  (doseq [lv (vals @levels) r (vals (:regions lv))] (region/close! r))
  (reset! levels nil))

(extend-type FileStore
  Store
  (get-chunk [store dim id]
    (some-> (retired-bytes store dim id) snapshot/thaw))
  (commit! [store m chunks-by-dim]
    (let [appended (appended-levels! store chunks-by-dim)
          n (try (forced! appended)
                 (manifests! store appended)
                 (root! store m appended)
                 (catch Throwable t
                   (abandoned! appended)
                   (throw t)))]
      (published! store appended)
      n))
  (load [store] (read-store! store))
  (close! [store] (close-all! store)))

(defn file-store
  "Returns a store that keeps a world in directory dir."
  [dir]
  (records/->FileStore dir (atom nil)))

(defn- check-meta! [store m]
  (when-let [why (config/complaints schema/Meta m)]
    (throw (ex-info (str "snapshot " store " has bad meta")
                    {:what (str "snapshot " store " has bad meta")
                     :why  why}))))

(defn load-world
  "Returns the world store holds, or nil when it holds none.
  Throws when its meta cannot be read or breaks the schema."
  [store]
  (when-let [m (load store)]
    (check-meta! store m)
    (snapshot/world-of m)))

(defn- per-level [f levels]
  (into {} (for [[dim lm] levels] [dim (f lm)])))

(defn- snapshot-meta [snap]
  (update snap :levels #(per-level (fn [lm] (dissoc lm :chunks)) %)))

(defn write-snapshot!
  [store snap]
  (commit! store (snapshot-meta snap)
           (per-level :chunks (:levels snap))))
