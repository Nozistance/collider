(ns collider.persist.saver
  "Commits of the world to a store, away from the tick."
  (:require [collider.game.entity :as entity]
            [collider.game.level :as level]
            [collider.game.schedule :as schedule]
            [collider.game.schema :as schema]
            [collider.log :as log]
            [collider.persist.snapshot :as snapshot]
            [collider.persist.store.records :as records]
            [collider.world.chunk :as chunk])
  (:import (java.util.concurrent
             Executors ScheduledExecutorService ScheduledFuture
             ThreadFactory TimeUnit)
           (java.util.zip DataFormatException)))

(set! *warn-on-reflection* true)

(defn start
  "Returns a saver. Its :pending holds the chunks unloaded since the
  last commit, its :wanted whether the next tick commits."
  []
  {:agent   (agent {:last nil :meta nil :writes 0 :held 0 :built 0}
                   :error-mode :continue)
   :pending (atom {})
   :wanted  (atom false)})

(def ^:private ^:const pending-limit (* 8 1024 1024))

(defn- held-bytes ^long [m]
  (transduce (keep (fn [[_ [_ v]]]
                     (when (bytes? v) (alength ^bytes v))))
             + 0 m))

(defn want-save!
  "Asks for a commit at the start of the next tick."
  [saver]
  (some-> saver :wanted (reset! true)))

(defn- frozen [state saver k payload]
  (let [data (snapshot/freeze payload)
        n (+ (long (:held state)) (alength data))
        keep! (fn [m] (if (identical? payload (peek (get m k)))
                        (assoc-in m [k 1] data)
                        m))]
    (swap! (:pending saver) keep!)
    (when (< pending-limit n) (want-save! saver))
    (assoc state :held n)))

(defn store-chunk!
  "Holds a chunk unloaded at tick until the next commit.
  A read sees it at once."
  [saver dim id tick payload]
  (swap! (:pending saver) assoc [dim id] [tick payload])
  (send-off (:agent saver) frozen saver [dim id] payload))

(defn- held-at? [^long tick [_ [t]]]
  (<= (long t) tick))

(defn- with-held [changed levels held]
  (reduce (fn [m [[dim id] [_ v]]]
            (if (contains? (get-in levels [dim :chunks]) id)
              m
              (update m dim (fnil conj []) [id v])))
          changed held))

(defn- released [state saver tick]
  (let [m (swap! (:pending saver)
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

(defn- saved-form [parts k]
  (let [v (get parts k)] (if (vector? v) (peek v) v)))

(defn- differs [old new k same?]
  (let [a (saved-form new k)
        b (saved-form old k)]
    (when-not (identical? a b)
      (fn [id] (not (same? (get a id) (get b id)))))))

(defn- changed? [old new moved?]
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
  (let [part #(get (saved-form new %) id)]
    (schema/payload-of (part :chunks) (part :bes) (part :groups)
                       (part :block-ticks) (part :fluid-ticks) t)))

(defn- loaded-only [payloads chunks]
  (reduce-kv (fn [m id _] (if (contains? chunks id) m (dissoc m id)))
             payloads payloads))

(defn- built [old new moved? t]
  (into {} (comp (filter (changed? old new moved?))
                 (map (fn [id] [id (payload new id t)])))
        (keys (:chunks new))))

(defn- level-changes [old world dim moved?]
  (let [new (level-parts old world dim)
        fresh (built old new moved? (long (:tick world 0)))
        before (:payloads old)
        payloads (merge (loaded-only before (:chunks new)) fresh)]
    {:parts   (assoc new :payloads payloads)
     :built   (count fresh)
     :changed (into [] (remove (fn [[id p]] (= p (get before id))))
                    fresh)}))

(def ^:private clock-keys [:tick :time-ms :clocks])

(defn- timeless [m]
  (apply dissoc m clock-keys))

(defn- meta-changed? [a b]
  (not= (timeless a) (timeless b)))

(defn- written [state store m changed]
  (try
    (records/commit! store m changed)
    (update state :writes inc)
    (catch Throwable t
      (log/warn "snapshot: commit failed, the next one retries -"
                (ex-message t))
      nil)))

(defn- all-changes [state world moved?]
  (let [level (fn [dim]
                (let [old (get-in state [:last dim])]
                  [dim (level-changes old world dim moved?)]))]
    (into {} (map level) (keys (:levels world)))))

(defn- changed-chunks [levels]
  (into {} (keep (fn [[dim {c :changed}]] (when (seq c) [dim c])))
        levels))

(defn- committed [state saver store world]
  (let [tick (long (:tick world 0))
        moved? (not= tick (:tick state))
        m (snapshot/meta-of world)
        levels (all-changes state world moved?)
        parts (update-vals levels :parts)
        held (filter (partial held-at? tick) @(:pending saver))
        changed (with-held (changed-chunks levels) parts held)
        idle? (and (empty? changed)
                   (not (meta-changed? m (:meta state))))
        state' (if idle? state (written state store m changed))]
    (if state'
      (-> (assoc state' :last parts :tick tick :meta m
                 :built (reduce + (map :built (vals levels))))
          (released saver tick))
      (assoc state :last nil :meta nil))))

(defn- chunk-name ^String [id]
  (let [[cx cz] (chunk/id->pos id)] (str "chunk " cx "," cz)))

(defn- read-stored [store dim id]
  (try (records/get-chunk store dim id)
       (catch DataFormatException e
         (log/warn (chunk-name id) "has no readable record and"
                   "starts over as a new chunk," (ex-message e))
         nil)))

(defn- read-chunk [saver store dim id]
  (let [held (some-> (:pending saver) deref (get [dim id]))
        v (if held (peek held) (read-stored store dim id))]
    (if (bytes? v) (snapshot/thaw v) v)))

(declare fetch-chunk!)

(defn- retried! [saver store dim id deliver ^Throwable t]
  (Thread/interrupted)
  (log/warn (chunk-name id) "could not be read, reading it again"
            "in a second," (ex-message t))
  (Thread/startVirtualThread
    #(do (Thread/sleep 1000)
         (fetch-chunk! saver store dim id deliver))))

(defn- fetched [state saver store dim id deliver]
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
  (send-off (:agent saver) fetched saver store dim id deliver))

(defn fetch-chunk-now!
  "Returns a saved chunk at once.
  A chunk that waits for its write comes back as it was given.
  Returns nil when no record of the chunk reads and throws when the
  read fails otherwise."
  [saver store dim id]
  (read-chunk saver store dim id))

(defn save!
  "Asks the saver to commit world to store.
  The commit holds the chunks unloaded up to the tick of world.
  Returns nil when there is no saver."
  [saver store world]
  (when saver
    (send-off (:agent saver) committed saver store world)
    true))

(defn save-due!
  "Asks the saver to commit the world of world-ref when a commit
  is wanted. Call it between two ticks."
  [saver store world-ref]
  (when (compare-and-set! (:wanted saver) true false)
    (save! saver store @world-ref)))

(defn await!
  "Waits up to ms for the saver to finish what it was given."
  ([saver] (await! saver 2000))
  ([saver ms] (if saver (await-for ms (:agent saver)) true)))

(defn stop!
  "Writes the world one last time and waits for the saver. Returns
  true when it finished in time."
  [saver store world]
  (save! saver store world)
  (let [ok (await! saver)]
    (when ok (records/close! store))
    ok))

(defn- timer-thread ^Thread [^Runnable r]
  (doto (Thread. r "collider-saver-timer")
    (.setDaemon true)))

(defn- thread-factory ^ThreadFactory []
  (reify ThreadFactory
    (newThread [_ r] (timer-thread r))))

(defn- autosave!
  [{:keys [^ScheduledExecutorService pool settings save! pending]
    :as timer}]
  (let [ms (long (:commit-period-ms @settings))
        run #(try (save!) (finally (autosave! timer)))
        unit TimeUnit/MILLISECONDS]
    (when (pos? ms)
      (reset! pending (.schedule pool ^Runnable run ms unit)))))

(defn timer
  "Returns a timer that calls save! once each :commit-period-ms of the
  settings atom. A period of 0 stops it."
  [save! settings]
  (doto {:pool     (Executors/newSingleThreadScheduledExecutor
                     (thread-factory))
         :settings settings :save! save! :pending (atom nil)}
    autosave!))

(defn retime!
  "Starts the period of timer again, as long as its settings say now."
  [{:keys [^ScheduledExecutorService pool pending] :as timer}]
  (let [cancel #(some-> ^ScheduledFuture @pending (.cancel false))]
    (.execute pool ^Runnable #(do (cancel) (autosave! timer)))))

(defn stop-timer!
  "Stops timer."
  [{:keys [^ScheduledExecutorService pool]}]
  (some-> pool .shutdownNow))
