(ns collider.core
  "Server start and stop."
  (:refer-clojure :exclude [run!])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.cli :as cli]
            [collider.config :as config]
            [collider.data :as data]
            [collider.game.deltas :as deltas]
            [collider.game.state :as state]
            [collider.game.tick :as tick]
            [collider.log :as log]
            [collider.net.render :as render]
            [collider.net.server :as server]
            [collider.net.session :as session]
            [collider.persist.snapshot :as snapshot])
  (:import (clojure.lang ExceptionInfo)
           (collider.game.deltas.record Deltas)
           (java.lang.management
             GarbageCollectorMXBean ManagementFactory)
           (java.net BindException ServerSocket URL)
           (java.util.concurrent
             ConcurrentLinkedQueue Executors
             ScheduledExecutorService ScheduledFuture ThreadFactory
             TimeUnit))
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private ^:const shutdown-drain-ms 1000)

(def ^:private shutdown-reason
  {:translate "multiplayer.disconnect.server_shutdown"})

(defn- on-loaded [^ConcurrentLinkedQueue queue dim id]
  #(.offer queue [:chunk-loaded dim id %]))

(defn- chunk-io! [{:keys [saver store]} queue ^Deltas deltas]
  (doseq [{:keys [msg dim id payload]} (deltas/out-of deltas)]
    (case msg
      :store-chunk
      (snapshot/store-chunk! saver store dim id payload)
      :load-chunk
      (let [done (on-loaded queue dim id)]
        (snapshot/fetch-chunk! saver store dim id done))
      nil)))

(defn- deliver! [conns world deltas]
  (let [cs @conns]
    (doseq [[eid pkt] (render/render world deltas)]
      (when-let [conn (cs eid)]
        (if (= :close pkt)
          (server/close! conn)
          (server/send! conn pkt))))))

(defn- shutdown!
  [{:keys [^ServerSocket socket conns ticker saver store world]}]
  (some-> socket .close)
  (some-> ticker tick/stop-ticker!)
  (server/close-all! conns shutdown-reason shutdown-drain-ms)
  (when saver (snapshot/stop-saver! saver store @world)))

(defn- saver-thread ^Thread [^Runnable r]
  (doto (Thread. r "collider-saver-timer")
    (.setDaemon true)))

(defn- saver-factory ^ThreadFactory []
  (reify ThreadFactory
    (newThread [_ r] (saver-thread r))))

(defn- autosave!
  [{:keys [^ScheduledExecutorService pool settings save! pending]
    :as saving}]
  (let [ms (long (:save-period-ms @settings))
        run #(try (save!) (finally (autosave! saving)))
        unit TimeUnit/MILLISECONDS]
    (when (pos? ms)
      (reset! pending (.schedule pool ^Runnable run ms unit)))))

(defn- reschedule!
  [{:keys [^ScheduledExecutorService pool pending] :as saving}]
  (let [cancel #(some-> ^ScheduledFuture @pending (.cancel false))]
    (.execute pool ^Runnable #(do (cancel) (autosave! saving)))))

(defn- start-saving [save! settings]
  (doto {:pool     (Executors/newSingleThreadScheduledExecutor
                     (saver-factory))
         :settings settings :save! save! :pending (atom nil)}
    autosave!))

(defn- restart-warning [old ks]
  (log/warn "config.edn:" (str/join ", " (map pr-str ks))
            "take a restart, keeping"
            (pr-str (select-keys old ks))))

(defn- reload-failure [e]
  (let [{:keys [what why]} (ex-data e)]
    (log/warn "reload failed:" (or what (ex-message e)))
    (doseq [line why] (log/warn " " line))))

(defn- applied!
  [{:keys [settings on-change]} r eid]
  (let [old @settings
        s (:settings r)]
    (when-let [ks (seq (:restart r))] (restart-warning old ks))
    (reset! settings s)
    (on-change old s)
    [:config-loaded eid (select-keys s config/world-keys)]))

(defn- reloaded!
  [{:keys [settings overlay path ^ConcurrentLinkedQueue queue]
    :as edge} eid]
  (let [reread #(config/reload @settings path overlay)]
    (.offer queue
            (try (applied! edge (reread) eid)
                 (catch Exception e
                   (reload-failure e)
                   [:config-failed eid])))))

(defn- reload-io! [reload ^Deltas deltas]
  (doseq [m (deltas/out-of deltas)
          :when (identical? :reload (:msg m))]
    (Thread/startVirtualThread
      ^Runnable #(reloaded! reload (:to m)))))

(defn- shutdown-hook! [server]
  (let [t (Thread. ^Runnable #(shutdown! server) "collider-shutdown")]
    (.addShutdownHook (Runtime/getRuntime) t)
    t))

(defn- open-store [opts cfg]
  (or (:store opts)
      (when-let [dir (:save-dir cfg)] (snapshot/file-store dir))))

(defn- run-out [& args]
  (let [^"[Ljava.lang.String;" argv (into-array String args)
        p (.start (ProcessBuilder. argv))
        out (str/trim (slurp (.getInputStream p)))]
    (when (and (zero? (.waitFor p)) (seq out)) out)))

(defn- source-dir []
  (let [^URL u (io/resource "collider/core.clj")]
    (when (= "file" (some-> u .getProtocol))
      (.getParent (.getParentFile (io/file u))))))

(defn- git-commit []
  (when-let [dir (source-dir)]
    (try (run-out "git" "-C" dir "rev-parse" "--short=11" "HEAD")
         (catch Exception _ nil))))

(defn- build-commit []
  (or (git-commit)
      (some-> (io/resource "collider/build.edn") slurp edn/read-string
              :commit)))

(defn- world-config [cfg store]
  (assoc (select-keys cfg config/world-keys)
         :unload-chunks? (some? store) :commit (build-commit)))

(defn- open-world [opts]
  (let [cfg (merge (config/load-config) opts)
        store (open-store opts cfg)
        saved (when store (snapshot/load-snapshot store))
        init (merge state/initial-world saved)
        world (atom (assoc init :config (world-config cfg store)))
        saver (when store (snapshot/start-saver))
        save! (when saver
                #(snapshot/request-save! saver store @world))]
    {:settings (atom cfg) :opts opts :store store :saved saved
     :world world :saver saver :save! save! :handle (promise)}))

(defn- open-net [{:keys [settings world save!]}]
  (let [queue (ConcurrentLinkedQueue.)
        conns (atom {})
        io {:queue queue :conns conns :settings settings :save! save!
            :world world :on-packet #'session/handle-packet}
        port (:port @settings)
        {:keys [socket accept]} (server/listen! io port)]
    {:queue queue :conns conns :socket socket :accept accept}))

(defn- reader [{:keys [saver store]}]
  (when saver
    #(snapshot/fetch-chunk-now! saver store %1 %2)))

(defn- io-input [base conns]
  (let [read (reader base)]
    #(hash-map :writable (server/writable-eids conns)
               :read-chunk read)))

(declare halt!)

(defn- on-crash [handle]
  (fn [_]
    (let [exit! (fn [code] (System/exit code))]
      (Thread/startVirtualThread ^Runnable #(halt! @handle exit!)))))

(defn- ticker-opts [base conns]
  {:io-input (io-input base conns)
   :settings (:settings base)
   :on-pause (:save! base)
   :on-crash (on-crash (:handle base))})

(defn- period-change [saving]
  (fn [old new]
    (when (and saving
               (not= (:save-period-ms old) (:save-period-ms new)))
      (reschedule! saving))))

(defn- reload-edge [base queue saving]
  {:settings  (:settings base) :overlay (:opts base)
   :path      "config.edn" :queue queue
   :on-change (period-change saving)})

(defn- start-clocks [base {:keys [queue conns]}]
  (let [{:keys [settings world saver save!]} base
        saving (when saver (start-saving save! settings))
        reload (reload-edge base queue saving)
        out! (fn [w d]
               (when saver (chunk-io! base queue d))
               (reload-io! reload d)
               (deliver! conns w d))
        opts (ticker-opts base conns)
        ticker (tick/start-ticker! world queue out! opts)]
    {:ticker    ticker :tick-stats (:stats ticker)
     :scheduler (:pool saving)}))

(defn- gc-name []
  (let [beans (ManagementFactory/getGarbageCollectorMXBeans)
        ^GarbageCollectorMXBean gc (first beans)]
    (re-find #"\S+" (.getName gc))))

(defn- host-event [saved config-written?]
  (let [rt (Runtime/getRuntime)
        lv (some-> saved (state/level :overworld))]
    {:event           :host
     :java            (System/getProperty "java.version")
     :cores           (.availableProcessors rt)
     :heap            (log/human-bytes (.maxMemory rt))
     :gc              (gc-name)
     :data            (data/dir)
     :world           (:save-dir (config/load-config))
     :chunks          (some-> (:stored lv) seq count)
     :entities        (count (:entities lv))
     :config          "config.edn"
     :config-written? config-written?}))

(defn- timed [report step f]
  (report {:event :begin :step step})
  (let [t (System/nanoTime)
        v (f)]
    (report {:event :end :step step :took (- (System/nanoTime) t)})
    v))

(defn- uptime ^long []
  (* 1000000 (.getUptime (ManagementFactory/getRuntimeMXBean))))

(defn- port-taken [port]
  (let [why (format "Perhaps a server is already running on port %s?"
                    port)
        cmd (format "Set another port in config.edn: {:port %s}"
                    (inc (long port)))]
    (ex-info "port taken"
             {:what "failed to bind to port" :why why :command cmd})))

(defn- listen [base]
  (try (open-net base)
       (catch BindException _
         (throw (port-taken (:port @(:settings base)))))))

(defn- hooked [{:keys [handle]} server]
  (let [s (assoc server :shutdown-hook (shutdown-hook! server))]
    (deliver handle s)
    s))

(defn start
  "Starts the server on the configured port and returns its handle."
  [opts]
  (let [report (:report opts (fn [_] nil))
        {:keys [store saved world saver] :as base} (open-world opts)]
    (report (host-event saved (:config-written? opts)))
    (timed report :load data/load!)
    (let [net (listen base)
          clocks (start-clocks base net)
          held {:world world :saver saver :store store}
          server (merge held net clocks)
          port (.getLocalPort ^ServerSocket (:socket net))]
      (report {:event :ready :port port :took (uptime)})
      (hooked base server))))

(defn stop
  "Stops a running server and everything it started."
  [{:keys [^ScheduledExecutorService scheduler ^Thread shutdown-hook]
    :as server}]
  (some-> scheduler .shutdownNow)
  (when shutdown-hook
    (try (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
         (catch IllegalStateException _ nil)))
  (shutdown! server)
  (log/info "server stopped")
  nil)

(defn- halt!
  "Stops a server whose tick crashed, then exits with code 1."
  [server exit!]
  (stop server)
  (exit! 1))

(defn- run! [opts]
  (try (start (assoc opts :report cli/render!))
       (catch ExceptionInfo e
         (cli/render! (assoc (ex-data e) :event :error))
         (System/exit 1))))

(defonce ^{:doc "The server -main started, for the REPL."} running
  (atom nil))

(defn -main
  "Starts the server.
  Each argument is an edn map that overrides the config."
  [& args]
  (log/to-file! "logs")
  (when-not (data/dir)
    (cli/render! (assoc (ex-data (data/no-tables)) :event :error))
    (System/exit 1))
  (let [written? (config/write-default!)
        opts (apply merge {} (map edn/read-string args))
        server (run! (assoc opts :config-written? written?))
        ^Thread accept (:accept server)]
    (reset! running server)
    (.join accept)))
