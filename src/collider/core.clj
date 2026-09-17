(ns collider.core
  "Starting and stopping the server."
  (:refer-clojure :exclude [run!])
  (:require [clojure.edn :as edn]
            [collider.cli :as cli]
            [collider.config :as config]
            [collider.data :as data]
            [collider.game.state :as state]
            [collider.game.tick :as tick]
            [collider.log :as log]
            [collider.net.render :as render]
            [collider.net.server :as server]
            [collider.net.session :as session]
            [collider.persist.snapshot :as snapshot])
  (:import (clojure.lang ExceptionInfo)
           (collider.game.deltas Deltas)
           (java.lang.management GarbageCollectorMXBean ManagementFactory)
           (java.net BindException ServerSocket)
           (java.util.concurrent ConcurrentLinkedQueue Executors
                                 ScheduledExecutorService ThreadFactory TimeUnit))
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private ^:const shutdown-drain-ms 1000)

(defn- on-loaded [^ConcurrentLinkedQueue queue id]
  #(.offer queue [:chunk-loaded id %]))

(defn- chunk-io! [{:keys [saver store]} queue ^Deltas deltas]
  (doseq [{:keys [msg id payload]} (.out deltas)]
    (case msg
      :store-chunk (snapshot/store-chunk! saver store id payload)
      :load-chunk (snapshot/fetch-chunk! saver store id
                                         (on-loaded queue id))
      nil)))

(defn- deliver! [conns world deltas]
  (let [cs @conns]
    (doseq [[eid pkt] (render/render world deltas)]
      (when-let [conn (cs eid)]
        (if (= :close pkt)
          (server/close! conn)
          (server/send! conn pkt))))))

(defn- shutdown! [{:keys [^ServerSocket socket conns ticker saver store world]}]
  (some-> socket .close)
  (some-> ticker tick/stop-ticker!)
  (server/close-all! conns {:translate "multiplayer.disconnect.server_shutdown"} shutdown-drain-ms)
  (when saver (snapshot/stop-saver! saver store @world)))

(defn- saver-scheduler ^ScheduledExecutorService [save! ^long period-ms]
  (doto (Executors/newSingleThreadScheduledExecutor
          (reify ThreadFactory
            (newThread [_ r] (doto (Thread. ^Runnable r "collider-saver-timer")
                               (.setDaemon true)))))
    (.scheduleWithFixedDelay ^Runnable save! period-ms period-ms TimeUnit/MILLISECONDS)))

(defn- shutdown-hook! [server]
  (let [t (Thread. ^Runnable #(shutdown! server) "collider-shutdown")]
    (.addShutdownHook (Runtime/getRuntime) t)
    t))

(defn- open-store [opts cfg]
  (or (:store opts)
      (when-let [dir (:save-dir cfg)] (snapshot/file-store dir))))

(defn- world-config [cfg store]
  (assoc (select-keys cfg [:view-distance :simulation-distance
                           :max-players :motd])
         :unload-chunks? (some? store)))

(defn- open-world [opts]
  (let [cfg (merge (config/load-config) opts)
        store (open-store opts cfg)
        saved (when store (snapshot/load-snapshot store))
        world (atom (assoc (merge state/initial-world saved)
                           :config (world-config cfg store)))
        saver (when store (snapshot/start-saver))]
    {:cfg   cfg :store store :saved saved :world world :saver saver
     :save! (when saver #(snapshot/request-save! saver store @world))}))

(defn- open-net [{:keys [cfg world save!]}]
  (let [queue (ConcurrentLinkedQueue.)
        conns (atom {})
        io {:queue     queue :conns conns :cfg cfg :save! save! :world world
            :on-packet session/handle-packet}
        {:keys [socket accept]} (server/listen! io (:port cfg))]
    {:queue queue :conns conns :socket socket :accept accept}))

(defn- ticker-opts [conns cfg save!]
  {:io-input #(hash-map :writable (server/writable-eids conns))
   :pause-when-empty-seconds (:pause-when-empty-seconds cfg)
   :on-pause save!})

(defn- start-clocks [base {:keys [queue conns]}]
  (let [{:keys [cfg world saver save!]} base
        out! (fn [w d]
               (when saver (chunk-io! base queue d))
               (deliver! conns w d))
        opts (ticker-opts conns cfg save!)
        ticker (tick/start-ticker! world queue out! opts)]
    {:ticker    ticker :tick-stats (:stats ticker)
     :scheduler (when saver
                  (saver-scheduler save! (:save-period-ms cfg)))}))

(defn- host-event [saved config-written?]
  (let [rt (Runtime/getRuntime)
        ^GarbageCollectorMXBean gc (first (ManagementFactory/getGarbageCollectorMXBeans))]
    {:event           :host
     :java            (System/getProperty "java.version")
     :cores           (.availableProcessors rt)
     :heap            (log/human-bytes (.maxMemory rt))
     :gc              (re-find #"\S+" (.getName gc))
     :data            (data/dir)
     :world           (:save-dir (config/load-config))
     :chunks          (when (seq (:stored saved)) (count (:stored saved)))
     :entities        (count (:entities saved))
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
  (ex-info "port taken"
           {:what    "failed to bind to port"
            :why     (str "Perhaps a server is already running on port " port "?")
            :command (str "Set another port in config.edn: {:port " (inc (long port)) "}")}))

(defn- listen [base]
  (try (open-net base)
       (catch BindException _
         (throw (port-taken (:port (:cfg base)))))))

(defn start [opts]
  (let [report (:report opts (fn [_] nil))
        {:keys [store saved world saver] :as base} (open-world opts)]
    (report (host-event saved (:config-written? opts)))
    (timed report :load data/load!)
    (let [net (listen base)
          clocks (start-clocks base net)
          server (merge {:world world :saver saver :store store} net clocks)]
      (report {:event :ready :port (.getLocalPort ^ServerSocket (:socket net)) :took (uptime)})
      (assoc server :shutdown-hook (shutdown-hook! server)))))

(defn stop [{:keys [^ScheduledExecutorService scheduler ^Thread shutdown-hook] :as server}]
  (some-> scheduler .shutdownNow)
  (when shutdown-hook
    (try (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
         (catch IllegalStateException _ nil)))
  (shutdown! server)
  (log/info "server stopped")
  nil)

(defn- run! [opts]
  (try (start (assoc opts :report cli/render!))
       (catch ExceptionInfo e
         (cli/render! (assoc (ex-data e) :event :error))
         (System/exit 1))))

(defn -main [& args]
  (let [written? (config/write-default!)
        opts (apply merge {} (map edn/read-string args))
        {:keys [^Thread accept]} (run! (assoc opts :config-written? written?))]
    (.join accept)))
