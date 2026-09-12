(ns collider.core
  (:require [collider.config :as config]
            [collider.game.state :as state]
            [collider.game.tick :as tick]
            [collider.log :as log]
            [collider.persist.snapshot :as snapshot]
            [collider.proto.codec :as c]
            [collider.net.render :as render]
            [collider.net.server :as server]
            [collider.net.session :as session])
  (:import (java.lang.management ManagementFactory)
           (java.net ServerSocket)
           (java.util Locale)
           (java.util.concurrent ConcurrentLinkedQueue Executors
                                 ScheduledExecutorService ThreadFactory TimeUnit))
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private ^:const shutdown-drain-ms 1000)
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

(defn- open-world [opts]
  (let [cfg (merge (config/load-config) opts)
        store (or (:store opts) (when-let [dir (:save-dir cfg)] (snapshot/file-store dir)))
        saved (when store (snapshot/load-snapshot store))
        world (atom (assoc (merge state/initial-world saved)
                      :config (select-keys cfg [:view-distance :simulation-distance])))
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

(defn- start-clocks [{:keys [cfg world saver save!]} {:keys [queue conns]}]
  (let [ticker (tick/start-ticker! world queue (fn [w d] (deliver! conns w d))
                                   {:io-input #(hash-map :writable (server/writable-eids conns))})]
    {:ticker    ticker :tick-stats (:stats ticker)
     :scheduler (when saver (saver-scheduler save! (:save-period-ms cfg)))}))

(defn- log-started! [store saved ^ServerSocket srv]
  (when (seq (:chunks saved))
    (log/info "world loaded:" (count (:chunks saved)) "chunks," (count (:entities saved)) "entities from" (str store)))
  (log/info (String/format Locale/ROOT "collider %s (protocol %s) done (%.3fs), on %s"
                           (object-array [c/game-version c/protocol-version
                                          (/ (double (.getUptime (ManagementFactory/getRuntimeMXBean))) 1000.0)
                                          (str (.getLocalSocketAddress srv))]))))

(defn start [opts]
  (let [{:keys [store saved world saver] :as base} (open-world opts)
        net (open-net base)
        clocks (start-clocks base net)
        server (merge {:world world :saver saver :store store} net clocks)]
    (log-started! store saved (:socket net))
    (assoc server :shutdown-hook (shutdown-hook! server))))

(defn stop [{:keys [^ScheduledExecutorService scheduler ^Thread shutdown-hook] :as server}]
  (some-> scheduler .shutdownNow)
  (when shutdown-hook
    (try (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
         (catch IllegalStateException _ nil)))
  (shutdown! server)
  (log/info "server stopped")
  nil)

(defn -main [& _]
  (config/write-default!)
  (let [{:keys [^Thread accept]} (start {})]
    (.join accept)))
