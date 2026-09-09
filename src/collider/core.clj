(ns collider.core
  (:require [collider.config :as config]
            [collider.game.state :as state]
            [collider.game.tick :as tick]
            [collider.log :as log]
            [collider.persist.snapshot :as snapshot]
            [collider.proto.codec :as c]
            [collider.render :as render]
            [collider.server :as server]
            [collider.session :as session])
  (:import (java.net ServerSocket)
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

(defn start
  [opts]
  (let [cfg (merge (config/load-config) opts)
        {:keys [save-file save-period-ms]} cfg
        store (or (:store opts) (when save-file (snapshot/file-store save-file)))
        saved (when store (snapshot/load-snapshot store))
        world (atom (assoc (merge state/initial-world saved)
                           :config (select-keys cfg [:view-distance :simulation-distance])))
        saver (when store (snapshot/start-saver))
        save! (when saver #(snapshot/request-save! saver store @world))
        queue (ConcurrentLinkedQueue.)
        conns (atom {})
        io {:queue queue :conns conns :cfg cfg :save! save!
             :on-packet session/handle-packet}
        {^ServerSocket srv :socket accept :accept} (server/listen! io (:port cfg))
        ticker (tick/start-ticker! world queue (fn [w d] (deliver! conns w d))
                                   {:io-input #(hash-map :writable (server/writable-eids conns))})
        sched (when saver (saver-scheduler save! save-period-ms))
        server {:socket srv :accept accept
                :world world :queue queue :conns conns :ticker ticker
                :tick-stats (:stats ticker)
                :saver saver :store store :scheduler sched}]
    (when (seq (:chunks saved))
      (log/info "world loaded:" (count (:chunks saved)) "chunks," (count (:entities saved)) "entities from" (str store)))
    (log/info "collider" c/game-version "(protocol" (str c/protocol-version ") on")
              (str (.getLocalSocketAddress srv)))
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
