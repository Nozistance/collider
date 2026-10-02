(ns collider.core
  "Server start and stop."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.config :as config]
            [collider.data :as data]
            [collider.game.deltas :as deltas]
            [collider.game.level :as level]
            [collider.game.schema :as schema]
            [collider.game.tick :as tick]
            [collider.game.ticker :as ticker]
            [collider.log :as log]
            [collider.net.crypt :as crypt]
            [collider.net.render :as render]
            [collider.net.server :as server]
            [collider.net.session :as session]
            [collider.persist.lock :as lock]
            [collider.persist.saver :as saver]
            [collider.persist.store :as store]
            [collider.plugin :as plugin])
  (:import (java.io Closeable)
           (collider.game.deltas.record Deltas)
           (java.lang.management ManagementFactory)
           (java.net BindException ServerSocket URL)
           (java.util.concurrent ConcurrentLinkedQueue)))

(set! *warn-on-reflection* true)

(def ^:private ^:const shutdown-drain-ms 1000)

(def ^:private shutdown-reason
  {:translate "multiplayer.disconnect.server_shutdown"})

(defn- on-loaded [^ConcurrentLinkedQueue queue dim id]
  #(.offer queue [:chunk-loaded dim id %]))

(defn- chunk-io! [{:keys [saver store]} queue world ^Deltas deltas]
  (doseq [{:keys [msg dim id payload]} (deltas/out-of deltas)]
    (case msg
      :store-chunk
      (saver/store-chunk! saver dim id (:tick world) payload)
      :load-chunk
      (let [done (on-loaded queue dim id)]
        (saver/fetch-chunk! saver store dim id done))
      nil)))

(defn- deliver! [conns world deltas]
  (let [cs @conns]
    (doseq [[eid pkt] (render/render world deltas)]
      (when-let [conn (cs eid)]
        (if (= :close pkt)
          (server/close! conn)
          (server/send! conn pkt))))))

(defn- shutdown!
  [{:keys [^ServerSocket socket conns ticker saver store world]
    :as server}]
  (some-> socket .close)
  (some-> ticker ticker/stop-ticker!)
  (server/close-all! conns shutdown-reason shutdown-drain-ms)
  (when saver (saver/stop! saver store @world))
  (plugin/stop-all! (:plugins server))
  (some-> ^Closeable (:lock server) .close))

(defn stop
  "Stops a running server and everything it started."
  [{:keys [timer ^Thread shutdown-hook] :as server}]
  (some-> timer saver/stop-timer!)
  (when shutdown-hook
    (try (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
         (catch IllegalStateException _ nil)))
  (shutdown! server)
  (log/info "server stopped")
  nil)

(defn halt!
  "Stops a server whose tick crashed, then calls exit! with code 1."
  [server exit!]
  (stop server)
  (exit! 1))

(defn- reload-io! [edge ^ConcurrentLinkedQueue queue ^Deltas deltas]
  (doseq [m (deltas/out-of deltas)
          :when (identical? :reload (:msg m))]
    (Thread/startVirtualThread
      ^Runnable #(.offer queue (config/reload! edge (:to m))))))

(defn- shutdown-hook! [server]
  (let [t (Thread. ^Runnable #(shutdown! server) "collider-shutdown")]
    (.addShutdownHook (Runtime/getRuntime) t)
    t))

(defn- open-store [opts cfg]
  (or (:store opts)
      (when-let [dir (:save-dir cfg)] (store/file-store dir))))

(defn- world-lock [opts cfg]
  (when-not (:store opts)
    (some-> (:save-dir cfg) lock/lock!)))

(defn- locked [opts cfg f]
  (let [l (world-lock opts cfg)]
    (try (f l)
         (catch Throwable t (some-> ^Closeable l .close) (throw t)))))

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

(defn build-commit
  "Returns the commit of the running code, or nil when it is not
  known."
  []
  (or (git-commit)
      (some-> (io/resource "collider/build.edn") slurp edn/read-string
              :commit)))

(defn- world-config [cfg store]
  (assoc (select-keys cfg config/world-keys)
         :unload-chunks? (some? store) :commit (build-commit)))

(defn- open-world [opts cfg l]
  (let [store (open-store opts cfg)
        saved (when store (store/load-world store))
        init (merge schema/initial-world saved)
        world (atom (assoc init :config (world-config cfg store)))
        saver (when store (saver/start))
        save! (when saver #(saver/want-save! saver))]
    {:settings (atom cfg) :opts opts :store store :saved saved
     :world world :saver saver :save! save! :handle (promise)
     :lock l}))

(defn- open-net [{:keys [settings world save! opts adds]}]
  (let [queue (ConcurrentLinkedQueue.)
        conns (atom {})
        io {:queue queue :conns conns :settings settings :save! save!
            :world world :on-packet #'session/handle-packet
            :identity (or (:identity opts) (:identity adds))
            :key-pair (crypt/key-pair)}
        port (:port @settings)
        {:keys [socket accept]} (server/listen! io port)]
    {:queue queue :conns conns :socket socket :accept accept}))

(defn- reader [{:keys [saver store]}]
  (when saver
    #(saver/fetch-chunk-now! saver store %1 %2)))

(defn- io-input [{:keys [saver store world] :as base} conns]
  (let [read-chunk (reader base)]
    #(do (when saver (saver/save-due! saver store world))
         {:writable   (server/writable-eids conns)
          :read-chunk read-chunk})))

(defn- on-crash [handle]
  (fn [_]
    (let [exit! (fn [code] (System/exit code))]
      (Thread/startVirtualThread ^Runnable #(halt! @handle exit!)))))

(defn- commit-now [{:keys [saver store world]}]
  (when saver #(saver/save! saver store @world)))

(defn- ticker-opts [base conns]
  (cond-> {:io-input (io-input base conns)
           :settings (:settings base)
           :on-pause (commit-now base)
           :on-crash (on-crash (:handle base))}
    (:phases base) (assoc :phases (:phases base))))

(defn- period-change [timer]
  (fn [old new]
    (when (and timer
               (not= (:commit-period-ms old) (:commit-period-ms new)))
      (saver/retime! timer))))

(defn- reload-edge [base timer]
  {:settings  (:settings base) :overlay (:opts base)
   :path      config/file
   :on-change (period-change timer)})

(defn- start-clocks [base {:keys [queue conns]}]
  (let [{:keys [settings world saver save!]} base
        timer (when saver (saver/timer save! settings))
        reload (reload-edge base timer)
        out! (fn [w d]
               (when saver (chunk-io! base queue w d))
               (reload-io! reload queue d)
               (deliver! conns w d))
        opts (ticker-opts base conns)
        ticker (ticker/start-ticker! world queue out! opts)]
    {:ticker ticker :tick-stats (:stats ticker) :timer timer}))

(defn- host-event [saved config-written?]
  (let [rt (Runtime/getRuntime)
        lv (some-> saved (level/level :overworld))]
    {:event           :host
     :java            (System/getProperty "java.version")
     :cores           (.availableProcessors rt)
     :heap            (log/human-bytes (.maxMemory rt))
     :world           (:save-dir (config/load-config))
     :chunks          (some-> (:stored lv) seq count)
     :entities        (count (:entities lv))
     :config          config/file
     :config-written? config-written?}))

(defn- timed [report step f]
  (report {:event :begin :step step})
  (let [t (System/nanoTime)
        v (f)]
    (report {:event :end :step step :took (- (System/nanoTime) t)})
    v))

(defn- uptime-ns ^long []
  (* 1000000 (.getUptime (ManagementFactory/getRuntimeMXBean))))

(defn- port-taken [port]
  (let [why (format "Perhaps a server is already running on port %s?"
                    port)
        cmd (format "Set another port in %s: {:port %s}"
                    config/file (inc (long port)))]
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

(defn- plugin-env [mode opts cfg store]
  {:dir (:plugins-dir opts plugin/default-dir) :mode mode
   :settings cfg :store store})

(defn- checked-phases
  "Returns the live phases with the plugin systems in them. Throws
  when a system names a phase the tick lacks."
  [systems]
  (let [phases (plugin/live-phases #'tick/phases systems)]
    (phases)
    phases))

(defn- plugged [{:keys [opts settings store world] :as base} report]
  (let [ps (plugin/load-all (plugin-env :server opts @settings store))
        adds (plugin/contributions ps)
        systems (:systems adds)]
    (report {:event :plugins :loaded (map :manifest ps)})
    (swap! world plugin/with-adds adds)
    (let [phases (checked-phases systems)]
      (cond-> (assoc base :plugins ps :adds adds)
        (seq systems) (assoc :phases phases)))))

(defn- prepared [opts cfg l report]
  (let [opened (open-world opts cfg l)]
    (report (host-event (:saved opened) (:config-written? opts)))
    (let [base (plugged opened report)]
      (timed report :load data/load!)
      base)))

(defn- started [opts cfg l]
  (let [report (:report opts (fn [_] nil))
        {:keys [store world saver plugins] :as base}
        (prepared opts cfg l report)
        net (listen base)
        clocks (start-clocks base net)
        held {:world world :saver saver :store store
              :plugins plugins :lock l}
        server (merge held net clocks)
        port (.getLocalPort ^ServerSocket (:socket net))]
    (report {:event :ready :port port :took (uptime-ns)})
    (hooked base server)))

(defn start
  "Starts the server on the configured port and returns its handle.
  The plugins load first, from :plugins-dir or plugins."
  [opts]
  (let [cfg (merge (config/load-config) opts)]
    (locked opts cfg #(started opts cfg %))))

(defn- cli-run [c args opts cfg ^Closeable l]
  (data/load!)
  (let [env (plugin-env :cli opts cfg (open-store opts cfg))
        ps (plugin/load-all env)]
    (try (plugin/run-cli! ps c args)
         (finally (plugin/stop-all! ps) (some-> l .close)))))

(defn run-cli
  "Runs command c of a plugin with args and returns its exit code.
  The world of opts stays locked while it runs, without the network
  and the tick."
  [c args opts]
  (let [cfg (merge (config/load-config) opts)]
    (locked opts cfg #(cli-run c args opts cfg %))))
