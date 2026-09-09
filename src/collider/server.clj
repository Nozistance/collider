(ns collider.server
  (:require [collider.game.commands :as commands]
            [clojure.data.json :as json]
            [collider.config :as config]
            [collider.data :as data]
            [collider.game.state :as state]
            [collider.game.tick :as tick]
            [collider.log :as log]
            [collider.persist.snapshot :as snapshot]
            [collider.proto.codec :as c]
            [collider.proto.packets :as packets]
            [collider.render :as render])
  (:import (collider.java Buf)
           (java.io BufferedInputStream BufferedOutputStream EOFException)
           (java.net ServerSocket Socket SocketException)
           (java.util.concurrent ArrayBlockingQueue ConcurrentLinkedQueue Executors
                                 ScheduledExecutorService ThreadFactory TimeUnit)
           (java.util.concurrent.atomic AtomicBoolean AtomicInteger)
           (java.util.zip Deflater Inflater))
  (:gen-class))

(set! *warn-on-reflection* true)

(defonce ^AtomicInteger next-entity-id (AtomicInteger.))

(def ^:private ^:const out-queue-size 4096)
(def ^:private ^:const out-queue-high 1024)
(def ^:private ^:const writer-poll-ms 500)

(defrecord Conn [^Socket sock ^ArrayBlockingQueue q st ^AtomicBoolean closing])

(defn- conn-state [^Conn c] (:state @(:st c)))

(defn- who [^Conn c]
  (let [{:keys [name eid addr]} @(:st c)]
    (str (or name addr) (when eid (str " (eid " eid ")")))))

(defn- set-conn-state! [^Conn c s] (swap! (:st c) assoc :state s))

(defn- close! [^Conn c]
  (.set ^AtomicBoolean (:closing c) true)
  (.offer ^ArrayBlockingQueue (:q c) [:close]))

(defn- send! [^Conn c m]
  (when-not (.offer ^ArrayBlockingQueue (:q c) [:packet (conn-state c) m])
    (log/info "output queue full, closing" (who c))
    (.set ^AtomicBoolean (:closing c) true)
    (.close ^Socket (:sock c))))

(defn- writer-loop [^Conn c]
  (let [^Socket sock (:sock c)
        ^ArrayBlockingQueue q (:q c)
        ^AtomicBoolean closing (:closing c)
        out     (BufferedOutputStream. (.getOutputStream sock))
        payload (Buf. 1024)
        body    (Buf. 1024)
        head    (Buf. 5)
        defl    (Deflater.)
        chunk   (byte-array 8192)]
    (try
      (loop [threshold -1]
        (let [x (.poll q writer-poll-ms TimeUnit/MILLISECONDS)]
          (cond
            (nil? x)
            (when-not (.get closing) (recur threshold))

            (= :packet (nth x 0))
            (let [[_ state m] x]
              (try
                (.clear payload)
                (packets/encode! state payload m)
                (c/write-frame! out payload body head threshold defl chunk)
                (catch Throwable t
                  (log/info "encode failed for" (:packet m) "-" (str t))))
              (when (.isEmpty q) (.flush out))
              (recur threshold))

            (= :threshold (nth x 0))
            (let [n (long (nth x 1))]
              (swap! (:st c) assoc :threshold n)
              (.flush out)
              (recur n)))))
      (finally
        (try (.flush out) (catch Throwable _ nil))
        (.end defl)
        (.close sock)))))

(defn- status-body [{:keys [motd max-players]}]
  {:version     {:name c/game-version :protocol c/protocol-version}
   :players     {:max max-players :online 0}
   :description {:text motd}})

(def ^:private known-pack ["minecraft" "core" c/game-version])

(defn- start-configuration! [^Conn conn]
  (send! conn {:packet :custom-payload :channel :brand :value "collider"})
  (send! conn {:packet :update-enabled-features :features [:vanilla]})
  (send! conn {:packet :select-known-packs :packs [known-pack]}))

(defn- finish-configuration! [^Conn conn]
  (doseq [[registry names] @data/datapack]
    (send! conn {:packet :registry-data :registry registry :names names}))
  (send! conn {:packet :update-tags :tags @data/tags})
  (send! conn {:packet :finish-configuration}))

(def ^:private overworld (delay (data/datapack-id "dimension_type" :overworld)))

(def ^:private command-tree (delay (commands/tree)))
(def ^:private world-border-size 5.9999968E7)
(def ^:private world-border-max 29999984)
(def ^:private op-level-event 24)

(defn- send-join-burst!
  "What a vanilla server sends between login and the first chunks, in its
   order: creative abilities, peaceful, operator level 4, MOTD, world border,
   spawn, tick rate, health and experience."
  [^Conn conn eid {:keys [max-players view-distance simulation-distance motd]}]
  (let [[x y z] state/spawn-pos]
    (send! conn {:packet :login :eid eid
                 :max-players (min 255 (long max-players))
                 :view-distance view-distance
                 :simulation-distance simulation-distance
                 :dimension-type @overworld})
    (send! conn {:packet :change-difficulty :difficulty 0 :locked false})
    (send! conn {:packet :player-abilities :flags (bit-or 1 4 8)
                 :flying-speed 0.05 :walking-speed 0.1})
    (send! conn {:packet :entity-event :eid eid :event (+ op-level-event 4)})
    (send! conn {:packet :commands :nodes @command-tree})
    (send! conn {:packet :server-data :motd motd})
    (send! conn {:packet :initialize-border :size world-border-size :max-size world-border-max})
    (send! conn {:packet :set-default-spawn-position :pos [(long x) (long y) (long z)]})
    (send! conn {:packet :game-event :event 13 :value 0.0})
    (send! conn {:packet :ticking-state :rate 20.0 :frozen? false})
    (send! conn {:packet :ticking-step :steps 0})
    (send! conn {:packet :set-health :health 20.0 :food 20 :saturation 5.0})
    (send! conn {:packet :set-experience :progress 0.0 :level 0 :total 0})
    (send! conn {:packet :update-attributes :eid eid
                         :attributes [[:entity-interaction-range 3.0]
                                      [:movement-speed 0.1]
                                      [:block-interaction-range 4.5]]})))

(defn- do-login! [^Conn conn {:keys [conns ^ConcurrentLinkedQueue queue cfg]}]
  (let [nm  (:name @(:st conn))
        eid (.incrementAndGet next-entity-id)]
    (swap! (:st conn) assoc :eid eid)
    (swap! conns assoc eid conn)
    (set-conn-state! conn :play)
    (send-join-burst! conn eid cfg)
    (.offer queue [:player-join eid nm])
    (log/info "player" nm "connected: eid" eid "addr" (:addr @(:st conn)))))

(defn- on-ground? [m] (odd? (long (:flags m))))

(defn- invalid-move?
  "NaN coordinates or a non-finite rotation (vanilla containsInvalidValues)."
  [m]
  (or (some #(Double/isNaN (double %)) (:pos m))
      (some #(Double/isInfinite (double %)) (:pos m))
      (some #(not (Double/isFinite (double %))) (keep m [:yaw :pitch]))))

(defn- packet->event [eid {:keys [packet] :as m}]
  (case packet
    :keep-alive              [:keepalive-echo eid (:id m)]
    :chunk-batch-received    [:chunk-batch-ack eid (:rate m)]
    :move-player-pos         [:move eid {:pos (:pos m) :on-ground (on-ground? m)}]
    :move-player-pos-rot     [:move eid {:pos (:pos m) :yaw (:yaw m) :pitch (:pitch m)
                                         :on-ground (on-ground? m)}]
    :move-player-rot         [:move eid {:yaw (:yaw m) :pitch (:pitch m) :on-ground (on-ground? m)}]
    :move-player-status-only [:move eid {:on-ground (on-ground? m)}]
    :accept-teleportation    [:teleport-ack eid (:id m)]
    :player-abilities        [:move eid {:flying (bit-test (long (:flags m)) 1)}]
    :player-action           [:dig eid (:action m) (:pos m) (:face m) (:sequence m)]
    :use-item-on             (let [[cx cy cz] (:cursor m)]
                               [:place eid (:pos m) (:face m) nil
                                [(long (* 16 (double cx))) (long (* 16 (double cy))) (long (* 16 (double cz)))]
                                (:sequence m)])
    :use-item                [:place eid [-1 -1 -1] -1 nil [0 0 0] (:sequence m) {:yaw (:yaw m) :pitch (:pitch m)}]
    :swing                   [:swing eid]
    :player-command          [:entity-action eid (:action m)]
    :player-input            [:input eid {:sneaking? (bit-test (long (:flags m)) 5)}]
    :set-carried-item        [:held-item eid (:slot m)]
    :pick-item-from-block    [:pick eid {:pos (:pos m)}]
    :pick-item-from-entity   [:pick eid {:entity (:id m)}]
    :set-creative-mode-slot  [:creative-slot eid (:slot m) (:stack m)]
    :container-click         (when (zero? (long (:container m))) [:click eid (dissoc m :packet :container)])
    :client-command          (case (long (:action m)) 0 [:respawn eid] 1 [:stats-request eid] 2 [:rules-request eid] nil)
    :set-game-rule           [:set-rules eid (:entries m)]
    :command-suggestion      [:tab-complete eid (:text m) nil (:id m)]
    :interact                (case (long (:action m))
                               0 [:interact eid (:target m)]
                               1 [:attack eid (:target m)]
                               nil)
    :chat                    [:chat eid (:message m)]
    :chat-command            [:chat eid (str "/" (:command m))]
    :sign-update             [:sign-update eid (:pos m) (:front? m) (:lines m)]
    nil))

(def ^:private ignored
  "Serverbound packets with nothing to do: bookkeeping the client does on
   its own, or content outside the frame (trading, books, structures,
   spectators, vehicles, signed chat)."
  #{:client-information :player-loaded :client-tick-end :custom-payload
    :chat-session-update :chat-ack :container-close :configuration-acknowledged
    :cookie-response :custom-click-action :debug-subscription-request
    :chat-command-signed :pong :bundle-item-selected :block-entity-tag-query
    :entity-tag-query :edit-book :jigsaw-generate :lock-difficulty
    :change-difficulty :move-vehicle :paddle-boat :place-recipe
    :recipe-book-change-settings :recipe-book-seen-recipe :rename-item
    :resource-pack :seen-advancements :select-trade :set-beacon
    :set-command-block :set-command-minecart :set-jigsaw-block
    :set-structure-block :set-test-block :spectator-action
    :teleport-to-entity :test-instance-block-action})

(def ^:private later
  "Serverbound packets of categories still to come (ticket 028): inventory
   screens, signs, entity interaction, commands, game mode."
  #{:container-button-click :container-slot-state-changed
    :attack :change-game-mode})

(def ^:private unhandled (atom #{}))

(defn- log-unhandled! [packet]
  (when-not (or (ignored packet) (later packet) (@unhandled packet))
    (swap! unhandled conj packet)
    (log/info "play:" packet "not handled")))

(defn- setup-compression!
  "Tells the client the threshold and, once that packet is out, compresses
   both ways (vanilla setupCompression). Negative threshold: no compression."
  [^Conn conn ^long threshold]
  (when-not (neg? threshold)
    (send! conn {:packet :login-compression :threshold threshold})
    (.offer ^ArrayBlockingQueue (:q conn) [:threshold threshold])))

(defn- handle-packet [^Conn conn {:keys [^ConcurrentLinkedQueue queue cfg] :as io} m]
  (case [(conn-state conn) (:packet m)]
    [:handshake :intention]
    (set-conn-state! conn (case (long (:next m)) 1 :status 2 :login :closed))

    [:status :status-request]
    (send! conn {:packet :status-response :json (json/write-str (status-body cfg))})

    [:status :ping-request]
    (do (send! conn {:packet :pong-response :payload (:payload m)})
        (close! conn))

    [:play :ping-request]
    (send! conn {:packet :pong-response :payload (:payload m)})

    [:login :hello]
    (let [nm (:name m)]
      (swap! (:st conn) assoc :name nm)
      (setup-compression! conn (long (:compression-threshold cfg -1)))
      (send! conn {:packet :login-finished :uuid (c/offline-uuid nm) :name nm}))

    [:login :login-acknowledged]
    (do (set-conn-state! conn :configuration)
        (start-configuration! conn))

    [:configuration :select-known-packs]
    (finish-configuration! conn)

    [:configuration :finish-configuration]
    (do-login! conn io)

    (when (= :play (conn-state conn))
      (when-let [eid (:eid @(:st conn))]
        (if (and (#{:move-player-pos :move-player-pos-rot :move-player-rot} (:packet m)) (invalid-move? m))
          (do (send! conn {:packet :disconnect :text {:translate "multiplayer.disconnect.invalid_player_movement"}})
              (close! conn))
          (if-let [ev (packet->event eid m)]
            (.offer queue ev)
            (log-unhandled! (:packet m))))))))

(defn- reader-loop [^Conn conn io]
  (let [in   (BufferedInputStream. (.getInputStream ^Socket (:sock conn)))
        buf  (Buf. 2048)
        infl (Inflater.)]
    (try
      (loop []
        (let [raw   (c/read-frame! in buf)
              frame (c/decompress! raw (long (:threshold @(:st conn))) infl)]
          (when-let [m (packets/decode (conn-state conn) frame)]
            (handle-packet conn io m)))
        (recur))
      (finally (.end infl)))))

(defn- disconnected! [^Conn conn {:keys [conns ^ConcurrentLinkedQueue queue save!]}]
  (when-let [eid (:eid (first (swap-vals! (:st conn) dissoc :eid)))]
    (swap! conns dissoc eid)
    (.offer queue [:player-quit eid])
    (log/info "player disconnected: eid" eid)
    (when save! (save!))))

(defn- serve-conn! [^Socket sock io]
  (let [conn (->Conn sock (ArrayBlockingQueue. out-queue-size)
                     (atom {:state :handshake :threshold -1
                            :addr (str (.getRemoteSocketAddress sock))})
                     (AtomicBoolean. false))]
    (Thread/startVirtualThread
      #(try (writer-loop conn)
            (catch InterruptedException _ nil)
            (catch SocketException _ nil)
            (catch Throwable t
              (log/info "writer failed for" (who conn) "-" (str t)))))
    (try
      (reader-loop conn io)
      (catch EOFException _ nil)
      (catch SocketException _ nil)
      (catch Throwable t
        (when-not (.isClosed sock)
          (log/info "reader failed for" (who conn) "-" (str t))))
      (finally
        (disconnected! conn io)
        (close! conn)))))

(defn- writable-eids [conns]
  (into #{}
        (keep (fn [[eid ^Conn conn]]
                (when (< (.size ^ArrayBlockingQueue (:q conn)) out-queue-high) eid)))
        @conns))

(defn- deliver! [conns world deltas]
  (let [cs @conns]
    (doseq [[eid pkt] (render/render world deltas)]
      (when-let [^Conn conn (cs eid)]
        (if (= :close pkt)
          (close! conn)
          (send! conn pkt))))))

(defn- saver-scheduler ^ScheduledExecutorService [save! ^long period-ms]
  (doto (Executors/newSingleThreadScheduledExecutor
          (reify ThreadFactory
            (newThread [_ r] (doto (Thread. ^Runnable r "collider-saver-timer")
                               (.setDaemon true)))))
    (.scheduleWithFixedDelay ^Runnable save! period-ms period-ms TimeUnit/MILLISECONDS)))

(defn- shutdown-hook! [saver save-file world]
  (let [t (Thread. ^Runnable #(snapshot/stop-saver! saver save-file @world) "collider-shutdown-save")]
    (.addShutdownHook (Runtime/getRuntime) t)
    t))

(defn- accept-loop [^ServerSocket srv io]
  (loop []
    (when-not (.isClosed srv)
      (let [sock (try (.accept srv)
                      (catch Throwable t
                        (when-not (.isClosed srv)
                          (log/info "accept failed:" (str t))
                          (Thread/sleep 100))
                        nil))]
        (when sock
          (.setTcpNoDelay ^Socket sock true)
          (Thread/startVirtualThread #(serve-conn! sock io)))
        (recur)))))

(defn- listen! [io port]
  (let [srv (ServerSocket. (int port))]
    {:socket srv
     :accept (Thread/startVirtualThread #(accept-loop srv io))}))

(defn start
  "Starts the server with opts merged over config.edn and returns the handle
   `stop` takes. :store overrides where the world is saved."
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
        io {:queue queue :conns conns :cfg cfg :save! save!}
        {^ServerSocket srv :socket accept :accept} (listen! io (:port cfg))
        ticker (tick/start-ticker! world queue (fn [w d] (deliver! conns w d))
                                   {:io-input #(hash-map :writable (writable-eids conns))})
        sched (when saver (saver-scheduler save! save-period-ms))
        hook (when saver (shutdown-hook! saver store world))]
    (when (seq (:chunks saved))
      (log/info "world loaded:" (count (:chunks saved)) "chunks," (count (:entities saved)) "entities from" (str store)))
    (log/info "collider" c/game-version "(protocol" (str c/protocol-version ") on")
              (str (.getLocalSocketAddress srv)))
    {:socket srv :accept accept
     :world world :queue queue :conns conns :ticker ticker
     :tick-stats (:stats ticker)
     :saver saver :store store :scheduler sched :shutdown-hook hook}))

(defn stop [{:keys [^ServerSocket socket conns ticker
                    saver store world ^ScheduledExecutorService scheduler ^Thread shutdown-hook]}]
  (some-> scheduler .shutdownNow)
  (when shutdown-hook
    (try (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
         (catch IllegalStateException _ nil)))
  (some-> ticker tick/stop-ticker!)
  (some-> socket .close)
  (doseq [[_ ^Conn conn] @conns]
    (close! conn)
    (.close ^Socket (:sock conn)))
  (when saver (snapshot/stop-saver! saver store @world))
  (log/info "server stopped")
  nil)

(defn -main [& _]
  (config/write-default!)
  (let [{:keys [^Thread accept]} (start {})]
    (.join accept)))
