(ns collider.server
  "Netty server for 26.2 (protocol 776). Connection states: handshake, status,
   login, configuration, play. Packets become events in the tick queue;
   `deliver!` renders the deltas of a tick and writes the packets."
  (:require [clojure.data.json :as json]
            [collider.config :as config]
            [collider.data :as data]
            [collider.game.state :as state]
            [collider.game.tick :as tick]
            [collider.log :as log]
            [collider.persist.snapshot :as snapshot]
            [collider.proto.codec :as c]
            [collider.proto.packets :as packets]
            [collider.render :as render])
  (:import (io.netty.bootstrap ServerBootstrap)
           (io.netty.buffer ByteBuf)
           (io.netty.channel Channel ChannelFutureListener ChannelHandler ChannelHandlerContext
                             ChannelInboundHandlerAdapter ChannelInitializer ChannelOption
                             MultiThreadIoEventLoopGroup)
           (io.netty.channel.nio NioIoHandler)
           (io.netty.channel.socket SocketChannel)
           (io.netty.channel.socket.nio NioServerSocketChannel)
           (io.netty.handler.codec MessageToMessageDecoder MessageToMessageEncoder)
           (io.netty.util AttributeKey)
           (java.util HashSet List)
           (java.util.concurrent ConcurrentLinkedQueue Executors ScheduledExecutorService
                                 ThreadFactory TimeUnit)
           (java.util.concurrent.atomic AtomicInteger))
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^AttributeKey conn-state-key (AttributeKey/valueOf "collider.conn-state"))
(def ^AttributeKey eid-key (AttributeKey/valueOf "collider.eid"))
(def ^AttributeKey name-key (AttributeKey/valueOf "collider.name"))
(defonce ^AtomicInteger next-entity-id (AtomicInteger.))

(defn- conn-state [^ChannelHandlerContext ctx]
  (.get (.attr (.channel ctx) conn-state-key)))

(defn- set-conn-state! [^ChannelHandlerContext ctx s]
  (.set (.attr (.channel ctx) conn-state-key) s))

(defn packet-decoder []
  (proxy [MessageToMessageDecoder] []
    (decode [^ChannelHandlerContext ctx ^ByteBuf frame ^List out]
      (when-let [m (packets/decode (conn-state ctx) frame)]
        (.add out m)))))

(defn packet-encoder []
  (proxy [MessageToMessageEncoder] []
    (encode [^ChannelHandlerContext ctx m ^List out]
      (let [buf (.buffer (.alloc ctx))]
        (try
          (packets/encode! (conn-state ctx) buf m)
          (.add out buf)
          (catch Throwable t
            (.release buf)
            (log/info "encode failed for" (:packet m) "-" (str t))
            (throw t)))))))

(defn- status-body [{:keys [motd max-players]}]
  {:version     {:name c/game-version :protocol c/protocol-version}
   :players     {:max max-players :online 0}
   :description {:text motd}})

(def ^:private known-pack ["minecraft" "core" c/game-version])

(defn- start-configuration! [^ChannelHandlerContext ctx]
  (.write ctx {:packet :custom-payload :channel :brand :value "collider"})
  (.write ctx {:packet :update-enabled-features :features [:vanilla]})
  (.writeAndFlush ctx {:packet :select-known-packs :packs [known-pack]}))

(defn- finish-configuration! [^ChannelHandlerContext ctx]
  (doseq [[registry names] @data/datapack]
    (.write ctx {:packet :registry-data :registry registry :names names}))
  (.write ctx {:packet :update-tags :tags @data/tags})
  (.writeAndFlush ctx {:packet :finish-configuration}))

(def ^:private overworld (delay (data/datapack-id "dimension_type" :overworld)))

(defn- send-join-burst! [^ChannelHandlerContext ctx eid {:keys [max-players view-distance simulation-distance]}]
  (let [[x y z] state/spawn-pos]
    (.write ctx {:packet :login :eid eid
                 :max-players (min 255 (long max-players))
                 :view-distance view-distance
                 :simulation-distance simulation-distance
                 :dimension-type @overworld})
    (.write ctx {:packet :player-abilities :flags (bit-or 1 4 8)
                 :flying-speed 0.05 :walking-speed 0.1})
    (.write ctx {:packet :set-default-spawn-position :pos [(long x) (long y) (long z)]})
    (.write ctx {:packet :set-health :health 20.0 :food 20 :saturation 5.0})
    (.write ctx {:packet :set-experience :progress 0.0 :level 0 :total 0})
    (.writeAndFlush ctx {:packet :game-event :event 13 :value 0.0})))

(defn- do-login! [^ChannelHandlerContext ctx {:keys [conns ^ConcurrentLinkedQueue queue cfg]}]
  (let [ch  (.channel ctx)
        nm  (.get (.attr ch name-key))
        eid (.incrementAndGet next-entity-id)]
    (.set (.attr ch eid-key) eid)
    (swap! conns assoc eid ch)
    (set-conn-state! ctx :play)
    (send-join-burst! ctx eid cfg)
    (.offer queue [:player-join eid nm])
    (log/info "player" nm "connected: eid" eid "addr" (str (.remoteAddress ch)))))

(defn- on-ground? [m] (odd? (long (:flags m))))

(defn- packet->event [eid {:keys [packet] :as m}]
  (case packet
    :keep-alive              [:keepalive-echo eid (:id m)]
    :chunk-batch-received    [:chunk-batch-ack eid (:rate m)]
    :move-player-pos         [:move eid {:pos (:pos m) :on-ground (on-ground? m)}]
    :move-player-pos-rot     [:move eid {:pos (:pos m) :yaw (:yaw m) :pitch (:pitch m)
                                         :on-ground (on-ground? m)}]
    :move-player-rot         [:move eid {:yaw (:yaw m) :pitch (:pitch m) :on-ground (on-ground? m)}]
    :move-player-status-only [:move eid {:on-ground (on-ground? m)}]
    :player-abilities        [:move eid {:flying (bit-test (long (:flags m)) 1)}]
    :player-action           [:dig eid (:action m) (:pos m) (:face m) (:sequence m)]
    :use-item-on             (let [[cx cy cz] (:cursor m)]
                               [:place eid (:pos m) (:face m) nil
                                [(long (* 16 (double cx))) (long (* 16 (double cy))) (long (* 16 (double cz)))]
                                (:sequence m)])
    :use-item                [:place eid [-1 -1 -1] -1 nil [0 0 0] (:sequence m)]
    :swing                   [:swing eid]
    :player-command          [:entity-action eid (:action m)]
    :player-input            [:input eid {:sneaking? (bit-test (long (:flags m)) 5)}]
    :set-carried-item        [:held-item eid (:slot m)]
    :pick-item-from-block    [:pick eid {:pos (:pos m)}]
    :pick-item-from-entity   [:pick eid {:entity (:id m)}]
    :set-creative-mode-slot  [:creative-slot eid (:slot m) (:stack m)]
    :client-command          (when (zero? (long (:action m))) [:respawn eid])
    :interact                (case (long (:action m))
                               0 [:interact eid (:target m)]
                               1 [:attack eid (:target m)]
                               nil)
    :chat                    [:chat eid (:message m)]
    :chat-command            [:chat eid (str "/" (:command m))]
    nil))

(def ^:private ignored
  #{:accept-teleportation :client-information :player-loaded
    :client-tick-end :custom-payload :chat-session-update :chat-ack
    :container-close})

(def ^:private unhandled (atom #{}))

(defn- log-unhandled! [packet]
  (when-not (or (ignored packet) (@unhandled packet))
    (swap! unhandled conj packet)
    (log/info "play:" packet "not handled")))

(defn- handle-packet [^ChannelHandlerContext ctx {:keys [^ConcurrentLinkedQueue queue cfg] :as io} m]
  (case [(conn-state ctx) (:packet m)]
    [:handshake :intention]
    (set-conn-state! ctx (case (long (:next m)) 1 :status 2 :login :closed))

    [:status :status-request]
    (.writeAndFlush ctx {:packet :status-response :json (json/write-str (status-body cfg))})

    [:status :ping-request]
    (-> (.writeAndFlush ctx {:packet :pong-response :payload (:payload m)})
        (.addListener ChannelFutureListener/CLOSE))

    [:login :hello]
    (let [nm (:name m)]
      (.set (.attr (.channel ctx) name-key) nm)
      (.writeAndFlush ctx {:packet :login-finished :uuid (c/offline-uuid nm) :name nm}))

    [:login :login-acknowledged]
    (do (set-conn-state! ctx :configuration)
        (start-configuration! ctx))

    [:configuration :select-known-packs]
    (finish-configuration! ctx)

    [:configuration :finish-configuration]
    (do-login! ctx io)

    (when (= :play (conn-state ctx))
      (when-let [eid (.get (.attr (.channel ctx) eid-key))]
        (if-let [ev (packet->event eid m)]
          (.offer queue ev)
          (log-unhandled! (:packet m)))))))

(defn connection-handler [{:keys [conns ^ConcurrentLinkedQueue queue save!] :as io}]
  (proxy [ChannelInboundHandlerAdapter] []
    (channelActive [^ChannelHandlerContext ctx]
      (set-conn-state! ctx :handshake))
    (channelInactive [^ChannelHandlerContext ctx]
      (when-let [eid (.getAndSet (.attr (.channel ctx) eid-key) nil)]
        (swap! conns dissoc eid)
        (.offer queue [:player-quit eid])
        (log/info "player disconnected: eid" eid)
        (when save! (save!))))
    (channelRead [^ChannelHandlerContext ctx m]
      (try (handle-packet ctx io m)
           (catch Exception e
             (log/info "handler error:" (.getMessage e))
             (.close ctx))))
    (exceptionCaught [^ChannelHandlerContext ctx ^Throwable t]
      (log/info "connection error:" (.getMessage t))
      (.close ctx))))

(defn- deliver! [conns world deltas]
  (let [cs @conns
        touched (HashSet.)]
    (doseq [[eid pkt] (render/render world deltas)]
      (when-let [^Channel ch (cs eid)]
        (if (= :close pkt)
          (do (.flush ch) (.close ch))
          (do (.write ch pkt)
              (.add touched ch)))))
    (doseq [^Channel ch touched]
      (.flush ch))))

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

(defn- pipeline-initializer [io]
  (proxy [ChannelInitializer] []
    (initChannel [^SocketChannel ch]
      (doto (.pipeline ch)
        (.addLast "framer" ^ChannelHandler (c/frame-decoder))
        (.addLast "frame-encoder" ^ChannelHandler (c/frame-encoder))
        (.addLast "packet-decoder" ^ChannelHandler (packet-decoder))
        (.addLast "packet-encoder" ^ChannelHandler (packet-encoder))
        (.addLast "handler" ^ChannelHandler (connection-handler io))))))

(defn- netty-server [io port]
  (let [boss   (MultiThreadIoEventLoopGroup. 1 (NioIoHandler/newFactory))
        worker (MultiThreadIoEventLoopGroup. (NioIoHandler/newFactory))
        bootstrap (doto (ServerBootstrap.)
                    (.group boss worker)
                    (.channel NioServerSocketChannel)
                    (.childOption ChannelOption/TCP_NODELAY Boolean/TRUE)
                    (.childHandler (pipeline-initializer io)))]
    {:boss    boss
     :worker  worker
     :channel (-> bootstrap (.bind (int port)) .sync .channel)}))

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
        {:keys [boss worker channel]} (netty-server io (:port cfg))
        ticker (tick/start-ticker! world queue (fn [w d] (deliver! conns w d)) nil)
        sched (when saver (saver-scheduler save! save-period-ms))
        hook (when saver (shutdown-hook! saver store world))]
    (when (seq (:chunks saved))
      (log/info "world loaded:" (count (:chunks saved)) "chunks," (count (:entities saved)) "entities from" (str store)))
    (log/info "collider" c/game-version "(protocol" (str c/protocol-version ") on")
              (str (.localAddress ^Channel channel)))
    {:channel channel :boss boss :worker worker
     :world world :queue queue :conns conns :ticker ticker
     :tick-stats (:stats ticker)
     :saver saver :store store :scheduler sched :shutdown-hook hook}))

(defn stop [{:keys [^Channel channel ^MultiThreadIoEventLoopGroup boss ^MultiThreadIoEventLoopGroup worker ticker
                    saver store world ^ScheduledExecutorService scheduler ^Thread shutdown-hook]}]
  (some-> scheduler .shutdownNow)
  (when shutdown-hook
    (try (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
         (catch IllegalStateException _ nil)))
  (some-> ticker tick/stop-ticker!)
  (some-> channel .close .sync)
  (when saver (snapshot/stop-saver! saver store @world))
  (.shutdownGracefully worker)
  (.shutdownGracefully boss)
  (log/info "server stopped")
  nil)

(defn -main [& _]
  (config/write-default!)
  (let [{:keys [^Channel channel]} (start {})]
    (-> channel .closeFuture .sync)))
