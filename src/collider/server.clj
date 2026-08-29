(ns collider.server
  (:require [clojure.data.json :as json]
            [collider.config :as config]
            [collider.game.state :as state]
            [collider.game.tick :as tick]
            [collider.log :as log]
            [collider.net.frame :as frame]
            [collider.persist.snapshot :as snapshot]
            [collider.proto.packets.handshake :as hs]
            [collider.proto.packets.login :as login]
            [collider.proto.packets.play :as play]
            [collider.proto.packets.status :as status]
            [collider.proto.spec :as spec])
  (:import (io.netty.bootstrap ServerBootstrap)
           (io.netty.buffer ByteBuf)
           (io.netty.channel Channel ChannelFutureListener ChannelHandler ChannelHandlerContext
                             ChannelInboundHandlerAdapter ChannelInitializer ChannelOption
                             WriteBufferWaterMark)
           (io.netty.channel.nio NioEventLoopGroup)
           (io.netty.channel.socket SocketChannel)
           (io.netty.channel.socket.nio NioServerSocketChannel)
           (io.netty.handler.codec MessageToMessageDecoder MessageToMessageEncoder)
           (io.netty.util AttributeKey)
           (java.lang.management ManagementFactory)
           (java.util HashSet List Locale)
           (java.util.concurrent ConcurrentLinkedQueue Executors ScheduledExecutorService
                                 ThreadFactory TimeUnit)
           (java.util.concurrent.atomic AtomicInteger))
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^AttributeKey conn-state-key (AttributeKey/valueOf "collider.conn-state"))
(def ^AttributeKey eid-key (AttributeKey/valueOf "collider.eid"))
(defonce ^AtomicInteger next-entity-id (AtomicInteger.))

(defn- conn-state [^ChannelHandlerContext ctx]
  (.get (.attr (.channel ctx) conn-state-key)))

(defn- set-conn-state! [^ChannelHandlerContext ctx state]
  (.set (.attr (.channel ctx) conn-state-key) state))

(defn packet-decoder []
  (proxy [MessageToMessageDecoder] []
    (decode [^ChannelHandlerContext ctx ^ByteBuf frame ^List out]
      (.add out (spec/decode (conn-state ctx) frame)))))

(defn packet-encoder []
  (proxy [MessageToMessageEncoder] []
    (encode [^ChannelHandlerContext ctx m ^List out]
      (let [buf (.buffer (.alloc ctx))]
        (try
          (spec/encode buf m)
          (.add out buf)
          (catch Throwable t
            (.release buf)
            (log/info "encode failed for" (:packet/key m) "-" (str t))
            (throw t)))))))

(defn- status-body [{:keys [motd max-players]}]
  {:version     {:name "1.8.9" :protocol 47}
   :players     {:max max-players :online 0}
   :description {:text motd}})

(defn- send-join-sequence! [^ChannelHandlerContext ctx eid {:keys [max-players]}]
  (.write ctx {:packet/key  ::play/join-game
               :entity-id   eid
               :gamemode    1 :dimension 0 :difficulty 0
               :max-players (min 255 (long max-players))
               :level-type  "flat" :reduced-debug? false})
  (.write ctx {:packet/key ::play/spawn-position :location [24 4 8]})
  (.write ctx {:packet/key ::play/player-abilities
               :flags      0x0D :flying-speed 0.05 :fov-modifier 0.1})
  (.flush ctx))

(defn- do-login! [^ChannelHandlerContext ctx {:keys [conns ^ConcurrentLinkedQueue queue cfg]} username]
  (let [eid (.incrementAndGet next-entity-id)
        ch (.channel ctx)]
    (.set (.attr ch eid-key) eid)
    (swap! conns assoc eid ch)
    (.write ctx {:packet/key ::login/success
                 :uuid       (str (state/offline-uuid username))
                 :username   username})
    (set-conn-state! ctx :play)
    (send-join-sequence! ctx eid cfg)
    (.offer queue [:player-join eid username])
    (log/info "player" username "connected: eid" eid "addr" (str (.remoteAddress ch)))))

(defn- packet->event [k eid m]
  (case k
    ::play/keep-alive-c [:keepalive-echo eid (:id m)]
    ::play/chat-c [:chat eid (:message m)]
    ::play/tab-complete-c [:tab-complete eid (:message m) (:target m)]
    ::play/player [:move eid {:on-ground (:on-ground m)}]
    ::play/player-position [:move eid {:pos       [(:x m) (:y m) (:z m)]
                                       :on-ground (:on-ground m)}]
    ::play/player-look [:move eid {:yaw       (:yaw m) :pitch (:pitch m)
                                   :on-ground (:on-ground m)}]
    ::play/player-position-look [:move eid {:pos       [(:x m) (:y m) (:z m)]
                                            :yaw       (:yaw m) :pitch (:pitch m)
                                            :on-ground (:on-ground m)}]
    ::play/player-digging [:dig eid (:status m) (:location m) (:face m)]
    ::play/player-block-placement [:place eid (:location m) (:face m)
                                   (:item-id m) (:damage m) (:cursor m)]
    ::play/animation-c [:swing eid]
    ::play/entity-action [:entity-action eid (:action m)]
    ::play/client-settings [:client-settings eid (:skin-parts m)]
    ::play/held-item-change [:held-item eid (:slot m)]
    ::play/creative-inventory-action [:creative-slot eid (:slot m) (:stack m)]
    ::play/client-status (when (zero? (long (:action m))) [:respawn eid])
    ::play/use-entity (case (long (:action m))
                        0 [:interact eid (:target m)]
                        1 [:attack eid (:target m)]
                        nil)))

(defn- handle-packet [^ChannelHandlerContext ctx {:keys [^ConcurrentLinkedQueue queue cfg] :as io} m]
  (let [k (:packet/key m)]
    (case k
      ::hs/handshake
      (set-conn-state! ctx (case (long (:next-state m)) 1 :status 2 :login :closed))
      ::status/request
      (.writeAndFlush ctx {:packet/key ::status/response
                           :json       (json/write-str (status-body cfg))})
      ::status/ping
      (-> (.writeAndFlush ctx {:packet/key ::status/pong
                               :payload    (:payload m)})
          (.addListener ChannelFutureListener/CLOSE))
      ::login/login-start
      (do-login! ctx io (:name m))
      (::play/keep-alive-c ::play/chat-c ::play/player ::play/player-position
        ::play/player-look ::play/player-position-look
        ::play/player-digging ::play/player-block-placement
        ::play/animation-c ::play/entity-action ::play/client-settings
        ::play/held-item-change ::play/creative-inventory-action ::play/use-entity
        ::play/client-status
        ::play/tab-complete-c)
      (when-let [eid (.get (.attr (.channel ctx) eid-key))]
        (when-let [ev (packet->event k eid m)]
          (.offer queue ev)))
      nil)))

(defn connection-handler [{:keys [conns ^ConcurrentLinkedQueue queue save!] :as io}]
  (proxy [ChannelInboundHandlerAdapter] []
    (channelActive [^ChannelHandlerContext ctx]
      (set-conn-state! ctx :handshaking))
    (channelInactive [^ChannelHandlerContext ctx]
      (when-let [eid (.getAndSet (.attr (.channel ctx) eid-key) nil)]
        (swap! conns dissoc eid)
        (.offer queue [:player-quit eid])
        (log/info "player disconnected: eid" eid)
        (when save! (save!))))
    (channelRead [^ChannelHandlerContext ctx m]
      (handle-packet ctx io m))
    (exceptionCaught [^ChannelHandlerContext ctx ^Throwable t]
      (log/info "connection error:" (.getMessage t))
      (.close ctx))))

(defn- deliver! [conns outbound]
  (let [cs @conns
        touched (HashSet.)]
    (doseq [[eid pkt] outbound]
      (when-let [^Channel ch (cs eid)]
        (if (= :close pkt)
          (do (log/info "connection closed by server: eid" eid)
              (.flush ch) (.close ch))
          (do (.write ch pkt)
              (.add touched ch)))))
    (doseq [^Channel ch touched]
      (.flush ch))))

(defn- saver-scheduler
  ^ScheduledExecutorService [save! ^long period-ms]
  (doto (Executors/newSingleThreadScheduledExecutor
          (reify ThreadFactory
            (newThread [_ r] (doto (Thread. ^Runnable r "collider-saver-timer")
                               (.setDaemon true)))))
    (.scheduleWithFixedDelay ^Runnable save! period-ms period-ms TimeUnit/MILLISECONDS)))

(defn- pipeline-initializer [io]
  (proxy [ChannelInitializer] []
    (initChannel [^SocketChannel ch]
      (doto (.pipeline ch)
        (.addLast "framer" ^ChannelHandler (frame/varint-frame-decoder))
        (.addLast "frame-encoder" ^ChannelHandler (frame/varint-frame-encoder))
        (.addLast "packet-decoder" ^ChannelHandler (packet-decoder))
        (.addLast "packet-encoder" ^ChannelHandler (packet-encoder))
        (.addLast "handler" ^ChannelHandler (connection-handler io))))))

(defn- netty-server [io port]
  (let [boss (NioEventLoopGroup. 1)
        worker (NioEventLoopGroup.)
        bootstrap (doto (ServerBootstrap.)
                    (.group boss worker)
                    (.channel NioServerSocketChannel)
                    (.childOption ChannelOption/TCP_NODELAY Boolean/TRUE)
                    (.childOption ChannelOption/WRITE_BUFFER_WATER_MARK
                                  (WriteBufferWaterMark. (* 512 1024) (* 1024 1024)))
                    (.childHandler (pipeline-initializer io)))]
    {:boss    boss
     :worker  worker
     :channel (-> bootstrap (.bind (int port)) .sync .channel)}))

(defn- shutdown-hook! [saver save-file world]
  (let [t (Thread. ^Runnable #(snapshot/stop-saver! saver save-file @world)
                   "collider-shutdown-save")]
    (.addShutdownHook (Runtime/getRuntime) t)
    t))

(defn- log-startup! [saved save-file ^Channel channel]
  (when (seq (:chunks saved))
    (log/info "world loaded:" (count (:chunks saved)) "chunks, time"
              (:time-of-day saved 0) "from" (str save-file)))
  (let [up-ms (double (.getUptime (ManagementFactory/getRuntimeMXBean)))]
    (log/info (String/format Locale/ROOT
                             "done (%.3fs), on %s"
                             (object-array [(/ up-ms 1000.0)
                                            (str (.localAddress channel))])))))

(defn start [opts]
  (let [cfg (merge (config/load-config) opts)
        {:keys [port save-file save-period-ms]} cfg
        saved (when save-file (snapshot/load-snapshot save-file))
        world (atom (assoc (merge state/initial-world saved)
                      :config (select-keys cfg [:view-distance :simulation-distance :chunk-send-rate])))
        saver (when save-file (snapshot/start-saver saved))
        save! (when saver #(snapshot/request-save! saver save-file @world))
        queue (ConcurrentLinkedQueue.)
        conns (atom {})
        io {:queue queue :conns conns :save! save! :cfg cfg}
        {:keys [boss worker channel]} (netty-server io port)
        writable (fn [] {:writable (persistent!
                                     (reduce-kv (fn [acc eid ^Channel ch]
                                                  (if (.isWritable ch) (conj! acc eid) acc))
                                                (transient #{})
                                                @conns))})
        ticker (tick/start-ticker! world queue #(deliver! conns %) {:io-input writable})
        sched (when saver (saver-scheduler save! save-period-ms))
        hook (when saver (shutdown-hook! saver save-file world))]
    (log-startup! saved save-file channel)
    {:channel    channel :boss boss :worker worker
     :world      world :queue queue :conns conns :ticker ticker
     :tick-stats (:stats ticker)
     :saver      saver :save-file save-file :scheduler sched :shutdown-hook hook}))

(defn stop [{:keys [^Channel channel ^NioEventLoopGroup boss ^NioEventLoopGroup worker ticker
                    saver save-file world ^ScheduledExecutorService scheduler
                    ^Thread shutdown-hook]}]
  (some-> scheduler .shutdownNow)
  (when shutdown-hook
    (try (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
         (catch IllegalStateException _ nil)))
  (some-> ticker tick/stop-ticker!)
  (some-> channel .close .sync)
  (when saver (snapshot/stop-saver! saver save-file @world))
  (.shutdownGracefully worker)
  (.shutdownGracefully boss)
  (log/info "server stopped")
  nil)

(defn -main [& _]
  (config/write-default!)
  (let [{:keys [^Channel channel]} (start {})]
    (-> channel .closeFuture .sync)))

(comment
  (def srv (collider.server/start {}))
  (dotimes [_ 600] (.offer (:queue srv) [:place 1 [0 4 0] 1 383 91 [8 8 8]]))
  (def radius 20)
  (doseq [x (range (- radius) (inc radius))
          z (range (- radius) (inc radius))
          :when (or (= radius (abs x)) (= radius (abs z)))]
    (.offer (:queue srv) [:place 1 [x 3 z] 1 85 0 [8 8 8]]))
  (collider.server/stop srv)
  )
