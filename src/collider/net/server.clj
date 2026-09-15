(ns collider.net.server
  "Serving the players who connect."
  (:require [collider.log :as log]
            [collider.proto.codec :as c]
            [collider.proto.packets :as packets])
  (:import (collider.java Buf)
           (java.io BufferedInputStream BufferedOutputStream EOFException)
           (java.net ServerSocket Socket SocketException SocketTimeoutException)
           (java.util.concurrent ArrayBlockingQueue ConcurrentLinkedQueue TimeUnit)
           (java.util.concurrent.atomic AtomicBoolean AtomicInteger)
           (java.util.zip Deflater Inflater)))

(set! *warn-on-reflection* true)

(def ^:private ^:const out-queue-size 4096)
(def ^:private ^:const out-queue-high 1024)
(def ^:private ^:const writer-poll-ms 500)
(def ^:private ^:const read-timeout-ms 30000)
(def ^:private ^:const default-max-connections 256)
(defrecord Conn [^Socket sock ^ArrayBlockingQueue q st ^AtomicBoolean closing])

(defn conn-state
  "Returns the state the connection c is in."
  [^Conn c] (:state @(:st c)))
(defn info
  "Returns the state map of the connection c."
  [^Conn c] @(:st c))
(defn put!
  "Records k as v on the connection c."
  [^Conn c k v] (swap! (:st c) assoc k v))
(defn- who
  "Returns the name of the connection c for logging."
  [^Conn c]
  (let [{:keys [name eid addr]} @(:st c)]
    (str (or name addr) (when eid (str " (eid " eid ")")))))

(defn set-conn-state!
  "Moves the connection c to state s."
  [^Conn c s] (swap! (:st c) assoc :state s))
(defn close!
  "Closes the connection c once everything already sent has gone out."
  [^Conn c]
  (.set ^AtomicBoolean (:closing c) true)
  (.offer ^ArrayBlockingQueue (:q c) [:close]))

(defn send!
  "Sends m to the connection c."
  [^Conn c m]
  (when-not (.offer ^ArrayBlockingQueue (:q c) [:packet (conn-state c) m])
    (log/info "output queue full, closing" (who c))
    (.set ^AtomicBoolean (:closing c) true)
    (.close ^Socket (:sock c))))

(defn compress!
  "Compresses everything above threshold on the connection c from here on."
  [^Conn c ^long threshold]
  (.offer ^ArrayBlockingQueue (:q c) [:threshold threshold]))

(defn- encode-packet! [^Buf payload state m]
  (try
    (.clear payload)
    (packets/encode! state payload m)
    true
    (catch Throwable t
      (log/warn "encode failed for" (:packet m) "-" (str t))
      false)))

(defn- write-packet! [out payload body head threshold defl chunk state m]
  (when (encode-packet! payload state m)
    (c/write-frame! out payload body head (long threshold) defl chunk)))

(defn- writer-wire [^BufferedOutputStream out]
  {:out out :payload (Buf. 1024) :body (Buf. 1024) :head (Buf. 5)
   :defl (Deflater.) :chunk (byte-array 8192)})

(defn- emit! [w ^long threshold state m]
  (write-packet! (:out w) (:payload w) (:body w) (:head w) threshold (:defl w) (:chunk w) state m))

(defn- close-writer! [w ^Socket sock]
  (let [^BufferedOutputStream out (:out w)]
    (try (.flush out) (catch Throwable _ nil))
    (.end ^Deflater (:defl w))
    (.close sock)))

(defn- writer-step [^Conn c w ^long threshold x]
  (let [^ArrayBlockingQueue q (:q c)
        ^BufferedOutputStream out (:out w)]
    (cond
      (nil? x)
      (when-not (.get ^AtomicBoolean (:closing c)) threshold)
      (= :packet (nth x 0))
      (do (emit! w threshold (nth x 1) (nth x 2))
          (when (.isEmpty q) (.flush out))
          threshold)
      (= :threshold (nth x 0))
      (let [n (long (nth x 1))]
        (swap! (:st c) assoc :threshold n)
        (.flush out)
        n))))

(defn- writer-loop [^Conn c]
  (let [^Socket sock (:sock c)
        ^ArrayBlockingQueue q (:q c)
        w (writer-wire (BufferedOutputStream. (.getOutputStream sock)))]
    (try
      (loop [threshold -1]
        (when-let [t (writer-step c w threshold (.poll q writer-poll-ms TimeUnit/MILLISECONDS))]
          (recur (long t))))
      (finally
        (close-writer! w sock)))))

(defn- reader-loop [^Conn conn io]
  (let [in (BufferedInputStream. (.getInputStream ^Socket (:sock conn)))
        buf (Buf. 2048)
        infl (Inflater.)]
    (try
      (loop []
        (let [raw (c/read-frame! in buf)
              frame (c/decompress! raw (long (:threshold @(:st conn))) infl)]
          (when-let [m (packets/decode (conn-state conn) frame)]
            ((:on-packet io) conn io m)))
        (recur))
      (finally (.end infl)))))

(defn- disconnected! [^Conn conn {:keys [conns ^ConcurrentLinkedQueue queue save!]}]
  (when-let [eid (:eid (first (swap-vals! (:st conn) dissoc :eid)))]
    (swap! conns dissoc eid)
    (.offer queue [:player-quit eid])
    (log/info "player disconnected: eid" eid)
    (when save! (save!))))

(defn- start-writer! [conn]
  (Thread/startVirtualThread
    #(try (writer-loop conn)
          (catch InterruptedException _ nil)
          (catch SocketException _ nil)
          (catch Throwable t
            (log/warn "writer failed for" (who conn) "-" (str t))))))

(defn- new-conn [^Socket sock]
  (->Conn sock (ArrayBlockingQueue. out-queue-size)
          (atom {:state :handshake :threshold -1
                 :addr  (str (.getRemoteSocketAddress sock))})
          (AtomicBoolean. false)))

(defn- read-safely! [^Conn conn io ^Socket sock]
  (try
    (reader-loop conn io)
    (catch EOFException _ nil)
    (catch SocketTimeoutException _
      (log/warn "read timeout, closing" (who conn)))
    (catch SocketException _ nil)
    (catch Throwable t
      (when-not (.isClosed sock)
        (log/warn "reader failed for" (who conn) "-" (str t))))))

(defn- serve-conn! [^Socket sock io]
  (let [conn (new-conn sock)]
    (swap! (:st conn) assoc :writer (start-writer! conn))
    (try
      (read-safely! conn io sock)
      (finally
        (disconnected! conn io)
        (close! conn)))))

(defn writable-eids
  "Returns the players whose connections can take more packets."
  [conns]
  (into #{}
        (keep (fn [[eid ^Conn conn]]
                (when (< (.size ^ArrayBlockingQueue (:q conn)) out-queue-high) eid)))
        @conns))

(defn- drain! [conns ^long ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (doseq [[_ ^Conn conn] conns]
      (when-let [^Thread w (:writer @(:st conn))]
        (^[long] Thread/.join w (max 1 (- deadline (System/currentTimeMillis))))))))

(defn close-all!
  "Disconnects everyone with text and waits up to ms for it to reach them."
  [conns text ^long ms]
  (let [cs @conns]
    (doseq [[_ ^Conn conn] cs]
      (when (= :play (conn-state conn))
        (send! conn {:packet :disconnect :text text}))
      (close! conn))
    (drain! cs ms)
    (doseq [[_ ^Conn conn] cs] (.close ^Socket (:sock conn)))))

(defn- admit! [^Socket sock ^AtomicInteger live ^long limit io]
  (if (> (.incrementAndGet live) limit)
    (do (.decrementAndGet live)
        (log/warn "connection limit" limit "reached, refusing" (str (.getRemoteSocketAddress sock)))
        (.close sock))
    (Thread/startVirtualThread
      #(try (serve-conn! sock io) (finally (.decrementAndGet live))))))

(defn- accept-loop [^ServerSocket srv io ^AtomicInteger live]
  (let [limit (long (:max-connections (:cfg io) default-max-connections))]
    (loop []
      (when-not (.isClosed srv)
        (let [sock (try (.accept srv)
                        (catch Throwable t
                          (when-not (.isClosed srv)
                            (log/warn "accept failed:" (str t))
                            (Thread/sleep 100))
                          nil))]
          (when sock
            (.setTcpNoDelay ^Socket sock true)
            (.setSoTimeout ^Socket sock read-timeout-ms)
            (admit! sock live limit io))
          (recur))))))

(defn listen!
  "Starts taking player connections on port and returns the server."
  [io port]
  (let [srv (ServerSocket. (int port))
        live (AtomicInteger.)]
    {:socket srv
     :accept (Thread/startVirtualThread #(accept-loop srv io live))}))
