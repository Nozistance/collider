(ns collider.net.server
  "Player connections."
  (:require [clojure.string :as str]
            [collider.log :as log]
            [collider.net.crypt :as crypt]
            [collider.net.frame :as frame]
            [collider.net.server.conn :as conn]
            [collider.proto.buf :as buf]
            [collider.proto.packets :as packets])
  (:import (collider.proto Buf)
           (collider.net.server.conn Conn)
           (java.io BufferedInputStream BufferedOutputStream
                    EOFException OutputStream)
           (java.net ServerSocket Socket SocketException
                     SocketTimeoutException)
           (java.util.concurrent
             BlockingQueue ConcurrentLinkedQueue LinkedBlockingQueue
             TimeUnit)
           (java.util.concurrent.atomic AtomicBoolean AtomicInteger)
           (java.util.zip Deflater Inflater)))

(set! *warn-on-reflection* true)

(def ^:private ^:const out-queue-high 1024)

(def ^:private ^:const writer-poll-ms 500)

(def ^:private ^:const read-timeout-ms 30000)

(def ^:private ^:const default-max-connections 256)

(defn conn-state
  "Returns the protocol state connection c is in."
  [^Conn c]
  (:state @(:props c)))

(defn info
  "Returns everything known about connection c."
  [^Conn c]
  @(:props c))

(defn put!
  "Remembers v under k on connection c."
  [^Conn c k v]
  (swap! (:props c) assoc k v))

(defn- who [^Conn c]
  (let [{:keys [name eid addr]} @(:props c)]
    (str (or name addr) (when eid (str " (eid " eid ")")))))

(defn close!
  "Closes the connection c once everything already sent has gone out."
  [^Conn c]
  (when (.compareAndSet ^AtomicBoolean (:closing c) false true)
    (.offer ^BlockingQueue (:queue c) [:close])))

(defn closing?
  "Returns true once connection c is closing."
  [^Conn c]
  (.get ^AtomicBoolean (:closing c)))

(defn send!
  "Queues packet m for connection c, dropping it once c is closing."
  [^Conn c m]
  (when-not (closing? c)
    (.offer ^BlockingQueue (:queue c) [:packet (conn-state c) m])))

(defn compress!
  "Compresses everything above threshold on connection c from now on."
  [^Conn c ^long threshold]
  (.offer ^BlockingQueue (:queue c) [:threshold threshold]))

(defn encrypt!
  "Encrypts connection c with secret s.
  It covers what c reads next and the packets queued after this call."
  [^Conn c s]
  (swap! (:props c) update :in crypt/decrypting s)
  (.offer ^BlockingQueue (:queue c) [:encrypt s]))

(defn- encode-packet! [^Buf payload state m]
  (try
    (buf/clear! payload)
    (packets/encode! state payload m)
    true
    (catch Throwable t
      (log/warn "encode failed for" (:packet m) "-" (str t))
      false)))

(defn- writer-wire [^OutputStream raw]
  {:raw raw :out (BufferedOutputStream. raw) :threshold -1
   :payload (buf/buf 1024) :body (buf/buf 1024)
   :head (buf/buf 5) :defl (Deflater.)})

(defn- emit! [w state m]
  (when (encode-packet! (:payload w) state m)
    (frame/write-frame! (:out w) (:payload w) (:body w) (:head w)
                        (:threshold w) (:defl w))))

(defn- close-writer! [w ^Socket sock]
  (.end ^Deflater (:defl w))
  (.close sock))

(defn- encrypted [w s]
  (let [out (crypt/encrypting (:raw w) s)]
    (assoc w :out (BufferedOutputStream. out))))

(defn- writer-step [^Conn c w x]
  (let [^BlockingQueue q (:queue c)
        ^OutputStream out (:out w)
        tag (when x (nth x 0))]
    (when-not (= :packet tag) (.flush out))
    (case tag
      nil (when-not (closing? c) w)
      :packet (do (emit! w (nth x 1) (nth x 2))
                  (when (.isEmpty q) (.flush out))
                  w)
      :threshold (let [n (long (nth x 1))]
                   (swap! (:props c) assoc :threshold n)
                   (assoc w :threshold n))
      :encrypt (encrypted w (nth x 1))
      :close nil)))

(defn- writer-loop [^Conn c]
  (let [^Socket sock (:sock c)
        ^BlockingQueue q (:queue c)
        w0 (writer-wire (.getOutputStream sock))]
    (try
      (loop [w w0]
        (let [x (.poll q writer-poll-ms TimeUnit/MILLISECONDS)]
          (when-let [w' (writer-step c w x)]
            (recur w'))))
      (finally
        (close-writer! w0 sock)))))

(defn- hex-of ^String [^bytes head]
  (str/join " " (map #(format "%02x" (bit-and 255 (long %))) head)))

(defn- decode-logged [^Conn conn ^Buf frame]
  (let [state (conn-state conn)
        n (buf/readable-bytes frame)
        head (buf/peek-bytes frame 64)]
    (try (packets/decode state frame)
         (catch Throwable t
           (log/warn "bad frame from" (who conn) state n "bytes:"
                     (hex-of head))
           (throw t)))))

(defn- reader-loop [^Conn conn io]
  (let [^Socket sock (:sock conn)
        buf (buf/buf 2048)
        infl (Inflater.)]
    (put! conn :in (BufferedInputStream. (.getInputStream sock)))
    (try
      (loop []
        (let [raw (frame/read-frame! (:in @(:props conn)) buf)
              thr (long (:threshold @(:props conn)))
              frame (frame/decompress! raw thr infl)]
          (when-let [m (decode-logged conn frame)]
            ((:on-packet io) conn io m)))
        (recur))
      (finally (.end infl)))))

(defn- take-eid! [^Conn conn]
  (:eid (first (swap-vals! (:props conn) dissoc :eid))))

(defn- disconnected! [^Conn conn io]
  (let [{:keys [conns ^ConcurrentLinkedQueue queue save!]} io
        w (who conn)]
    (when-let [eid (take-eid! conn)]
      (swap! conns dissoc eid)
      (.offer queue [:player-quit eid])
      (log/info "player disconnected:" w)
      (when save! (save!)))))

(defn- start-writer! [conn]
  (Thread/startVirtualThread
    #(try (writer-loop conn)
          (catch InterruptedException _ nil)
          (catch SocketException _ nil)
          (catch Throwable t
            (log/warn "writer failed for" (who conn) "-" (str t))))))

(defn- new-props [^Socket sock]
  (atom {:state :handshake :threshold -1
         :addr  (str (.getRemoteSocketAddress sock))
         :ip    (.getHostAddress (.getInetAddress sock))}))

(defn- new-conn [^Socket sock]
  (conn/->Conn sock (LinkedBlockingQueue.) (new-props sock)
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
    (swap! (:props conn) assoc :writer (start-writer! conn))
    (try
      (read-safely! conn io sock)
      (finally
        (disconnected! conn io)
        (close! conn)))))

(defn writable-eids
  "Returns the eids whose outgoing queue still has room."
  [conns]
  (let [room? (fn [^Conn conn]
                (< (.size ^BlockingQueue (:queue conn))
                   out-queue-high))]
    (into #{}
          (keep (fn [[eid conn]] (when (room? conn) eid)))
          @conns)))

(defn- drain! [conns ^long ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (doseq [[_ ^Conn conn] conns]
      (when-let [^Thread w (:writer @(:props conn))]
        (let [left (- deadline (System/currentTimeMillis))]
          (^[long] Thread/.join w (max 1 left)))))))

(defn close-all!
  "Disconnects everyone with text and waits up to ms for it to
  reach them."
  [conns text ^long ms]
  (let [cs @conns]
    (doseq [[_ ^Conn conn] cs]
      (when (= :play (conn-state conn))
        (send! conn {:packet :disconnect :text text}))
      (close! conn))
    (drain! cs ms)
    (doseq [[_ ^Conn conn] cs] (.close ^Socket (:sock conn)))))

(defn session-id
  "Returns the session id of io.
  The id stays while anyone is connected and changes after all leave."
  [io]
  (swap! (:session io) #(or % (random-uuid))))

(defn- refuse! [^Socket sock ^long limit]
  (log/warn "connection limit" limit "reached, refusing"
            (str (.getRemoteSocketAddress sock)))
  (.close sock))

(defn- admit! [^Socket sock ^AtomicInteger live ^long limit io]
  (if (> (.incrementAndGet live) limit)
    (do (.decrementAndGet live)
        (refuse! sock limit))
    (Thread/startVirtualThread
      #(try (serve-conn! sock io)
            (finally
              (when (zero? (.decrementAndGet live))
                (reset! (:session io) nil)))))))

(defn- accept-one [^ServerSocket srv]
  (try (.accept srv)
       (catch Throwable t
         (when-not (.isClosed srv)
           (log/warn "accept failed:" (str t))
           (Thread/sleep 100))
         nil)))

(defn- connection-limit ^long [io]
  (let [s (some-> (:settings io) deref)]
    (long (:max-connections s default-max-connections))))

(defn- accept-loop [^ServerSocket srv io ^AtomicInteger live]
  (loop []
    (when-not (.isClosed srv)
      (when-let [sock (accept-one srv)]
        (.setTcpNoDelay ^Socket sock true)
        (.setSoTimeout ^Socket sock read-timeout-ms)
        (admit! sock live (connection-limit io) io))
      (recur))))

(defn listen!
  "Opens the port and starts accepting connections on it."
  [io port]
  (let [srv (ServerSocket. (int port))
        live (AtomicInteger.)
        io (assoc io :session (atom nil))]
    {:socket srv
     :accept (Thread/startVirtualThread #(accept-loop srv io live))}))
