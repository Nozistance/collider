(ns collider.net.server
  (:require [collider.log :as log]
            [collider.proto.codec :as c]
            [collider.proto.packets :as packets])
  (:import (collider.java Buf)
           (java.io BufferedInputStream BufferedOutputStream EOFException)
           (java.net ServerSocket Socket SocketException)
           (java.util.concurrent ArrayBlockingQueue ConcurrentLinkedQueue TimeUnit)
           (java.util.concurrent.atomic AtomicBoolean)
           (java.util.zip Deflater Inflater)))

(set! *warn-on-reflection* true)

(def ^:private ^:const out-queue-size 4096)
(def ^:private ^:const out-queue-high 1024)
(def ^:private ^:const writer-poll-ms 500)
(defrecord Conn [^Socket sock ^ArrayBlockingQueue q st ^AtomicBoolean closing])

(defn conn-state [^Conn c] (:state @(:st c)))
(defn info [^Conn c] @(:st c))
(defn put! [^Conn c k v] (swap! (:st c) assoc k v))
(defn- who [^Conn c]
  (let [{:keys [name eid addr]} @(:st c)]
    (str (or name addr) (when eid (str " (eid " eid ")")))))

(defn set-conn-state! [^Conn c s] (swap! (:st c) assoc :state s))
(defn close! [^Conn c]
  (.set ^AtomicBoolean (:closing c) true)
  (.offer ^ArrayBlockingQueue (:q c) [:close]))

(defn send! [^Conn c m]
  (when-not (.offer ^ArrayBlockingQueue (:q c) [:packet (conn-state c) m])
    (log/info "output queue full, closing" (who c))
    (.set ^AtomicBoolean (:closing c) true)
    (.close ^Socket (:sock c))))

(defn compress! [^Conn c ^long threshold]
  (.offer ^ArrayBlockingQueue (:q c) [:threshold threshold]))

(defn- writer-loop [^Conn c]
  (let [^Socket sock (:sock c)
        ^ArrayBlockingQueue q (:q c)
        ^AtomicBoolean closing (:closing c)
        out (BufferedOutputStream. (.getOutputStream sock))
        payload (Buf. 1024)
        body (Buf. 1024)
        head (Buf. 5)
        defl (Deflater.)
        chunk (byte-array 8192)]
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

(defn- serve-conn! [^Socket sock io]
  (let [conn (->Conn sock (ArrayBlockingQueue. out-queue-size)
                     (atom {:state :handshake :threshold -1
                            :addr  (str (.getRemoteSocketAddress sock))})
                     (AtomicBoolean. false))]
    (swap! (:st conn) assoc :writer
           (Thread/startVirtualThread
             #(try (writer-loop conn)
                   (catch InterruptedException _ nil)
                   (catch SocketException _ nil)
                   (catch Throwable t
                     (log/info "writer failed for" (who conn) "-" (str t))))))
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

(defn writable-eids [conns]
  (into #{}
        (keep (fn [[eid ^Conn conn]]
                (when (< (.size ^ArrayBlockingQueue (:q conn)) out-queue-high) eid)))
        @conns))

(defn- drain! [conns ^long ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (doseq [[_ ^Conn conn] conns]
      (when-let [^Thread w (:writer @(:st conn))]
        (^[long] Thread/.join w (max 1 (- deadline (System/currentTimeMillis))))))))

(defn close-all! [conns text ^long ms]
  (let [cs @conns]
    (doseq [[_ ^Conn conn] cs]
      (when (= :play (conn-state conn))
        (send! conn {:packet :disconnect :text text}))
      (close! conn))
    (drain! cs ms)
    (doseq [[_ ^Conn conn] cs] (.close ^Socket (:sock conn)))))

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

(defn listen! [io port]
  (let [srv (ServerSocket. (int port))]
    {:socket srv
     :accept (Thread/startVirtualThread #(accept-loop srv io))}))
