(ns collider.net.session
  "The login of a player, from handshake to play, and the play
  packets it passes on as events."
  (:require [clojure.data.json :as json]
            [collider.data :as data]
            [collider.game.player :as player]
            [collider.log :as log]
            [collider.net.crypt :as crypt]
            [collider.net.events :as events]
            [collider.net.server :as server]
            [collider.proto.codec :as c])
  (:import (java.util.concurrent ConcurrentLinkedQueue)
           (java.util.concurrent.atomic AtomicInteger)))

(set! *warn-on-reflection* true)

(defonce ^AtomicInteger next-entity-id (AtomicInteger.))

(def ^:private generic-reason
  {:translate "multiplayer.disconnect.generic"})

(def ^:private server-full-reason
  {:translate "multiplayer.disconnect.server_full"})

(def ^:private duplicate-reason
  {:translate "multiplayer.disconnect.duplicate_login"})

(def ^:private slow-login-reason
  {:translate "multiplayer.disconnect.slow_login"})

(def ^:private transfers-reason
  {:translate "multiplayer.disconnect.transfers_disabled"})

(def ^:private bad-movement-reason
  {:translate "multiplayer.disconnect.invalid_player_movement"})

(defn- disconnect-packet [text]
  {:packet :disconnect :text text})

(defn- login-kick-packet [reason]
  {:packet :login-disconnect :json (json/write-str reason)})

(defn- status-body [{:keys [conns settings]}]
  (let [s @settings]
    {:version     {:name c/game-version :protocol c/protocol-version}
     :players     {:max (:max-players s) :online (count @conns)}
     :description {:text (:motd s)}}))

(def ^:private known-pack ["minecraft" "core" c/game-version])

(defn- start-configuration! [conn]
  (server/send! conn {:packet :custom-payload :channel :brand
                      :value "collider"})
  (server/send! conn {:packet :update-enabled-features
                      :features [:vanilla]})
  (server/send! conn {:packet :select-known-packs
                      :packs [known-pack]}))

(defn- finish-configuration! [conn]
  (doseq [[registry names] (data/datapack)]
    (server/send! conn {:packet :registry-data :registry registry
                        :names names}))
  (server/send! conn {:packet :update-tags :tags (data/tags)})
  (server/send! conn {:packet :finish-configuration}))

(defn- of-profile? [uuid c]
  (= uuid (:uuid (:profile (server/info c)))))

(defn- full? [conns cfg]
  (>= (count conns) (long (:max-players cfg))))

(defn- join-refusal [conns cfg uuid]
  (cond
    (some #(of-profile? uuid %) (vals conns)) duplicate-reason
    (full? conns cfg) server-full-reason))

(defn- admit! [conns cfg uuid eid conn]
  (let [admit #(cond-> % (nil? (join-refusal % cfg uuid))
                 (assoc eid conn))]
    (join-refusal (first (swap-vals! conns admit)) cfg uuid)))

(defn- kick! [conn reason]
  (server/send! conn (disconnect-packet reason))
  (server/close! conn))

(defn- joined! [conn ^ConcurrentLinkedQueue queue eid]
  (let [{nm :name :keys [settings profile addr]} (server/info conn)]
    (server/put! conn :eid eid)
    (server/put! conn :state :play)
    (.offer queue [:player-join eid nm (merge settings profile)])
    (log/info "player" nm "connected: eid" eid "addr" addr)))

(defn- do-login! [conn {:keys [conns queue]} cfg]
  (let [uuid (:uuid (:profile (server/info conn)))
        eid (.incrementAndGet next-entity-id)]
    (if-let [reason (admit! conns cfg uuid eid conn)]
      (kick! conn reason)
      (joined! conn queue eid))))

(defn- log-unhandled! [packet]
  (when-not (events/ignored? packet)
    (log/once! [::unhandled packet]
               log/info "play:" packet "not handled")))

(defn- kick-login! [conn reason]
  (server/send! conn (login-kick-packet reason))
  (server/close! conn))

(defn- version-reason [k]
  {:translate k :with [c/game-version]})

(defn- outdated-key [^long protocol]
  (if (< protocol 754)
    "multiplayer.disconnect.outdated_client"
    "multiplayer.disconnect.incompatible"))

(def ^:private ^:const tick-ms 50)

(def ^:private ^:const max-login-ticks 600)

(def ^:private ^:const slow-login-ms (* max-login-ticks tick-ms))

(defn- watch-login! [conn ^long ms]
  (Thread/startVirtualThread
    #(do (Thread/sleep ms)
         (when (= :login (server/conn-state conn))
           (kick-login! conn slow-login-reason)))))

(defn- begin-login! [conn io ^long protocol]
  (server/put! conn :state :login)
  (if (= protocol c/protocol-version)
    (watch-login! conn (:slow-login-ms io slow-login-ms))
    (kick-login! conn (version-reason (outdated-key protocol)))))

(defn- valid-name? [nm]
  (and (string? nm)
       (<= (count nm) 16)
       (every? #(< 32 (long (int %)) 127) nm)))

(defn- kick-duplicates! [conns uuid]
  (let [dupes (filterv #(of-profile? uuid %) (vals @conns))]
    (run! #(kick! % duplicate-reason) dupes)
    (seq dupes)))

(defn- await-quit! [conn conns uuid]
  (when (and (not (server/closing? conn))
             (some #(of-profile? uuid %) (vals @conns)))
    (Thread/sleep tick-ms)
    (recur conn conns uuid)))

(defn- setup-compression! [conn ^long threshold]
  (when-not (neg? threshold)
    (server/send! conn {:packet :login-compression
                        :threshold threshold})
    (server/compress! conn threshold)))

(defn- intention! [conn io m]
  (case (long (:next m))
    1 (server/put! conn :state :status)
    2 (begin-login! conn io (long (:protocol m)))
    3 (do (server/put! conn :state :login)
          (kick-login! conn transfers-reason))
    (server/close! conn)))

(defn- refuse! [conn nm text]
  (log/info "refused" nm "addr" (:addr (server/info conn)) "-" text)
  (kick-login! conn text))

(defn- login-ok! [conn io cfg {nm :name :keys [uuid properties]}]
  (let [conns (:conns io)]
    (server/put! conn :name nm)
    (server/put! conn :profile {:uuid uuid :properties properties})
    (setup-compression! conn (long (:compression-threshold cfg -1)))
    (when (kick-duplicates! conns uuid)
      (await-quit! conn conns uuid))
    (server/send! conn {:packet :login-finished :name nm :uuid uuid
                        :properties properties
                        :session (server/session-id io)})))

(defn- verified! [conn io cfg who]
  (if (full? @(:conns io) cfg)
    (refuse! conn (:name who) server-full-reason)
    (login-ok! conn io cfg who)))

(defn- offline [{nm :name}]
  {:uuid (player/offline-uuid nm) :name nm :properties []})

(defn- provider [io]
  (or (:identity io) {:identify offline}))

(defn- identified! [conn io cfg who]
  (let [ip (:ip (server/info conn))
        r ((:identify (provider io)) (assoc who :ip ip))]
    (cond
      (server/closing? conn) nil
      (:refuse r) (refuse! conn (:name who) (:refuse r))
      :else (verified! conn io cfg r))))

(defn- ask-key! [conn io nm]
  (let [token (crypt/challenge)
        auth? (boolean (:encrypt? (provider io)))]
    (server/put! conn :login {:name nm :token token})
    (server/send! conn {:packet :hello :server-id ""
                        :key (vec (crypt/public-key (:key-pair io)))
                        :token (vec token) :authenticate? auth?})))

(defn- encrypt? [io cfg]
  (or (:encrypt? (provider io)) (:encryption cfg)))

(defn- hello! [conn io cfg m]
  (let [nm (:name m)]
    (cond
      (not (valid-name? nm)) (kick-login! conn generic-reason)
      (encrypt? io cfg) (ask-key! conn io nm)
      :else (identified! conn io cfg {:name nm}))))

(defn- pending-login [conn]
  (or (:login (server/info conn))
      (throw (ex-info "unexpected key packet" {}))))

(defn- key! [conn io cfg m]
  (let [{nm :name token :token} (pending-login conn)
        kp (:key-pair io)
        s (crypt/secret kp (byte-array (:secret m))
                        (byte-array (:token m)) token)]
    (server/put! conn :login nil)
    (server/encrypt! conn s)
    (identified! conn io cfg
                 {:name nm
                  :server-hash (crypt/server-hash "" kp s)})))

(defn- play-packet! [conn ^ConcurrentLinkedQueue queue m]
  (when (= :play (server/conn-state conn))
    (when-let [eid (:eid (server/info conn))]
      (if (events/bad-move? m)
        (kick! conn bad-movement-reason)
        (if-let [ev (events/packet->event eid m)]
          (.offer queue ev)
          (log-unhandled! (:packet m)))))))

(defn- disconnect-generic! [conn t]
  (log/warn "bad packet from" (:addr (server/info conn)) "-" (str t))
  (case (server/conn-state conn)
    :play (server/send! conn (disconnect-packet generic-reason))
    :login (server/send! conn (login-kick-packet generic-reason))
    nil)
  (server/close! conn))

(defn- status-response [io]
  {:packet :status-response :json (json/write-str (status-body io))})

(defn- pong [m]
  {:packet :pong-response :payload (:payload m)})

(defn- last-pong! [conn m]
  (server/send! conn (pong m))
  (server/close! conn))

(defn- login-acknowledged! [conn]
  (server/put! conn :state :configuration)
  (start-configuration! conn))

(defn- client-information! [conn m]
  (server/put! conn :settings (events/client-settings m)))

(defn- dispatch!
  [conn {:keys [^ConcurrentLinkedQueue queue settings] :as io} m]
  (case [(server/conn-state conn) (:packet m)]
    [:handshake :intention] (intention! conn io m)
    [:status :status-request] (server/send! conn (status-response io))
    [:status :ping-request] (last-pong! conn m)
    [:play :ping-request] (server/send! conn (pong m))
    [:login :hello] (hello! conn io @settings m)
    [:login :key] (key! conn io @settings m)
    [:login :login-acknowledged] (login-acknowledged! conn)
    [:configuration :client-information] (client-information! conn m)
    [:configuration :select-known-packs] (finish-configuration! conn)
    [:configuration :finish-configuration]
    (do-login! conn io @settings)
    (play-packet! conn queue m)))

(defn handle-packet
  "Acts on packet m from conn, disconnecting it on any error."
  [conn io m]
  (try
    (dispatch! conn io m)
    (catch Throwable t
      (disconnect-generic! conn t))))
