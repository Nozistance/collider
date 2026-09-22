(ns collider.net.session
  "The login of a player, from handshake to play."
  (:require [clojure.data.json :as json]
            [collider.data :as data]
            [collider.log :as log]
            [collider.proto.codec :as c]
            [collider.net.server :as server])
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

(def ^:private transfers-reason
  {:translate "multiplayer.disconnect.transfers_disabled"})

(def ^:private bad-movement-reason
  {:translate "multiplayer.disconnect.invalid_player_movement"})

(defn- disconnect-packet [text]
  {:packet :disconnect :text text})

(defn- login-kick-packet [reason]
  {:packet :login-disconnect :json (json/write-str reason)})

(defn- status-body [{:keys [conns cfg]}]
  {:version     {:name c/game-version :protocol c/protocol-version}
   :players     {:max (:max-players cfg) :online (count @conns)}
   :description {:text (:motd cfg)}})

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

(defn- client-settings
  "Returns the options a player entity keeps of the packet."
  [m]
  {:view-distance (:view-distance m) :skin-parts (:skin-parts m)})

(defn- do-login! [conn {:keys [conns ^ConcurrentLinkedQueue queue]}]
  (let [{nm :name settings :settings} (server/info conn)
        eid (.incrementAndGet next-entity-id)]
    (server/put! conn :eid eid)
    (swap! conns assoc eid conn)
    (server/set-conn-state! conn :play)
    (.offer queue [:player-join eid nm settings])
    (log/info "player" nm "connected: eid" eid
              "addr" (:addr (server/info conn)))))

(defn- on-ground? [m] (odd? (long (:flags m))))

(defn- invalid-move? [m]
  (let [bad? #(not (Double/isFinite (double %)))]
    (or (some bad? (:pos m))
        (some bad? (keep m [:yaw :pitch])))))

(def ^:private move-events
  {:move-player-pos
   (fn [eid m]
     [:move eid {:pos (:pos m) :on-ground (on-ground? m)}])
   :move-player-pos-rot
   (fn [eid m]
     [:move eid {:pos (:pos m) :yaw (:yaw m) :pitch (:pitch m)
                 :on-ground (on-ground? m)}])
   :move-player-rot
   (fn [eid m]
     [:move eid {:yaw (:yaw m) :pitch (:pitch m)
                 :on-ground (on-ground? m)}])
   :move-player-status-only
   (fn [eid m] [:move eid {:on-ground (on-ground? m)}])
   :accept-teleportation
   (fn [eid m] [:teleport-ack eid (:id m)])
   :player-abilities
   (fn [eid m] [:move eid {:flying (bit-test (long (:flags m)) 1)}])
   :player-input
   (fn [eid m]
     [:input eid {:sneaking? (bit-test (long (:flags m)) 5)}])})

(defn- place-on-block [eid m]
  (let [[cx cy cz] (:cursor m)]
    [:place eid (:pos m) (:face m) nil
     [(* 16.0 (double cx)) (* 16.0 (double cy)) (* 16.0 (double cz))]
     (:sequence m)]))

(def ^:private ^:const release-use-item 5)

(defn- dig-event [eid m]
  (if (= release-use-item (long (:action m)))
    [:release-use eid]
    [:dig eid (:action m) (:pos m) (:face m) (:sequence m)]))

(defn- hand-of [m]
  (if (zero? (long (or (:hand m) 0))) :main :off))

(def ^:private action-events
  {:player-action dig-event
   :use-item-on place-on-block
   :use-item
   (fn [eid m]
     [:use-item eid (hand-of m) (:sequence m)
      {:yaw (:yaw m) :pitch (:pitch m)}])
   :swing (fn [eid m] [:swing eid (hand-of m)])
   :player-command (fn [eid m] [:entity-action eid (:action m)])
   :interact
   (fn [eid m]
     [:interact eid (:target m) (hand-of m) (boolean (:sneaking m))])
   :attack (fn [eid m] [:attack eid (:target m)])
   :pick-item-from-block
   (fn [eid m]
     [:pick eid {:pos (:pos m) :include-data (:include-data m)}])
   :pick-item-from-entity (fn [eid m] [:pick eid {:entity (:id m)}])})

(defn- click-event [eid m]
  (if (zero? (long (:container m)))
    [:click eid (dissoc m :packet :container)]
    [:menu-click eid (dissoc m :packet)]))

(def ^:private container-events
  {:set-carried-item (fn [eid m] [:held-item eid (:slot m)])
   :set-creative-mode-slot
   (fn [eid m] [:creative-slot eid (:slot m) (:stack m)])
   :container-click click-event
   :container-close (fn [eid m] [:menu-close eid (:container m)])
   :container-button-click
   (fn [eid m] [:menu-button eid (:container m) (:button m)])})

(defn- client-command-event [eid m]
  (case (long (:action m))
    0 [:respawn eid]
    1 [:stats-request eid]
    2 [:rules-request eid]
    nil))

(def ^:private session-events
  {:keep-alive (fn [eid m] [:keepalive-echo eid (:id m)])
   :client-information
   (fn [eid m] [:client-settings eid (client-settings m)])
   :chunk-batch-received (fn [eid m] [:chunk-batch-ack eid (:rate m)])
   :client-command client-command-event
   :set-game-rule (fn [eid m] [:set-rules eid (:entries m)])
   :command-suggestion
   (fn [eid m] [:tab-complete eid (:text m) nil (:id m)])
   :chat (fn [eid m] [:chat eid (:message m)])
   :chat-command (fn [eid m] [:chat eid (str "/" (:command m))])
   :chat-command-signed
   (fn [eid m] [:chat eid (str "/" (:command m))])
   :sign-update
   (fn [eid m] [:sign-update eid (:pos m) (:front? m) (:lines m)])
   :rename-item (fn [eid m] [:rename-item eid (:name m)])})

(def ^:private event-table
  (merge session-events move-events action-events container-events))

(defn- packet->event [eid {:keys [packet] :as m}]
  (when-let [f (get event-table packet)]
    (f eid m)))

(def ^:private ignored
  #{:player-loaded :client-tick-end
    :custom-payload :chat-session-update :chat-ack
    :configuration-acknowledged
    :cookie-response :custom-click-action :debug-subscription-request
    :pong :bundle-item-selected :block-entity-tag-query
    :entity-tag-query :edit-book :jigsaw-generate :lock-difficulty
    :change-difficulty :move-vehicle :paddle-boat :place-recipe
    :recipe-book-change-settings :recipe-book-seen-recipe
    :resource-pack :seen-advancements :select-trade :set-beacon
    :set-command-block :set-command-minecart :set-jigsaw-block
    :set-structure-block :set-test-block :spectator-action
    :teleport-to-entity :test-instance-block-action})

(def ^:private later
  #{:container-slot-state-changed :change-game-mode})

(def ^:private unhandled (atom #{}))

(defn- log-unhandled! [packet]
  (when-not (or (ignored packet) (later packet) (@unhandled packet))
    (swap! unhandled conj packet)
    (log/info "play:" packet "not handled")))

(defn- kick-login! [conn reason]
  (server/send! conn (login-kick-packet reason))
  (server/close! conn))

(defn- version-reason [k]
  {:translate k :with [c/game-version]})

(defn- outdated-key [^long protocol]
  (if (< protocol 754)
    "multiplayer.disconnect.outdated_client"
    "multiplayer.disconnect.incompatible"))

(defn- begin-login! [conn ^long protocol]
  (server/set-conn-state! conn :login)
  (when-not (= protocol c/protocol-version)
    (kick-login! conn (version-reason (outdated-key protocol)))))

(defn- valid-name? [nm]
  (and (string? nm)
       (<= (count nm) 16)
       (every? #(< 32 (long (int %)) 127) nm)))

(defn- kick-duplicates! [conns nm]
  (doseq [[_ c] @conns :when (= nm (:name (server/info c)))]
    (server/send! c (disconnect-packet duplicate-reason))
    (server/close! c)))

(defn- setup-compression! [conn ^long threshold]
  (when-not (neg? threshold)
    (server/send! conn {:packet :login-compression
                        :threshold threshold})
    (server/compress! conn threshold)))

(defn- intention! [conn m]
  (case (long (:next m))
    1 (server/set-conn-state! conn :status)
    2 (begin-login! conn (long (:protocol m)))
    3 (do (server/set-conn-state! conn :login)
          (kick-login! conn transfers-reason))
    (server/close! conn)))

(defn- login-ok! [conn conns cfg nm]
  (server/put! conn :name nm)
  (setup-compression! conn (long (:compression-threshold cfg -1)))
  (kick-duplicates! conns nm)
  (server/send! conn {:packet :login-finished :name nm
                      :uuid (c/offline-uuid nm)}))

(defn- hello! [conn io cfg m]
  (let [nm (:name m)
        conns (:conns io)]
    (cond
      (not (valid-name? nm)) (kick-login! conn generic-reason)
      (>= (count @conns) (long (:max-players cfg)))
      (kick-login! conn server-full-reason)
      :else (login-ok! conn conns cfg nm))))

(def ^:private move-packets
  #{:move-player-pos :move-player-pos-rot :move-player-rot})

(defn- kick-bad-move! [conn]
  (server/send! conn (disconnect-packet bad-movement-reason))
  (server/close! conn))

(defn- play-packet! [conn ^ConcurrentLinkedQueue queue m]
  (when (= :play (server/conn-state conn))
    (when-let [eid (:eid (server/info conn))]
      (if (and (move-packets (:packet m)) (invalid-move? m))
        (kick-bad-move! conn)
        (if-let [ev (packet->event eid m)]
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

(defn- dispatch!
  [conn {:keys [^ConcurrentLinkedQueue queue cfg] :as io} m]
  (case [(server/conn-state conn) (:packet m)]
    [:handshake :intention] (intention! conn m)
    [:status :status-request] (server/send! conn (status-response io))
    [:status :ping-request]
    (do (server/send! conn (pong m))
        (server/close! conn))
    [:play :ping-request] (server/send! conn (pong m))
    [:login :hello] (hello! conn io cfg m)
    [:login :login-acknowledged]
    (do (server/set-conn-state! conn :configuration)
        (start-configuration! conn))
    [:configuration :client-information]
    (server/put! conn :settings (client-settings m))
    [:configuration :select-known-packs] (finish-configuration! conn)
    [:configuration :finish-configuration] (do-login! conn io)
    (play-packet! conn queue m)))

(defn handle-packet
  "Acts on packet m from conn, disconnecting it on any error."
  [conn io m]
  (try
    (dispatch! conn io m)
    (catch Throwable t
      (disconnect-generic! conn t))))
