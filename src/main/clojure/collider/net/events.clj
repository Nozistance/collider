(ns collider.net.events
  "Game events of the play packets of a player."
  (:require [collider.game.mode :as game-mode]))

(set! *warn-on-reflection* true)

(defn client-settings
  "Returns the settings of a player that client information m holds."
  [m]
  {:view-distance (:view-distance m) :skin-parts (:skin-parts m)})

(defn- on-ground? [m] (bit-test (long (:flags m)) 0))

(defn- invalid-move? [m]
  (let [bad? #(not (Double/isFinite (double %)))]
    (or (some bad? (:pos m))
        (some bad? (keep m [:yaw :pitch])))))

(def ^:private move-packets
  #{:move-player-pos :move-player-pos-rot :move-player-rot})

(defn bad-move?
  "Returns true when move packet m holds a number that is not finite."
  [m]
  (boolean (and (move-packets (:packet m)) (invalid-move? m))))

(defn- turn [m]
  {:yaw (:yaw m) :pitch (:pitch m) :on-ground (on-ground? m)})

(def ^:private move-events
  {:move-player-pos
   (fn [eid m]
     [:move eid {:pos (:pos m) :on-ground (on-ground? m)}])
   :move-player-pos-rot
   (fn [eid m] [:move eid (assoc (turn m) :pos (:pos m))])
   :move-player-rot (fn [eid m] [:move eid (turn m)])
   :move-player-status-only
   (fn [eid m] [:move eid {:on-ground (on-ground? m)}])
   :accept-teleportation
   (fn [eid m] [:teleport-ack eid (:id m)])})

(def ^:private state-events
  {:player-abilities
   (fn [eid m]
     [:abilities eid {:flying (bit-test (long (:flags m)) 1)}])
   :player-loaded (fn [eid _] [:player-loaded eid])
   :player-input
   (fn [eid m]
     [:input eid {:sneaking? (bit-test (long (:flags m)) 5)}])})

(defn- hand-of [m]
  (if (zero? (long (or (:hand m) 0))) :main :off))

(defn- cursor-pixels [[cx cy cz]]
  [(* 16.0 (double cx)) (* 16.0 (double cy)) (* 16.0 (double cz))])

(defn- place-on-block [eid m]
  [:place eid (:pos m) (:face m) nil (cursor-pixels (:cursor m))
   (:sequence m) nil (hand-of m)])

(def ^:private ^:const release-use-item 5)

(defn- dig-event [eid m]
  (if (= release-use-item (long (:action m)))
    [:release-use eid]
    [:dig eid (:action m) (:pos m) (:face m) (:sequence m)]))

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
   (fn [eid m] [:menu-button eid (:container m) (:button m)])
   :bundle-item-selected
   (fn [eid m] [:bundle-select eid (:slot m) (:selected m)])})

(defn- client-command-event [eid m]
  (case (long (:action m))
    0 [:respawn eid]
    1 [:stats-request eid]
    2 [:rules-request eid]
    nil))

(defn- command-event [eid m]
  [:chat eid (str "/" (:command m))])

(defn- spectate-event [eid m]
  (let [n (long (:target m))]
    [:spectate eid (when (pos? n) (dec n))]))

(def ^:private session-events
  {:keep-alive (fn [eid m] [:keepalive-echo eid (:id m)])
   :client-tick-end (fn [eid _] [:client-tick-end eid])
   :client-information
   (fn [eid m] [:client-settings eid (client-settings m)])
   :chunk-batch-received (fn [eid m] [:chunk-batch-ack eid (:rate m)])
   :client-command client-command-event
   :set-game-rule (fn [eid m] [:set-rules eid (:entries m)])
   :command-suggestion
   (fn [eid m] [:tab-complete eid (:text m) nil (:id m)])
   :chat (fn [eid m] [:chat eid (:message m)])
   :chat-command command-event
   :chat-command-signed command-event
   :sign-update
   (fn [eid m] [:sign-update eid (:pos m) (:front? m) (:lines m)])
   :rename-item (fn [eid m] [:rename-item eid (:name m)])
   :edit-book
   (fn [eid m]
     [:edit-book eid (:slot m) (select-keys m [:pages :title])])
   :change-game-mode
   (fn [eid m] [:change-game-mode eid (game-mode/of-id (:mode m))])
   :spectator-action spectate-event
   :teleport-to-entity
   (fn [eid m] [:teleport-to-entity eid (:uuid m)])})

(def ^:private event-table
  (merge session-events move-events state-events action-events
         container-events))

(defn packet->event
  "Returns the event of play packet m from player eid, or nil when the
  packet makes none."
  [eid {:keys [packet] :as m}]
  (when-let [f (get event-table packet)]
    (f eid m)))

(def ^:private ignored
  #{:custom-payload :chat-session-update :chat-ack
    :configuration-acknowledged
    :cookie-response :custom-click-action :debug-subscription-request
    :pong :block-entity-tag-query
    :entity-tag-query :jigsaw-generate :lock-difficulty
    :change-difficulty :move-vehicle :paddle-boat :place-recipe
    :recipe-book-change-settings :recipe-book-seen-recipe
    :resource-pack :seen-advancements :select-trade :set-beacon
    :set-command-block :set-command-minecart :set-jigsaw-block
    :set-structure-block :set-test-block
    :test-instance-block-action})

(def ^:private later
  #{:container-slot-state-changed})

(defn ignored?
  "Returns true for a play packet that makes no event on purpose."
  [packet]
  (boolean (or (ignored packet) (later packet))))
