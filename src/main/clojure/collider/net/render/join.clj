(ns collider.net.render.join
  "Packets of a player joining the server or entering a level."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.block.menu :as menu]
            [collider.game.clock :as clock]
            [collider.game.command.forms :as forms]
            [collider.game.command.nodes :as commands]
            [collider.game.effect :as effect]
            [collider.game.entity :as entity]
            [collider.game.food :as food]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.schema :as schema]
            [collider.net.render.view :as view]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.env.biome :as biome]))

(set! *warn-on-reflection* true)

(defn- spawn-info [lv e]
  (let [dim (:dim lv :overworld)]
    {:dimension-type (data/datapack-id "dimension_type" dim)
     :dimension dim :sea-level (biome/sea-level-of dim)
     :gamemode (game-mode/id (:game-mode e))
     :last-gamemode (game-mode/id (:previous-game-mode e))}))

(def ^:private difficulty-packet
  {:packet :change-difficulty :difficulty 0 :locked false})

(defn- abilities-flags ^long [m]
  (bit-or (if (:invulnerable? m) 1 0) (if (:flying? m) 2 0)
          (if (:may-fly? m) 4 0) (if (:instabuild? m) 8 0)))

(defn abilities-packet
  "Returns the packet of the abilities m names."
  [m]
  {:packet       :player-abilities :flags (abilities-flags m)
   :flying-speed 0.05 :walking-speed 0.1})

(defn- own-abilities [e]
  (abilities-packet (game-mode/abilities e)))

(def ^:private ^:table command-tree (delay (commands/tree)))

(def ^:private ^:table open-command-tree (delay (commands/tree 0)))

(def ^:private plugin-tree (memoize commands/tree))

(defn- command-nodes [lv ^long level]
  (let [extra (forms/extra-of lv)]
    (cond (seq extra) (plugin-tree level extra)
          (< level (long forms/gamemaster)) @open-command-tree
          :else @command-tree)))

(def ^:private op-level-event 24)

(def ^:private game-events
  {:start-raining 1 :stop-raining 2 :change-game-mode 3
   :rain-level-change 7 :thunder-level-change 8
   :immediate-respawn 11 :limited-crafting 12
   :level-chunks-load-start 13})

(defn game-event-packet
  [event value]
  {:packet :game-event :event (game-events event) :value value})

(defn- join-spawn [world]
  (v/cell (player/respawn-at world)))

(defn- permission-packets [lv eid e]
  (let [level (player/permission-level e)]
    [{:packet :entity-event :eid eid :event (+ op-level-event level)}
     {:packet :commands :nodes (command-nodes lv level)}]))

(def ^:private border-size (* 2.0 chunk/world-border))

(def ^:private border-packet
  {:packet     :initialize-border :center-x 0.0 :center-z 0.0
   :old-size   border-size :size border-size
   :max-size   chunk/world-border :warning-blocks 5
   :warning-time 300})

(defn- spawn-pos-packet [world]
  (let [[yaw pitch] (player/spawn-turn world)]
    {:packet :set-default-spawn-position
     :dimension (:world-spawn-dimension world :overworld)
     :pos (join-spawn world) :yaw yaw :pitch pitch}))

(def ^:private ticking-packets
  [{:packet :ticking-state :rate 20.0 :frozen? false}
   {:packet :ticking-step :steps 0}])

(defn set-time-packet
  [m]
  {:packet :set-time :age (:age m) :clocks (:clocks m)})

(defn- clock-sync-packet [lv]
  (set-time-packet {:age (:tick lv) :clocks (clock/full-sync lv)}))

(defn- rain-packets [lv]
  (let [rain (double (:rain-level lv 0.0))
        thunder (* rain (double (:thunder-level lv 0.0)))]
    (when (> rain 0.2)
      [(game-event-packet :start-raining 0.0)
       (game-event-packet :rain-level-change rain)
       (game-event-packet :thunder-level-change thunder)])))

(defn- level-info-packets [lv]
  (concat [border-packet (clock-sync-packet lv) (spawn-pos-packet lv)]
          (rain-packets lv)
          [(game-event-packet :level-chunks-load-start 0.0)]
          ticking-packets))

(defn- leave-packets [m]
  (concat (map view/forget-chunk-packet (:forget m))
          (map (fn [id] {:packet :remove-entities :eids [id]})
               (:untrack m))))

(defn- player-info-packets [e]
  (let [inv (or (:inventory e) {})]
    [{:packet :container-set-content :container 0 :state-id 0
      :items (mapv inv (range menu/slot-count)) :carried (:carried e)}
     {:packet :set-held-slot :slot (long (or (:held-slot e) 0))}]))

(defn- experience-packet [e]
  {:packet :set-experience :progress (double (:xp-progress e 0.0))
   :level (long (:xp-level e 0)) :total (long (:xp-total e 0))})

(defn health-packet
  "Returns the packet that shows a player its health and food."
  [health food saturation]
  {:packet :set-health :health health :food food
   :saturation saturation})

(defn- own-health-packet [e]
  (health-packet (double (:health e 20.0)) (food/level e)
                 (food/saturation e)))

(defn resent-packets
  "Returns the health and experience a player gets again after
  entering a level."
  [e]
  [(own-health-packet e)
   (experience-packet e)])

(defn position-packet
  "Returns the packet that moves a player to the place m names."
  [m]
  {:packet :player-position :teleport-id 0
   :pos (:pos m) :vel [0.0 0.0 0.0] :yaw (:yaw m)
   :pitch (:pitch m) :relative (:relative m 0)})

(defn- arrival-packets [m e]
  [(position-packet m)
   (view/center-packet (chunk/pos-chunk (:pos m)))
   (own-abilities e)])

(defn- effect-packets [eid e]
  (for [[k i] (effect/in-order (:effects e))]
    (-> (out/mob-effect eid k i false)
        (dissoc :msg)
        (assoc :packet :update-mob-effect))))

(defn change-dimension-packets
  "Returns the packets of a player moving into level lv."
  [lv m]
  (let [eid (:to m) e (get-in lv [:entities eid])]
    (concat
      [(assoc (spawn-info lv e) :packet :respawn :keep 3)
       difficulty-packet]
      (permission-packets lv eid e)
      (leave-packets m)
      (arrival-packets m e)
      (level-info-packets lv)
      (player-info-packets e)
      (effect-packets eid e))))

(defn respawn-packets
  "Returns the packets of a player respawning in level lv."
  [lv m]
  (let [e (get-in lv [:entities (:to m)])]
    [(assoc (spawn-info lv e) :packet :respawn :keep 0)
     (clock-sync-packet lv)
     (game-event-packet :level-chunks-load-start 0.0)]))

(defn- rule-flags [lv]
  (let [on? #(boolean (get-in lv [:rules %]))]
    {:reduced-debug    (on? :reduced-debug-info)
     :death-screen     (not (on? :immediate-respawn))
     :limited-crafting (on? :limited-crafting)}))

(defn- join-login-packets [cfg lv eid e]
  (let [{:keys [max-players view-distance simulation-distance]} cfg]
    [(merge (spawn-info lv e) (rule-flags lv)
            {:packet              :login :eid eid
             :levels              schema/dims
             :max-players         (min 255 (long max-players))
             :view-distance       view-distance
             :simulation-distance simulation-distance})
     difficulty-packet
     (own-abilities e)]))

(defn- join-teleport-packet [e]
  (let [p (:pos e)]
    {:packet :player-position :teleport-id (long (:tp-id e 1))
     :pos [(v/x p) (v/y p) (v/z p)] :vel [0.0 0.0 0.0]
     :yaw (double (:yaw e)) :pitch (double (:pitch e)) :relative 0}))

(defn- join-world-packets [world e eid motd]
  (concat
    [(assoc (data/recipes) :packet :update-recipes)]
    (permission-packets world eid e)
    [{:packet :system-chat :overlay false :text (entity/joined-text e)}
     (join-teleport-packet e)
     {:packet :server-data :motd motd}
     border-packet
     (clock-sync-packet world)
     (spawn-pos-packet world)
     (game-event-packet :level-chunks-load-start 0.0)]
    ticking-packets))

(defn- join-player-packets [eid e]
  (let [[entity block] (game-mode/reach-attributes e)
        speed (attribute/modifiers e (:effects e) :movement-speed)
        base (get (attribute/base-values e) :movement-speed)]
    (into [(own-health-packet e)
           (experience-packet e)
           {:packet     :update-attributes :eid eid
            :attributes [entity [:movement-speed base speed] block]}]
          (effect-packets eid e))))

(def ^:private unset-settings
  {:motd "Powered by Collider" :max-players 20
   :view-distance 4 :simulation-distance 2})

(defn join-packets
  "Returns the packets of player eid joining level lv of world."
  [world lv eid]
  (let [cfg (merge unset-settings (:config world))
        e (get-in lv [:entities eid])]
    (concat (join-login-packets cfg lv eid e)
            (join-world-packets world e eid (:motd cfg))
            (join-player-packets eid e))))
