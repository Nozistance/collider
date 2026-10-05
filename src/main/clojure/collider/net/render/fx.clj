(ns collider.net.render.fx
  "Packets of the effects of a tick."
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.gamerules :as rules]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.log :as log]
            [collider.net.render.join :as join]
            [collider.net.render.tracked :as tracked]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- block-state ^long [world pos]
  (chunk/at (:chunks world) pos))

(def ^:private ^:table explosion-block-particles
  (delay (let [id #(data/registry-id "particle_type" %)]
           [[[(id :poof) nil] 0.5 1.0 1]
            [[(id :smoke) nil] 1.0 1.0 1]])))

(def ^:private ^:table explosion-particle
  (delay [(data/registry-id "particle_type" :explosion-emitter) nil]))

(defn- particle-id ^long [kind]
  (data/registry-id "particle_type" kind))

(defn- particles-packet [m]
  (let [[dx dy dz] (or (:spread m) [0.0 0.0 0.0])]
    {:packet   :level-particles
     :particle [(particle-id (:kind m)) (:state m)]
     :pos      (:pos m) :count (:count m) :speed (:speed m)
     :dx (double dx) :dy (double dy) :dz (double dz)}))

(def ^:private ^:table trail-particle
  (delay (data/registry-id "particle_type" :trail)))

(defn- trail-packet [m]
  {:packet   :level-particles
   :particle [@trail-particle [(:target m) (:color m) (:ticks m)]]
   :pos      (:pos m) :count 1 :speed 0.0 :dx 0.0 :dy 0.0 :dz 0.0})

(defn- once! [kind]
  (log/once! [::unrendered kind]
             log/warn "render:" kind "not rendered yet"))

(defn- section-index ^long [[x y z]]
  (bit-or (bit-shift-left (bit-and (long x) 15) 8)
          (bit-shift-left (bit-and (long z) 15) 4)
          (bit-and (long y) 15)))

(defn- section-change [[pos st]] [(section-index pos) st])

(defn- section-of ^long [[[_ y _] _]] (bit-shift-right (long y) 4))

(defn- block-records [[cx cz] records]
  (if (= 1 (count records))
    (let [[[pos st]] records]
      [{:packet :block-update :pos pos :state st}])
    (for [[sy recs] (group-by section-of records)]
      {:packet  :section-blocks-update :section [cx sy cz]
       :changes (map section-change recs)})))

(def ^:private entity-events
  {:hop 1 :death 3 :break 3 :eat 10 :break-main 47 :break-off 48
   :love 18 :peek 64 :teleport 46 :reduced-debug 22 :full-debug 23
   :honey-jump 54 :lower-head 58 :raise-head 59})

(defn- status-packet [m]
  (when-let [ev (entity-events (:kind m))]
    {:packet :entity-event :eid (:eid m) :event ev}))

(defn- entity-ref ^long [eid] (if eid (inc (long eid)) 0))

(defn- damage-event-packet [m]
  {:packet :damage-event :eid (:eid m)
   :kind (data/entry-id "damage_type" (:kind m))
   :cause (entity-ref (:cause m)) :direct (entity-ref (:direct m))
   :pos (:pos m)})

(def ^:private source-ids (zipmap out/sound-sources (range)))

(def ^:private sound-sources
  (update-vals {:records "record" :blocks "block" :neutral "neutral"
                :players "player"}
               source-ids))

(defn- sound-id [kind]
  (let [reg (get (data/registries) "sound_event")]
    (if-let [[ev src] (get data/sound-table kind)]
      (when-let [id (get reg ev)] [id src])
      (when-let [id (get reg kind)] [id (sound-sources :blocks)]))))

(defn- sound-packet [m]
  (if-let [[id src] (sound-id (:kind m))]
    (let [p {:sound id :seed 0 :volume (:volume m) :pitch (:pitch m)
             :source (get sound-sources (:source m) src)}]
      (if-let [eid (:entity m)]
        (assoc p :packet :sound-entity :eid eid)
        (assoc p :packet :sound :pos (:pos m))))
    (once! [:sound (:kind m)])))

(def ^:private anchors {:feet 0 :eyes 1})

(defn- rotation-packet [m]
  (assoc (select-keys m [:yaw :relative-yaw :pitch :relative-pitch])
         :packet :player-rotation))

(defn- look-at-packet [m]
  {:packet :player-look-at :from (anchors (:from m)) :pos (:pos m)
   :id (:id m) :to (some-> (:anchor m) anchors)})

(defn- title-times-packet [m]
  (assoc (select-keys m [:fade-in :stay :fade-out])
         :packet :set-titles-animation))

(defn- named-sound-packet [m]
  {:packet :sound :sound (:id m)
   :source (source-ids (:source m)) :pos (:pos m)
   :volume (:volume m) :pitch (:pitch m) :seed (:seed m)})

(defn- stop-sound-packet [m]
  {:packet :stop-sound :id (:id m) :source (source-ids (:source m))})

(defn explode-packet
  "Returns the explosion m as player eid sees it."
  [m eid]
  (let [k (get (:motions m) eid)]
    {:packet    :explode :center (:center m) :radius (:radius m)
     :blocks    (:blocks m)
     :knockback (when (and k (some #(not (zero? (double %))) k)) k)
     :particle  @explosion-particle
     :sound     (first (sound-id :explosion))
     :block-particles @explosion-block-particles}))

(defn- rule-pair [[k v]]
  [(rules/wire-name k) (rules/serialize k v)])

(def ^:private title-packets
  {:title :set-title-text :subtitle :set-subtitle-text
   :actionbar :set-action-bar-text})

(defn- chat-fx [_ m]
  [{:packet :system-chat :overlay false :text (:text m)}])

(def ^:private session-fx
  {:teleport      (fn [_ m]
                    [{:packet :player-position :teleport-id 0
                      :pos (:pos m) :vel [0.0 0.0 0.0]
                      :yaw (:yaw m) :pitch (:pitch m)
                      :relative (:relative m 0)}])
   :keepalive     (fn [_ m] [{:packet :keep-alive :id (:id m)}])
   :disconnect    (fn [_ m] [{:packet :disconnect :text (:text m)}])
   :system-chat   chat-fx
   :overlay       (fn [_ m]
                    [{:packet :system-chat :overlay true
                      :text (:text m)}])
   :title         (fn [_ m]
                    [{:packet (title-packets (:kind m))
                      :text (:text m)}])
   :title-times   (fn [_ m] [(title-times-packet m)])
   :named-sound   (fn [_ m] [(named-sound-packet m)])
   :player-rotation (fn [_ m] [(rotation-packet m)])
   :look-at       (fn [_ m] [(look-at-packet m)])
   :stop-sound    (fn [_ m] [(stop-sound-packet m)])
   :clear-titles  (fn [_ m]
                    [{:packet :clear-titles :reset (:reset m)}])
   :player-chat   chat-fx
   :stats         (fn [_ m]
                    [{:packet :award-stats :stats (:stats m)}])
   :suggestions   (fn [_ m]
                    [{:packet :command-suggestions :id (:id m)
                      :start (:start m) :length (:length m)
                      :matches (:matches m)}])
   :game-rules    (fn [_ m]
                    [{:packet :game-rule-values
                      :values (into {} (map rule-pair) (:rules m))}])
   :health        (fn [_ m] [(join/health-packet (:health m))])
   :experience    (fn [_ m]
                    [{:packet :set-experience :progress (:progress m)
                      :level (:level m) :total (:total m)}])
   :mob-effect    (fn [_ m]
                    [{:packet :update-mob-effect :eid (:eid m)
                      :effect (:effect m) :amplifier (:amplifier m)
                      :duration (:duration m) :flags (:flags m)}])
   :mob-effect-gone (fn [_ m]
                      [{:packet :remove-mob-effect :eid (:eid m)
                        :effect (:effect m)}])
   :cooldown      (fn [_ m]
                    [{:packet :cooldown :group (:group m)
                      :duration (:ticks m)}])
   :change-dimension join/change-dimension-packets
   :respawn       join/respawn-packets
   :abilities     (fn [_ m] [(join/abilities-packet m)])
   :camera        (fn [_ m] [{:packet :set-camera :id (:id m)}])
   :game-mode     (fn [_ m]
                    [(join/game-event-packet
                       :change-game-mode
                       (double (game-mode/id (:mode m))))])
   :default-spawn (fn [_ m]
                    [{:packet :set-default-spawn-position
                      :dimension (:dimension m) :pos (:pos m)
                      :yaw (:yaw m 0.0) :pitch (:pitch m 0.0)}])
   :joined        (fn [_ _] nil)
   :close         (fn [_ _] [:close])})

(defn- event-block [world pos]
  (data/registry-id "block" (block/block-of (block-state world pos))))

(defn- block-entity-packet [pos e t]
  {:packet :block-entity-data :pos pos
   :type (be/type-id e) :nbt (be/nbt e t)})

(defn- block-entity-fx [world m]
  (when-let [e (be/at world (:pos m))]
    (when (be/on-wire? e)
      [(block-entity-packet (:pos m) e (:tick world))])))

(def ^:private plant-growth-particles 15)

(def level-events
  "The wire ids of the level events by name."
  {:sound-extinguish-fire            1009
   :sound-play-jukebox-song          1010
   :sound-stop-jukebox-song          1011
   :sound-anvil-broken               1029
   :sound-anvil-used                 1030
   :sound-anvil-land                 1031
   :sound-chorus-grow                1033
   :sound-chorus-death               1034
   :sound-brewing-stand-brew         1035
   :sound-grindstone-used            1042
   :sound-page-turn                  1043
   :sound-smithing-table-used        1044
   :sound-pointed-dripstone-land     1045
   :sound-drip-lava-into-cauldron    1046
   :sound-drip-water-into-cauldron   1047
   :composter-fill                   1500
   :lava-fizz                        1501
   :dripstone-drip                   1504
   :particles-and-sound-plant-growth 1505
   :particles-destroy-block          2001
   :particles-spell-potion-splash    2002
   :particles-instant-potion-splash  2007
   :particles-water-evaporating      2009
   :particles-and-sound-wax-on       3003
   :particles-wax-off                3004
   :particles-scrape                 3005})

(defn- level-event-packet [event pos data]
  {:packet :level-event :event (level-events event) :pos pos
   :data data})

(defn- weather-fx [event]
  (fn [_ m] [(join/game-event-packet event (:level m))]))

(defn- level-event-fx [event data]
  (fn [_ m] [(level-event-packet event (:pos m) (data m))]))

(defn- sign-editor-packet [m]
  {:packet :open-sign-editor :pos (:pos m) :front? (:front? m)})

(defn- open-book-packet [m]
  {:packet :open-book :hand (if (= :off (:hand m)) 1 0)})

(defn- reloaded-packets []
  [{:packet :update-tags :tags (data/tags)}
   (assoc (data/recipes) :packet :update-recipes)])

(defn- rule-flag-packet [m]
  (join/game-event-packet (:kind m) (if (:on? m) 1.0 0.0)))

(defn- rain-packet [event]
  (join/game-event-packet event 0.0))

(def ^:private world-fx
  {:rule-flag      (fn [_ m] [(rule-flag-packet m)])
   :rain-started   (fn [_ _] [(rain-packet :start-raining)])
   :rain-stopped   (fn [_ _] [(rain-packet :stop-raining)])
   :rain-level     (weather-fx :rain-level-change)
   :thunder-level  (weather-fx :thunder-level-change)
   :time           (fn [_ m] [(join/set-time-packet m)])
   :blocks-changed (fn [_ m]
                     (let [cp (chunk/id->pos (:cp m))]
                       (block-records cp (:records m))))
   :break-effect   (level-event-fx :particles-destroy-block :state)
   :fizz           (level-event-fx :lava-fizz (constantly 0))
   :bonemeal
   (level-event-fx :particles-and-sound-plant-growth
                   (constantly plant-growth-particles))
   :extinguish
   (level-event-fx :sound-extinguish-fire (constantly 0))
   :level-event    (fn [_ m]
                     [(level-event-packet
                        (:event m) (:pos m) (:data m 0))])
   :sign-editor    (fn [_ m] [(sign-editor-packet m)])
   :open-book      (fn [_ m] [(open-book-packet m)])
   :block-event    (fn [world m]
                     [{:packet :block-event :pos (:pos m)
                       :action (:action m)
                       :param (bit-and (long (:param m)) 255)
                       :block  (event-block world (:pos m))}])
   :block-entity   block-entity-fx
   :sound          (fn [_ m] (when-let [p (sound-packet m)] [p]))
   :particles      (fn [_ m] [(particles-packet m)])
   :trail          (fn [_ m] [(trail-packet m)])
   :explosion      (fn [_ _] nil)
   :load-chunk     (fn [_ _] nil)
   :store-chunk    (fn [_ _] nil)
   :reload         (fn [_ _] nil)
   :reloaded       (fn [_ _] (reloaded-packets))
   :view-distance  (fn [_ m]
                     [{:packet :set-chunk-cache-radius
                       :radius (:distance m)}])
   :simulation-distance
   (fn [_ m]
     [{:packet :set-simulation-distance :distance (:distance m)}])})

(defn- open-screen-packet [m]
  {:packet :open-screen :container (:container m)
   :menu (data/registry-id "menu" (:menu m)) :title (:title m)})

(defn- content-packet [m]
  {:packet :container-set-content :container (:container m)
   :state-id (:state-id m) :items (:items m) :carried (:carried m)})

(defn- container-slot-packet [m]
  {:packet :container-set-slot :container (:container m)
   :state-id (:state-id m) :slot (:slot m) :stack (:stack m)})

(defn- container-data-packet [m]
  {:packet :container-set-data :container (:container m)
   :id (:id m) :value (:value m)})

(defn- cursor-packet [m]
  {:packet :set-cursor-item :stack (:stack m)})

(defn- held-slot-packet [m]
  {:packet :set-held-slot :slot (:slot m)})

(defn- own-slot-packet [m]
  {:packet   :container-set-slot :container 0
   :state-id 0 :slot (:slot m) :stack (:stack m)})

(defn- own-content-packet [m]
  {:packet   :container-set-content :container 0
   :state-id 0 :items (:slots m) :carried (:carried m)})

(defn- close-packet [m]
  {:packet :container-close :container (:container m)})

(defn- block-ack-packet [m]
  {:packet :block-changed-ack :sequence (:sequence m)})

(defn- listed [entry]
  (-> entry
      (dissoc :game-mode)
      (assoc :gamemode (game-mode/id (:game-mode entry)))))

(defn- tab-add-packet [m]
  {:packet :player-info-update :players (mapv listed (:entries m))})

(defn- tab-game-mode-packet [m]
  {:packet :player-info-update :action :game-mode
   :players [{:uuid (:uuid m) :gamemode (game-mode/id (:mode m))}]})

(defn- tab-remove-packet [m]
  {:packet :player-info-remove :uuids (:uuids m)})

(defn- tab-latency-packet [m]
  {:packet :player-info-update :action :latency
   :players (:entries m)})

(defn- tab-header-packet [m]
  {:packet :tab-list :header (:header m) :footer (:footer m)})

(def ^:private container-fx
  {:set-slot          (fn [_ m] [(own-slot-packet m)])
   :carried           (fn [_ m] [(cursor-packet m)])
   :open-screen       (fn [_ m] [(open-screen-packet m)])
   :container-content (fn [_ m] [(content-packet m)])
   :container-slot    (fn [_ m] [(container-slot-packet m)])
   :container-data    (fn [_ m] [(container-data-packet m)])
   :container-close   (fn [_ m] [(close-packet m)])
   :held-slot         (fn [_ m] [(held-slot-packet m)])
   :block-ack         (fn [_ m] [(block-ack-packet m)])
   :inventory         (fn [_ m] [(own-content-packet m)])
   :tab-add           (fn [_ m] [(tab-add-packet m)])
   :tab-remove        (fn [_ m] [(tab-remove-packet m)])
   :tab-game-mode     (fn [_ m] [(tab-game-mode-packet m)])
   :tab-latency       (fn [_ m] [(tab-latency-packet m)])
   :tab-header        (fn [_ m] [(tab-header-packet m)])})

(def ^:private animate-actions
  {:swing 0 :wake-up 2 :swing-off 3 :crit 4})

(defn- animate-action ^long [kind]
  (get animate-actions kind 0))

(defn- head-look-packet [m]
  {:packet :rotate-head :eid (:eid m) :yaw (:yaw m)})

(defn- velocity-packet [m]
  {:packet :set-entity-motion :eid (:eid m) :vel (:vel m)})

(defn- collect-packet [m]
  {:packet :take-item-entity :item (:eid m)
   :collector (:collector m) :amount 1})

(defn- meta-packets [m]
  (let [d (tracked/entity-data (tracked/kind-of m) (:meta m))]
    (when (seq d)
      [{:packet :set-entity-data :eid (:eid m) :data d}])))

(def ^:private entity-fx
  {:move      (fn [_ m]
                [{:packet :move-entity-pos :eid (:eid m)
                  :dx (:dx m) :dy (:dy m) :dz (:dz m)
                  :on-ground (:on-ground m)}])
   :move-look (fn [_ m]
                [{:packet :move-entity-pos-rot :eid (:eid m)
                  :dx (:dx m) :dy (:dy m) :dz (:dz m)
                  :yaw (:yaw m) :pitch (:pitch m)
                  :on-ground (:on-ground m)}])
   :look      (fn [_ m]
                [{:packet :move-entity-rot :eid (:eid m)
                  :yaw (:yaw m) :pitch (:pitch m)
                  :on-ground (:on-ground m)}])
   :sync-pos  (fn [_ m]
                [{:packet :entity-position-sync :eid (:eid m)
                  :pos (:pos m) :vel [0.0 0.0 0.0]
                  :yaw (:yaw m) :pitch (:pitch m)
                  :on-ground (:on-ground m)}])
   :head-look (fn [_ m] [(head-look-packet m)])
   :velocity  (fn [_ m] [(velocity-packet m)])
   :meta      (fn [_ m] (meta-packets m))
   :equipment (fn [_ m]
                (let [slot (tracked/equipment-slots (:slot m))]
                  [{:packet :set-equipment :eid (:eid m)
                    :slots [[slot (:stack m)]]}]))
   :animation (fn [_ m]
                [{:packet :animate :eid (:eid m)
                  :action (animate-action (:kind m))}])
   :status    (fn [_ m] (when-let [p (status-packet m)] [p]))
   :damage-event (fn [_ m] [(damage-event-packet m)])
   :collect   (fn [_ m] [(collect-packet m)])
   :attributes (fn [_ m]
                 [{:packet :update-attributes :eid (:eid m)
                   :attributes (:attributes m)}])})

(def ^:private fx-table
  (merge session-fx world-fx container-fx entity-fx))

(defn fx-packets
  "Returns the packets of effect m in level world."
  [world m]
  (if-let [f (fx-table (:msg m))]
    (f world m)
    (once! (:msg m))))
