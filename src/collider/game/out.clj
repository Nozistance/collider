(ns collider.game.out
  "Effect vocabulary of the game systems."
  (:refer-clojure :exclude [time meta]))

(set! *warn-on-reflection* true)

(defn to [eid msg] [:fx (assoc msg :to eid)])

(defn all [msg] [:fx msg])

(defn except [eid msg] [:fx (assoc msg :except eid)])

(defn everyone
  "Returns msg as the server's effect: every player gets it,
  whatever level they are in."
  [msg]
  [:fx (assoc msg :dim nil)])

(defn load-chunk [id] {:msg :load-chunk :id id})

(defn store-chunk [id payload]
  {:msg :store-chunk :id id :payload payload})

(defn blocks-changed [cp records]
  {:msg :blocks-changed :cp cp :records records})

(defn time [age time-of-day]
  {:msg :time :age age :time time-of-day})

(defn explosion [center radius blocks motions pitch]
  {:msg     :explosion :center center :radius radius :blocks blocks
   :motions motions :pitch (double pitch)})

(defn teleport
  "Returns the effect that moves a player to pos, turned to yaw and
  pitch. Each bit of relative makes one of them an offset from
  where the player is, or keeps its motion on one axis."
  ([pos yaw pitch] (teleport pos yaw pitch 0))
  ([pos yaw pitch relative]
   {:msg :teleport :pos pos :yaw (double yaw) :pitch (double pitch)
    :relative (long relative)}))

(defn health [health]
  {:msg :health :health (double health)})

(defn respawn []
  {:msg :respawn})

(defn change-dimension
  "Returns the effect of a player entering level dim at pos.
  yaw, pitch and relative are as in a teleport. It names the
  chunks and entities the player knew in the level it left."
  [dim pos [yaw pitch relative] forget untrack]
  {:msg :change-dimension :dim dim :pos pos :yaw (double yaw)
   :pitch (double pitch) :relative (long relative)
   :forget (vec forget) :untrack (vec untrack)})

(defn default-spawn
  "Returns the effect that shows the world spawn in level dim,
  faced to yaw and pitch."
  [dim pos yaw pitch]
  {:msg :default-spawn :dimension dim :pos pos
   :yaw (double yaw) :pitch (double pitch)})

(defn rain-started []
  {:msg :rain-started})

(defn rain-stopped []
  {:msg :rain-stopped})

(defn rain-level [level]
  {:msg :rain-level :level (double level)})

(defn thunder-level [level]
  {:msg :thunder-level :level (double level)})

(defn keepalive [id]
  {:msg :keepalive :id id})

(defn disconnect [text]
  {:msg :disconnect :text text})

(defn close []
  {:msg :close})

(defn joined []
  {:msg :joined})

(defn block-ack [sequence]
  {:msg :block-ack :sequence sequence})

(defn set-slot [slot stack]
  {:msg :set-slot :slot slot :stack stack})

(defn carried [stack]
  {:msg :carried :stack stack})

(defn held-slot [slot]
  {:msg :held-slot :slot slot})

(defn inventory
  ([slots] (inventory slots nil))
  ([slots carried] {:msg :inventory :slots slots :carried carried}))

(defn suggestions [id start length matches]
  {:msg :suggestions :id id :start start :length length
   :matches (vec matches)})

(defn system-chat
  "Returns the effect that shows the text component in chat."
  [text]
  {:msg :system-chat :text text})

(defn stats [stats]
  {:msg :stats :stats stats})

(defn game-rules [rules]
  {:msg :game-rules :rules rules})

(defn reload
  "Returns the request that the server reread its config."
  []
  {:msg :reload})

(defn reloaded
  "Returns the effect of a finished reload: the data a reload
  resends to the players."
  []
  {:msg :reloaded})

(defn view-distance [n] {:msg :view-distance :distance n})

(defn simulation-distance [n]
  {:msg :simulation-distance :distance n})

(defn overlay
  "Returns the effect that shows a message above the hotbar."
  [text]
  {:msg :overlay :text text})

(defn player-chat
  "Returns the effect that shows a line a player said, the text
  component already decorated with its sender."
  [text]
  {:msg :player-chat :text text})

(defn tab-add [entries]
  {:msg :tab-add :entries entries})

(defn tab-remove [uuids]
  {:msg :tab-remove :uuids uuids})

(defn tab-latency [entries]
  {:msg :tab-latency :entries entries})

(defn tab-game-mode
  "Returns the effect that shows the game mode of the player with
  uuid in the player list."
  [uuid mode]
  {:msg :tab-game-mode :uuid uuid :mode mode})

(defn game-mode
  "Returns the effect that tells a player its new game mode."
  [mode]
  {:msg :game-mode :mode mode})

(defn abilities
  "Returns the effect that tells a player what it may do: a map of
  :invulnerable? :flying? :may-fly? and :instabuild?."
  [m]
  (assoc m :msg :abilities))

(defn tab-header [header footer]
  {:msg :tab-header :header header :footer footer})

(defn move
  "Returns the effect that an entity moved a short way."
  [eid dx dy dz on-ground]
  {:msg :move :eid eid :dx dx :dy dy :dz dz :on-ground on-ground})

(defn move-look
  "Returns the effect that an entity moved a short way and turned."
  [eid dx dy dz yaw pitch on-ground]
  {:msg :move-look :eid eid :dx dx :dy dy :dz dz :yaw yaw
   :pitch pitch :on-ground on-ground})

(defn look [eid yaw pitch on-ground]
  {:msg :look :eid eid :yaw yaw :pitch pitch :on-ground on-ground})

(defn sync-pos [eid pos yaw pitch on-ground]
  {:msg :sync-pos :eid eid :pos pos :yaw yaw :pitch pitch
   :on-ground on-ground})

(defn head-look [eid yaw]
  {:msg :head-look :eid eid :yaw yaw})

(defn meta [eid type meta]
  {:msg :meta :eid eid :type type :meta meta})

(defn attributes
  "Returns the effect that shows the attributes of an entity, each
  [name base modifiers]."
  [eid attrs]
  {:msg :attributes :eid eid :attributes attrs})

(defn velocity [eid vel]
  {:msg :velocity :eid eid :vel vel})

(defn equipment [eid slot stack]
  {:msg :equipment :eid eid :slot slot :stack stack})

(defn animation [eid kind]
  {:msg :animation :eid eid :kind kind})

(defn status
  "Returns the effect that an entity does something brief.
  Being hurt is one such thing."
  [eid kind]
  {:msg :status :eid eid :kind kind})

(defn collect [item-eid collector-eid]
  {:msg :collect :eid item-eid :collector collector-eid})

(defn sound
  "Returns the effect of a sound at pos. Source names the mixer
  channel when it is not the one the sound is listed under."
  ([kind pos volume pitch] (sound kind pos volume pitch nil))
  ([kind pos volume pitch source]
   (cond-> {:msg :sound :kind kind :pos pos
            :volume (double volume) :pitch (double pitch)}
           source (assoc :source source))))

(defn particles [kind state pos count speed]
  {:msg :particles :kind kind :state state :pos pos :count count
   :speed (double speed)})

(defn break-effect [pos state]
  {:msg :break-effect :pos pos :state state})

(defn extinguish [pos]
  {:msg :extinguish :pos pos})

(defn fizz
  "Returns the effect of something hot meeting water."
  [pos]
  {:msg :fizz :pos pos})

(def ^:const sound-play-jukebox-song 1010)

(def ^:const sound-stop-jukebox-song 1011)

(def ^:const sound-extinguish-fire 1009)

(def ^:const sound-anvil-broken 1029)

(def ^:const sound-anvil-used 1030)

(def ^:const sound-anvil-land 1031)

(def ^:const sound-chorus-grow 1033)

(def ^:const sound-chorus-death 1034)

(def ^:const sound-brewing-stand-brew 1035)

(def ^:const sound-grindstone-used 1042)

(def ^:const sound-page-turn 1043)

(def ^:const sound-smithing-table-used 1044)

(def ^:const sound-drip-lava-into-cauldron 1046)

(def ^:const sound-drip-water-into-cauldron 1047)

(def ^:const sound-pointed-dripstone-land 1045)

(def ^:const composter-fill 1500)

(def ^:const dripstone-drip 1504)

(def ^:const particles-destroy-block 2001)

(def ^:const particles-and-sound-wax-on 3003)

(def ^:const particles-wax-off 3004)

(def ^:const particles-scrape 3005)

(defn level-event
  ([event pos] (level-event event pos 0))
  ([event pos data]
   {:msg :level-event :event event :pos pos :data data}))

(defn sign-editor [pos front?]
  {:msg :sign-editor :pos pos :front? (boolean front?)})

(defn block-event
  "Returns the effect of a block moving in place, like a chest lid."
  [pos action param]
  {:msg :block-event :pos pos :action action :param param})

(defn open-screen [container menu title]
  {:msg :open-screen :container container :menu menu :title title})

(defn container-content [container state-id items carried]
  {:msg :container-content :container container :state-id state-id
   :items (vec items) :carried carried})

(defn container-slot [container state-id slot stack]
  {:msg :container-slot :container container :state-id state-id
   :slot slot :stack stack})

(defn container-data
  "Returns the effect that a value an open menu shows has changed."
  [container id value]
  {:msg :container-data :container container :id id :value value})

(defn container-close [container]
  {:msg :container-close :container container})

(defn block-entity [pos]
  {:msg :block-entity :pos pos})

(defn bonemeal [pos]
  {:msg :bonemeal :pos pos})

(defn cooldown
  "Returns the effect that a cooldown group locked for ticks."
  [group ticks]
  {:msg :cooldown :group group :ticks ticks})
