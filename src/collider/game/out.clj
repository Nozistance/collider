(ns collider.game.out
  "Effect vocabulary of the game systems."
  (:refer-clojure :exclude [time meta]))

(set! *warn-on-reflection* true)

(defn to
  "Returns the effect msg addressed to player eid."
  [eid msg] [:fx (assoc msg :to eid)])
(defn all
  "Returns the effect msg for everybody it concerns."
  [msg] [:fx msg])
(defn except
  "Returns the effect msg for everybody but eid."
  [eid msg] [:fx (assoc msg :except eid)])
(defn blocks-changed
  "Returns the effect that a batch of blocks in one chunk now look different."
  [cp records]
  {:msg :blocks-changed :cp cp :records records})

(defn time
  "Returns the effect that carries the world age and the time of day."
  [age time-of-day]
  {:msg :time :age age :time time-of-day})

(defn explosion
  "Returns the effect of an explosion seen and heard at center."
  [center radius blocks motions pitch]
  {:msg     :explosion :center center :radius radius :blocks blocks
   :motions motions :pitch (double pitch)})

(defn teleport
  "Returns the effect that places a player at pos, facing yaw and pitch."
  [pos yaw pitch]
  {:msg :teleport :pos pos :yaw (double yaw) :pitch (double pitch)})

(defn health
  "Returns the effect that a player's health changed."
  [health]
  {:msg :health :health (double health)})

(defn respawn
  "Returns the effect that a player starts over in the world."
  []
  {:msg :respawn})

(defn default-spawn
  "Returns the effect that the world spawn point moved."
  [pos]
  {:msg :default-spawn :pos pos})

(defn rain-started
  "Returns the effect that rain begins."
  []
  {:msg :rain-started})

(defn rain-stopped
  "Returns the effect that rain ends."
  []
  {:msg :rain-stopped})

(defn rain-level
  "Returns the effect that sets how hard it is raining."
  [level]
  {:msg :rain-level :level (double level)})

(defn thunder-level
  "Returns the effect that sets how heavy the storm is."
  [level]
  {:msg :thunder-level :level (double level)})

(defn keepalive
  "Returns the effect that pings a player."
  [id]
  {:msg :keepalive :id id})

(defn disconnect
  "Returns the effect that drops a player with a reason to show."
  [text]
  {:msg :disconnect :text text})

(defn close
  "Returns the effect that closes a connection."
  []
  {:msg :close})

(defn joined
  "Returns the effect that a player is now in the world."
  []
  {:msg :joined})

(defn block-ack
  "Returns the effect that answers a player's block action."
  [sequence]
  {:msg :block-ack :sequence sequence})

(defn set-slot
  "Returns the effect that a slot of a player's inventory holds stack."
  [slot stack]
  {:msg :set-slot :slot slot :stack stack})

(defn carried
  "Returns the effect that sets what a player holds on the cursor."
  [stack]
  {:msg :carried :stack stack})

(defn held-slot
  "Returns the effect that selects a player's hotbar slot."
  [slot]
  {:msg :held-slot :slot slot})

(defn inventory
  "Returns the effect that replaces a player's whole inventory."
  ([slots] (inventory slots nil))
  ([slots carried] {:msg :inventory :slots slots :carried carried}))

(defn suggestions
  "Returns the effect that offers completions for what a player is typing."
  [id start length matches]
  {:msg :suggestions :id id :start start :length length :matches (vec matches)})

(defn system-chat
  "Returns the effect that shows a message from the server in chat."
  [runs]
  {:msg :system-chat :runs runs})

(defn stats
  "Returns the effect that shows a player their statistics."
  [stats]
  {:msg :stats :stats stats})

(defn game-rules
  "Returns the effect that tells a player the game rules in force."
  [rules]
  {:msg :game-rules :rules rules})

(defn overlay
  "Returns the effect that shows a message above the hotbar."
  [runs]
  {:msg :overlay :runs runs})

(defn player-chat
  "Returns the effect that shows what a player said."
  [name runs]
  {:msg :player-chat :name name :runs runs})

(defn tab-add
  "Returns the effect that adds players to the player list."
  [entries]
  {:msg :tab-add :entries entries})

(defn tab-remove
  "Returns the effect that removes players from the player list."
  [uuids]
  {:msg :tab-remove :uuids uuids})

(defn tab-latency
  "Returns the effect that updates the ping shown in the player list."
  [entries]
  {:msg :tab-latency :entries entries})

(defn tab-header
  "Returns the effect that sets the text above and below the player list."
  [header footer]
  {:msg :tab-header :header header :footer footer})

(defn move
  "Returns the effect that an entity moved a short way."
  [eid dx dy dz on-ground]
  {:msg :move :eid eid :dx dx :dy dy :dz dz :on-ground on-ground})

(defn move-look
  "Returns the effect that an entity moved a short way and turned."
  [eid dx dy dz yaw pitch on-ground]
  {:msg :move-look :eid eid :dx dx :dy dy :dz dz :yaw yaw :pitch pitch :on-ground on-ground})

(defn look
  "Returns the effect that an entity turned in place."
  [eid yaw pitch on-ground]
  {:msg :look :eid eid :yaw yaw :pitch pitch :on-ground on-ground})

(defn sync-pos
  "Returns the effect that puts an entity exactly where it belongs."
  [eid pos yaw pitch on-ground]
  {:msg :sync-pos :eid eid :pos pos :yaw yaw :pitch pitch :on-ground on-ground})

(defn head-look
  "Returns the effect that an entity turned its head."
  [eid yaw]
  {:msg :head-look :eid eid :yaw yaw})

(defn meta
  "Returns the effect that an entity changed how it looks or behaves."
  [eid type meta]
  {:msg :meta :eid eid :type type :meta meta})

(defn velocity
  "Returns the effect that an entity was set moving."
  [eid vel]
  {:msg :velocity :eid eid :vel vel})

(defn equipment
  "Returns the effect that an entity wears or holds stack."
  [eid slot stack]
  {:msg :equipment :eid eid :slot slot :stack stack})

(defn animation
  "Returns the effect that an entity plays an animation, such as a swing."
  [eid kind]
  {:msg :animation :eid eid :kind kind})

(defn status
  "Returns the effect that an entity does something brief, such as being hurt."
  [eid kind]
  {:msg :status :eid eid :kind kind})

(defn collect
  "Returns the effect of an item being picked up."
  [item-eid collector-eid]
  {:msg :collect :eid item-eid :collector collector-eid})

(defn sound
  "Returns the effect of a sound played at pos."
  [kind pos volume pitch]
  {:msg :sound :kind kind :pos pos :volume (double volume) :pitch (double pitch)})

(defn particles
  "Returns the effect of particles at pos."
  [kind state pos count speed]
  {:msg :particles :kind kind :state state :pos pos :count count :speed (double speed)})

(defn break-effect
  "Returns the effect of a block breaking, with its sound and shards."
  [pos state]
  {:msg :break-effect :pos pos :state state})

(defn extinguish
  "Returns the effect of a fire going out."
  [pos]
  {:msg :extinguish :pos pos})

(defn fizz
  "Returns the effect of something hot meeting water."
  [pos]
  {:msg :fizz :pos pos})

(def ^:const sound-play-jukebox-song 1010)
(def ^:const sound-stop-jukebox-song 1011)
(def ^:const sound-anvil-broken 1029)
(def ^:const sound-anvil-land 1031)
(def ^:const sound-chorus-grow 1033)
(def ^:const sound-chorus-death 1034)
(def ^:const sound-page-turn 1043)
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
  "Returns the effect of a level event at pos."
  ([event pos] (level-event event pos 0))
  ([event pos data] {:msg :level-event :event event :pos pos :data data}))

(defn sign-editor
  "Returns the effect that opens a sign for a player to write on."
  [pos front?]
  {:msg :sign-editor :pos pos :front? (boolean front?)})

(defn block-event
  "Returns the effect of a block moving in place, such as a chest lid."
  [pos action param]
  {:msg :block-event :pos pos :action action :param param})

(defn open-screen
  "Returns the effect that opens a menu for a player."
  [container menu title]
  {:msg :open-screen :container container :menu menu :title title})

(defn container-content
  "Returns the effect that fills an open menu with items."
  [container state-id items carried]
  {:msg :container-content :container container :state-id state-id :items (vec items) :carried carried})

(defn container-slot
  "Returns the effect that one slot of an open menu holds stack."
  [container state-id slot stack]
  {:msg :container-slot :container container :state-id state-id :slot slot :stack stack})

(defn container-data
  "Returns the effect that a value an open menu shows has changed."
  [container id value]
  {:msg :container-data :container container :id id :value value})

(defn container-close
  "Returns the effect that closes an open menu."
  [container]
  {:msg :container-close :container container})

(defn block-entity
  "Returns the effect that resends the block entity at pos."
  [pos]
  {:msg :block-entity :pos pos})

(defn bonemeal
  "Returns the effect of bone meal sparkling at pos."
  [pos]
  {:msg :bonemeal :pos pos})
