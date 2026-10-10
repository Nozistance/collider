(ns collider.game.out
  "Effect vocabulary of the game systems."
  (:refer-clojure :exclude [time meta]))

(set! *warn-on-reflection* true)

(defn to
  "Returns msg as an effect for player eid alone."
  [eid msg]
  [:fx (assoc msg :to eid)])

(defn all
  "Returns msg as an effect for every player it concerns."
  [msg]
  [:fx msg])

(defn except
  "Returns msg as an effect for every player it concerns but eid."
  [eid msg]
  [:fx (assoc msg :except eid)])

(defn everyone
  "Returns msg as an effect of the server.
  Every player gets it, whatever level it is in."
  [msg]
  [:fx (assoc msg :dim nil)])

(defn load-chunk [id] {:msg :load-chunk :id id})

(defn store-chunk [id payload]
  {:msg :store-chunk :id id :payload payload})

(defn blocks-changed [cp records]
  {:msg :blocks-changed :cp cp :records records})

(defn time
  "Returns the effect that sets the game time age and the clocks,
  their network state by clock. No clocks leaves the clocks of the
  client running."
  [age clocks]
  {:msg :time :age age :clocks clocks})

(defn explosion [center radius blocks motions pitch]
  {:msg     :explosion :center center :radius radius :blocks blocks
   :motions motions :pitch (double pitch)})

(defn teleport
  "Returns the effect that moves a player to pos, turned to yaw and
  pitch. Each bit of relative makes one of them an offset from where
  the player is, or keeps its motion on one axis."
  ([pos yaw pitch] (teleport pos yaw pitch 0))
  ([pos yaw pitch relative]
   {:msg :teleport :pos pos :yaw (double yaw) :pitch (double pitch)
    :relative (long relative)}))

(defn health
  "Returns the effect that shows a player its health and food."
  [health food saturation]
  {:msg :health :health (double health) :food (long food)
   :saturation (double saturation)})

(defn experience
  "Returns the effect that shows a player its experience bar."
  [progress level total]
  {:msg :experience :progress (double progress) :level (long level)
   :total (long total)})

(defn respawn []
  {:msg :respawn})

(defn change-dimension
  "Returns the effect of a player entering level dim at pos.
  Yaw, pitch and relative work as in a teleport. The effect names the
  chunks and entities the player knew in the level it left."
  [dim pos [yaw pitch relative] forget untrack]
  {:msg :change-dimension :dim dim :pos pos :yaw (double yaw)
   :pitch (double pitch) :relative (long relative)
   :forget (vec forget) :untrack (vec untrack)})

(defn default-spawn
  "Returns the effect that shows the world spawn in level dim, faced
  to yaw and pitch."
  [dim pos yaw pitch]
  {:msg :default-spawn :dimension dim :pos pos
   :yaw (double yaw) :pitch (double pitch)})

(defn rule-flag
  "Returns the effect that the rule kind, :immediate-respawn or
  :limited-crafting, is now on? for the clients."
  [kind on?]
  {:msg :rule-flag :kind kind :on? (boolean on?)})

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

(defn combat-kill
  "Returns the effect that shows player eid the cause of its death on
  the death screen."
  [eid text]
  {:msg :combat-kill :eid eid :text text})

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
  "Returns the effect of a finished reload."
  []
  {:msg :reloaded})

(defn view-distance [n] {:msg :view-distance :distance n})

(defn simulation-distance [n]
  {:msg :simulation-distance :distance n})

(defn overlay
  "Returns the effect that shows a message above the hotbar."
  [text]
  {:msg :overlay :text text})

(defn title
  "Returns the effect that shows text as the title, the subtitle or
  the action bar, by kind."
  [kind text]
  {:msg :title :kind kind :text text})

(defn title-times
  "Returns the effect that sets how many ticks titles fade in, stay
  and fade out."
  [fade-in stay fade-out]
  {:msg :title-times :fade-in (long fade-in) :stay (long stay)
   :fade-out (long fade-out)})

(defn clear-titles
  "Returns the effect that hides the titles. With reset? the client
  also forgets their text and times."
  [reset?]
  {:msg :clear-titles :reset (boolean reset?)})

(defn player-rotation
  "Returns the effect that turns a player to yaw and pitch. A
  relative one is added to where the player looks."
  [yaw relative-yaw? pitch relative-pitch?]
  {:msg :player-rotation :yaw (double yaw) :relative-yaw relative-yaw?
   :pitch (double pitch) :relative-pitch relative-pitch?})

(defn look-at
  "Returns the effect that turns a player to look at pos, or at the
  anchor of entity id when there is one."
  [from pos id anchor]
  {:msg :look-at :from from :pos pos :id id :anchor anchor})

(defn waypoint
  "Returns the effect that puts op (:track, :update or :untrack) on
  the point of uuid in the locator bar. Kind is :block, :chunk or
  :azimuth, with at a block, a chunk or an angle."
  [op uuid kind at]
  {:msg :waypoint :op op :uuid uuid :kind kind :at at})

(defn player-chat
  "Returns the effect that shows a line a player said.
  The text component already carries its sender."
  [text]
  {:msg :player-chat :text text})

(defn tab-add [entries]
  {:msg :tab-add :entries entries})

(defn tab-remove [uuids]
  {:msg :tab-remove :uuids uuids})

(defn tab-latency [entries]
  {:msg :tab-latency :entries entries})

(defn tab-game-mode
  "Returns the effect that shows the game mode of the player with uuid
  in the player list."
  [uuid mode]
  {:msg :tab-game-mode :uuid uuid :mode mode})

(defn camera
  "Returns the effect that tells a spectator the entity of id it
  looks through."
  [id]
  {:msg :camera :id id})

(defn game-mode
  "Returns the effect that tells a player its new game mode."
  [mode]
  {:msg :game-mode :mode mode})

(defn abilities
  "Returns the effect that tells a player what it may do."
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
  "Returns the effect that shows the attributes of an entity.
  Each is [name base modifiers]."
  [eid attrs]
  {:msg :attributes :eid eid :attributes attrs})

(defn velocity [eid vel]
  {:msg :velocity :eid eid :vel vel})

(defn equipment [eid slot stack]
  {:msg :equipment :eid eid :slot slot :stack stack})

(defn animation [eid kind]
  {:msg :animation :eid eid :kind kind})

(defn damage-event
  "Returns the effect that entity eid took a full hit of damage type
  kind, caused by entity cause through entity direct, from pos. Each
  of these may be nil."
  [eid kind cause direct pos]
  {:msg :damage-event :eid eid :kind kind :cause cause
   :direct direct :pos pos})

(defn status
  "Returns the effect that an entity does something brief.
  Being hurt is one such thing."
  [eid kind]
  {:msg :status :eid eid :kind kind})

(defn collect [item-eid collector-eid]
  {:msg :collect :eid item-eid :collector collector-eid})

(defn sound
  "Returns the effect of a sound at pos.
  Source names the mixer channel when it is not the one the sound is
  listed under."
  ([kind pos volume pitch] (sound kind pos volume pitch nil))
  ([kind pos volume pitch source]
   (cond-> {:msg :sound :kind kind :pos pos
            :volume (double volume) :pitch (double pitch)}
     source (assoc :source source))))

(def sound-sources
  "The mixer channels of sounds, in the order the client numbers
  them."
  ["master" "music" "record" "weather" "block" "hostile" "neutral"
   "player" "ambient" "voice" "ui"])

(defn named-sound
  "Returns the effect of the sound named id, registered or not, on
  mixer channel source at pos. Seed picks its variant."
  [id source pos volume pitch seed]
  {:msg :named-sound :id id :source source :pos pos
   :volume (double volume) :pitch (double pitch) :seed (long seed)})

(defn stop-sound
  "Returns the effect that stops the sound named id on channel
  source. Nil for either stops all of them."
  [id source]
  {:msg :stop-sound :id id :source source})

(defn entity-sound
  "Returns the effect of a sound that follows entity eid at pos."
  [kind eid pos volume pitch source]
  (assoc (sound kind pos volume pitch source) :entity eid))

(defn block-sound
  "Returns the effect of a sound at the centre of the block at pos."
  ([kind pos volume pitch] (block-sound kind pos volume pitch nil))
  ([kind [x y z] volume pitch source]
   (sound kind [(+ (double x) 0.5) (+ (double y) 0.5)
                (+ (double z) 0.5)]
          volume pitch source)))

(defn particle
  "Returns the effect of count particles p, as [type options], at pos,
  spread by delta and moving at speed. Force lets them show farther
  and past the client's own particle setting."
  [p pos delta speed count force]
  {:msg :particle :particle p :pos pos :delta delta
   :speed (double speed) :count (long count) :force (boolean force)})

(defn particles
  "Returns the effect of count particles of kind at pos, spread by
  the offsets dxyz."
  ([kind state pos count speed]
   (particles kind state pos count speed nil))
  ([kind state pos count speed dxyz]
   (cond-> {:msg :particles :kind kind :state state :pos pos
            :count count :speed (double speed)}
     dxyz (assoc :spread dxyz))))

(defn trail
  "Returns the effect of a trail particle from pos to target, of an
  rgb color, that lasts ticks."
  [pos target color ticks]
  {:msg :trail :pos pos :target target :color color :ticks ticks})

(defn destroy-stage
  "Returns the effect that shows the crack stage of the block at pos
  that player eid digs, -1 for none."
  [eid pos stage]
  {:msg :destroy-stage :eid eid :pos pos :stage stage})

(defn break-effect [pos state]
  {:msg :break-effect :pos pos :state state})

(defn extinguish [pos]
  {:msg :extinguish :pos pos})

(defn fizz
  "Returns the effect of something hot meeting water."
  [pos]
  {:msg :fizz :pos pos})

(def sound-play-jukebox-song :sound-play-jukebox-song)

(def sound-stop-jukebox-song :sound-stop-jukebox-song)

(def sound-extinguish-fire :sound-extinguish-fire)

(def sound-anvil-broken :sound-anvil-broken)

(def sound-anvil-used :sound-anvil-used)

(def sound-anvil-land :sound-anvil-land)

(def sound-brewing-stand-brew :sound-brewing-stand-brew)

(def sound-grindstone-used :sound-grindstone-used)

(def sound-page-turn :sound-page-turn)

(def sound-smithing-table-used :sound-smithing-table-used)

(def sound-drip-lava-into-cauldron :sound-drip-lava-into-cauldron)

(def sound-drip-water-into-cauldron :sound-drip-water-into-cauldron)

(def sound-pointed-dripstone-land :sound-pointed-dripstone-land)

(def composter-fill :composter-fill)

(def dripstone-drip :dripstone-drip)

(def particles-destroy-block :particles-destroy-block)

(def particles-and-sound-wax-on :particles-and-sound-wax-on)

(def particles-wax-off :particles-wax-off)

(def particles-scrape :particles-scrape)

(defn level-event
  "Returns level event event at pos with data. The event is the name
  of the level event as a keyword."
  ([event pos] (level-event event pos 0))
  ([event pos data]
   {:msg :level-event :event event :pos pos :data data}))

(defn sign-editor [pos front?]
  {:msg :sign-editor :pos pos :front? (boolean front?)})

(defn open-book [hand] {:msg :open-book :hand hand})

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

(defn- effect-flags ^long [i blend?]
  (cond-> 0
    (:ambient? i) (bit-or 1)
    (:visible? i) (bit-or 2)
    (:icon? i) (bit-or 4)
    blend? (bit-or 8)))

(defn mob-effect
  "Returns the effect that shows effect k of entity eid as instance
  i. With blend? the client fades it in."
  [eid k i blend?]
  {:msg :mob-effect :eid eid :effect k
   :amplifier (long (:amplifier i)) :duration (long (:duration i))
   :flags (effect-flags i blend?)})

(defn mob-effect-gone
  "Returns the effect that effect k of entity eid ended."
  [eid k]
  {:msg :mob-effect-gone :eid eid :effect k})
