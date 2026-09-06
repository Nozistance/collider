(ns collider.game.out
  "Effects: what systems tell players. An effect is a map with :msg.
   Positions in blocks, angles in degrees, stacks as {:item :count}, block
   states as ids. `to` addresses one player, `except` everyone but one, `all`
   everyone concerned; who receives what is decided in collider.render."
  (:refer-clojure :exclude [time meta]))

(set! *warn-on-reflection* true)

(defn to
  "Effect for one player."
  [eid msg] [:fx (assoc msg :to eid)])
(defn all
  "Effect for everyone concerned."
  [msg] [:fx msg])
(defn except
  "Effect for everyone concerned but eid, usually the player who caused it."
  [eid msg] [:fx (assoc msg :except eid)])
(defn blocks-changed
  "Blocks of one chunk that changed: cp is the chunk id, records [[pos state] …]."
  [cp records]
  {:msg :blocks-changed :cp cp :records records})

(defn time
  "Clock broadcast; the values sent are those of the world after the tick."
  [age time-of-day]
  {:msg :time :age age :time time-of-day})

(defn explosion
  "Explosion as the 26.2 client sees it: the centre, the radius, how many
   blocks went (it draws the smoke from the count) and the knockback of the
   addressee."
  [center radius blocks motion]
  {:msg :explosion :center center :radius radius :blocks blocks :motion motion})

(defn teleport [pos yaw pitch]
  {:msg :teleport :pos pos :yaw (double yaw) :pitch (double pitch)})

(defn health [health]
  {:msg :health :health (double health)})

(defn respawn []
  {:msg :respawn})

(defn keepalive [id]
  {:msg :keepalive :id id})

(defn disconnect [text]
  {:msg :disconnect :text text})

(defn close []
  {:msg :close})

(defn block-ack [sequence]
  {:msg :block-ack :sequence sequence})

(defn set-slot [slot stack]
  {:msg :set-slot :slot slot :stack stack})

(defn carried
  "The stack on the cursor of the inventory screen."
  [stack]
  {:msg :carried :stack stack})

(defn held-slot [slot]
  {:msg :held-slot :slot slot})

(defn inventory
  ([slots] (inventory slots nil))
  ([slots carried] {:msg :inventory :slots slots :carried carried}))

(defn suggestions
  "Completions of the last word of what the player typed: request id, where
   the word starts and how long it is, the matches."
  [id start length matches]
  {:msg :suggestions :id id :start start :length length :matches (vec matches)})

(defn system-chat [runs]
  {:msg :system-chat :runs runs})

(defn stats
  "Statistics of the player for the statistics screen: {[type key] count}."
  [stats]
  {:msg :stats :stats stats})

(defn game-rules
  "All game rule values for the client's rules screen."
  [rules]
  {:msg :game-rules :rules rules})

(defn overlay
  "Message over the hotbar, as vanilla sendOverlayMessage."
  [runs]
  {:msg :overlay :runs runs})

(defn player-chat [name runs]
  {:msg :player-chat :name name :runs runs})

(defn tab-add [entries]
  {:msg :tab-add :entries entries})

(defn tab-remove [uuids]
  {:msg :tab-remove :uuids uuids})

(defn tab-latency [entries]
  {:msg :tab-latency :entries entries})

(defn tab-header [header footer]
  {:msg :tab-header :header header :footer footer})

(defn move [eid dx dy dz on-ground]
  {:msg :move :eid eid :dx dx :dy dy :dz dz :on-ground on-ground})

(defn move-look [eid dx dy dz yaw pitch on-ground]
  {:msg :move-look :eid eid :dx dx :dy dy :dz dz :yaw yaw :pitch pitch :on-ground on-ground})

(defn look [eid yaw pitch on-ground]
  {:msg :look :eid eid :yaw yaw :pitch pitch :on-ground on-ground})

(defn sync-pos [eid pos yaw pitch on-ground]
  {:msg :sync-pos :eid eid :pos pos :yaw yaw :pitch pitch :on-ground on-ground})

(defn head-look [eid yaw]
  {:msg :head-look :eid eid :yaw yaw})

(defn meta [eid meta]
  {:msg :meta :eid eid :meta meta})

(defn velocity [eid vel]
  {:msg :velocity :eid eid :vel vel})

(defn equipment [eid slot stack]
  {:msg :equipment :eid eid :slot slot :stack stack})

(defn animation [eid kind]
  {:msg :animation :eid eid :kind kind})

(defn status [eid kind]
  {:msg :status :eid eid :kind kind})

(defn collect [item-eid collector-eid]
  {:msg :collect :item item-eid :collector collector-eid})

(defn sound [kind pos volume pitch]
  {:msg :sound :kind kind :pos pos :volume (double volume) :pitch (double pitch)})

(defn particles [kind state pos count speed]
  {:msg :particles :kind kind :state state :pos pos :count count :speed (double speed)})

(defn break-effect [pos state]
  {:msg :break-effect :pos pos :state state})

(defn extinguish
  "Fire put out: the sound for everyone, vanilla level event 1009."
  [pos]
  {:msg :extinguish :pos pos})

(defn fizz [pos]
  {:msg :fizz :pos pos})
