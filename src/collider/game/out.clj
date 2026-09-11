(ns collider.game.out
  (:refer-clojure :exclude [time meta]))

(set! *warn-on-reflection* true)

(defn to [eid msg] [:fx (assoc msg :to eid)])
(defn all [msg] [:fx msg])
(defn except [eid msg] [:fx (assoc msg :except eid)])
(defn blocks-changed [cp records]
  {:msg :blocks-changed :cp cp :records records})

(defn time [age time-of-day]
  {:msg :time :age age :time time-of-day})

(defn explosion [center radius blocks motion]
  {:msg :explosion :center center :radius radius :blocks blocks :motion motion})

(defn teleport [pos yaw pitch]
  {:msg :teleport :pos pos :yaw (double yaw) :pitch (double pitch)})

(defn health [health]
  {:msg :health :health (double health)})

(defn respawn []
  {:msg :respawn})

(defn default-spawn [pos]
  {:msg :default-spawn :pos pos})

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
  {:msg :suggestions :id id :start start :length length :matches (vec matches)})

(defn system-chat [runs]
  {:msg :system-chat :runs runs})

(defn stats [stats]
  {:msg :stats :stats stats})

(defn game-rules [rules]
  {:msg :game-rules :rules rules})

(defn overlay [runs]
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

(defn extinguish [pos]
  {:msg :extinguish :pos pos})

(defn fizz [pos]
  {:msg :fizz :pos pos})

(defn level-event
  ([event pos] (level-event event pos 0))
  ([event pos data] {:msg :level-event :event event :pos pos :data data}))

(defn sign-editor [pos front?]
  {:msg :sign-editor :pos pos :front? (boolean front?)})

(defn block-event [pos action param]
  {:msg :block-event :pos pos :action action :param param})

(defn open-screen [container menu title]
  {:msg :open-screen :container container :menu menu :title title})

(defn container-content [container state-id items carried]
  {:msg :container-content :container container :state-id state-id :items (vec items) :carried carried})

(defn container-slot [container state-id slot stack]
  {:msg :container-slot :container container :state-id state-id :slot slot :stack stack})

(defn container-data [container id value]
  {:msg :container-data :container container :id id :value value})

(defn container-close [container]
  {:msg :container-close :container container})

(defn block-entity [pos]
  {:msg :block-entity :pos pos})

(defn bonemeal [pos]
  {:msg :bonemeal :pos pos})
