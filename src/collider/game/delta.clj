(ns collider.game.delta
  (:import (collider.java V3)))

(def Pos "Block position [x y z]." [:tuple :int :int :int])
(defn- vec3? [v]
  (or (instance? V3 v)
      (and (sequential? v) (= 3 (count v)) (every? number? v))))
(def Vec3 "Vector in blocks: [x y z] of doubles or a primitive V3." [:fn vec3?])
(def Eid "Entity id. Players start at 1, other entities at 1000000." :int)
(def State "Global block state id of 26.2." :int)
(def Stack "Item stack." [:map [:item :keyword] [:count :int]])
(def Records "Batch of blocks [[pos state] ...]." [:sequential [:tuple Pos State]])
(def Coll "Any collection (int-set, vector, list)." [:fn coll?])
(def Runs "Chat text: strings or translate/with maps." [:sequential [:or :string :map]])
(def world-deltas
  {:set-blocks
   [[:cat Records]
    "Write blocks and update neighbors. Clients get one batch per chunk at the
     end of the tick (ChunkHolder.broadcastChanges)."]
   :ticks-flushed
   [[:cat :int Coll]
    "Remove block ticks up to t. Parked ticks (inactive chunks) stay and run
     when the chunk becomes active."]
   :schedule-ticks
   [[:cat [:map-of :int Coll]]
    "Tick cells again without a block change (scheduleTick, fire):
     {tick [block-id ...]}."]
   :block-events-flushed
   [[:cat]
    "Clear the sent block event queue."]
   :set-time
   [[:cat :int]
    "Time of day (/time command, sleep)."]
   :set-rule
   [[:cat :keyword :any]
    "Game rule: a key from game/rules and a value."]
   :set-world-spawn
   [[:cat Pos]
    "World spawn point (/setworldspawn)."]
   :set-weather
   [[:cat :map]
    "Weather fields of the world (timers, flags and levels) after
     ServerLevel.advanceWeatherCycle or /weather."]
   :set-block-entity
   [[:cat Pos [:maybe :map]]
    "Block entity at pos (a sign), nil removes it."]
   :spawn-entity
   [[:cat :map]
    "New entity from a map (entity/of). The world gives the eid."]
   :remove-entity
   [[:cat Eid]
    "Remove an entity. A removed player leaves the server."]
   :listed
   [[:cat [:map-of Eid :uuid] Coll]
    "Tab list (:listed of the world): add {eid uuid}, remove eids."]})

(def entity-deltas
  {:merge-entity
   [[:cat :map]
    "Merge fields into the entity."]
   :teleport
   [[:cat Vec3]
    "Move the player and wait for teleport-ack. Sets :pos, :tp-target and
     :tp-id (the tick). Retry after 20 ticks."]
   :client-slots
   [[:cat [:map-of :int [:maybe Stack]] [:maybe Stack]]
    "Slots and cursor set by the client (copy of remoteSlots in Track)."]
   :track
   [[:cat :map]
    "What clients know about the entity (Track: pos, yaw, mdata, equip,
     vel-sent, slots, carried, :seen). The player system sends the difference."]
   :tracking
   [[:cat Coll Coll]
    "Entities that this player sees: add eids, remove eids. render sends
     spawn and removal from this delta."]
   :set-slot
   [[:cat :int [:maybe Stack]]
    "Inventory slot: a stack or nil (empty)."]
   :chunks-sent
   [[:cat Coll Coll]
    "Chunks of the player: added and removed ids. render sends the chunks."]
   :damage
   [[:cat number? [:? [:cat number? number?]]]
    "Damage: amount and optional knockback direction dx dz."]
   :push
   [[:cat Vec3]
    "Add velocity (for TNT, the knockback :kb)."]})

(def fx-messages
  {:blocks-changed [[[:cp :int] [:records Records]] "Changed blocks of a chunk. With :to it is a correction for one player."]
   :break-effect   [[[:pos Pos] [:state State]] "Particles and sound of a block break."]
   :explosion      [[[:center Vec3] [:radius number?] [:blocks :int] [:motion Vec3]] "Explosion: center, radius, count of removed blocks, push for the receiver."]
   :sound          [[[:kind :keyword] [:pos Vec3] [:volume number?] [:pitch number?]] "Sound at a point."]
   :particles      [[[:kind :keyword] [:state [:maybe State]] [:pos Vec3] [:count :int] [:speed number?]] "Particles."]
   :extinguish     [[[:pos Pos]] "Fire is out (level event 1009)."]
   :fizz           [[[:pos Pos]] "Fizz: lava with water, fire in water."]
   :bonemeal       [[[:pos Pos]] "Bone meal took: green sparks, level event 1505."]
   :level-event    [[[:event :int] [:pos Pos] [:data :int]] "A level event at a block with its data: wax on/off, scrape, composter fill."]
   :sign-editor    [[[:pos Pos] [:front? :boolean]] "Open the sign editor of one side (to the player)."]
   :block-event    [[[:pos Pos] [:action :int] [:param :int]] "Block event at a block: pot wobble, bell ring."]
   :block-entity   [[[:pos Pos]] "Block entity data at pos changed: sent from the world after the tick."]
   :time           [[[:age :int] [:time :int]] "Clock: world age and time of day after the tick."]
   :teleport       [[[:pos Vec3] [:yaw number?] [:pitch number?]] "Put the player here (we wait for teleport-ack)."]
   :health         [[[:health number?]] "Player health."]
   :respawn        [[] "Respawn the player."]
   :default-spawn  [[[:pos Pos]] "World spawn point shown by the compass."]
   :rain-started   [[] "Rain begins (game event 1)."]
   :rain-stopped   [[] "Rain ends (game event 2)."]
   :rain-level     [[[:level number?]] "Rain level 0..1 (game event 7)."]
   :thunder-level  [[[:level number?]] "Thunder level 0..1 (game event 8)."]
   :keepalive      [[[:id :int]] "Ping."]
   :disconnect     [[[:text [:or :string :map]]] "Disconnect with a text (string or translate)."]
   :close          [[] "Close the connection."]
   :block-ack      [[[:sequence :int]] "Confirm the block changes of the client up to sequence."]
   :set-slot       [[[:slot :int] [:stack [:maybe Stack]]] "Inventory slot for the client."]
   :carried        [[[:stack [:maybe Stack]]] "Stack on the cursor."]
   :held-slot      [[[:slot :int]] "Selected hotbar slot."]
   :inventory      [[[:slots [:sequential :any]] [:carried [:maybe Stack]]] "Full inventory."]
   :suggestions    [[[:id :int] [:start :int] [:length :int] [:matches [:sequential :any]]] "Command suggestions."]
   :system-chat    [[[:runs Runs]] "System message to the chat."]
   :player-chat    [[[:name :string] [:runs Runs]] "Player message."]
   :overlay        [[[:runs Runs]] "Text above the hotbar."]
   :stats          [[[:stats :map]] "Statistics for the client screen."]
   :game-rules     [[[:rules :map]] "Game rules for the client screen."]
   :tab-add        [[[:entries [:sequential :map]]] "Add to the tab list."]
   :tab-remove     [[[:uuids [:sequential :uuid]]] "Remove from the tab list."]
   :tab-latency    [[[:entries [:sequential :map]]] "Pings in the tab list."]
   :tab-header     [[[:header :string] [:footer :string]] "Header and footer of the tab list."]
   :move           [[[:eid Eid] [:dx :int] [:dy :int] [:dz :int] [:on-ground :boolean]] "Entity moved (in 1/4096 of a block)."]
   :move-look      [[[:eid Eid] [:dx :int] [:dy :int] [:dz :int] [:yaw :int] [:pitch :int] [:on-ground :boolean]] "Entity moved and turned."]
   :look           [[[:eid Eid] [:yaw :int] [:pitch :int] [:on-ground :boolean]] "Entity turned."]
   :sync-pos       [[[:eid Eid] [:pos Vec3] [:yaw number?] [:pitch number?] [:on-ground :boolean]] "Absolute position."]
   :head-look      [[[:eid Eid] [:yaw number?]] "Head turn."]
   :meta           [[[:eid Eid] [:meta :map]] "Metadata: on fire, sneaking, sprinting, skin."]
   :velocity       [[[:eid Eid] [:vel Vec3]] "Entity velocity."]
   :equipment      [[[:eid Eid] [:slot :int] [:stack [:maybe Stack]]] "Equipment: slot 0..5 (main hand, off hand, boots ... helmet)."]
   :animation      [[[:eid Eid] [:kind :keyword]] "Animation: swing, wake up."]
   :status         [[[:eid Eid] [:kind :keyword]] "Entity event: hurt, death, shear."]
   :collect        [[[:item Eid] [:collector Eid]] "Item collected."]
   :open-screen    [[[:container :int] [:menu :keyword] [:title :map]] "Open a container screen: id, menu type, title."]
   :container-content [[[:container :int] [:state-id :int] [:items [:sequential [:maybe Stack]]] [:carried [:maybe Stack]]] "Full contents of an open menu."]
   :container-slot [[[:container :int] [:state-id :int] [:slot :int] [:stack [:maybe Stack]]] "One slot of an open menu."]
   :container-close [[[:container :int]] "Close the open menu on the client."]})

(defn- with-address [fields]
  (into [:map [:msg :keyword] [:to {:optional true} Eid] [:except {:optional true} Eid]] fields))

(def Fx
  (into [:multi {:dispatch :msg}]
        (for [[msg [fields _]] fx-messages] [msg (with-address fields)])))

(def Delta
  (into [:multi {:dispatch first}]
        (concat (for [[tag [args _]] world-deltas] [tag (into [:cat [:= tag]] (rest args))])
                (for [[tag [args _]] entity-deltas] [tag (into [:cat [:= tag] Eid] (rest args))])
                [[:fx [:cat [:= :fx] Fx]]])))

(def ^:private delta-validator (delay ((requiring-resolve 'malli.core/validator) Delta)))
(def ^:private delta-explainer (delay ((requiring-resolve 'malli.core/explainer) Delta)))
(defn valid? [delta] (@delta-validator delta))
(defn explain [delta]
  (when-let [e (@delta-explainer delta)]
    ((requiring-resolve 'malli.error/humanize) e)))

(def validate?
  false)

(defn check! [deltas]
  (doseq [d deltas]
    (when-not (@delta-validator d)
      (throw (ex-info (str "invalid delta " (first d)) {:delta d :why (explain d)}))))
  deltas)

(defn describe [tag]
  (or (second (world-deltas tag)) (second (entity-deltas tag))
      (second (fx-messages tag))))
