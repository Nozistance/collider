(ns collider.net.render
  "Packets for each player from the tick."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.menu :as menu]
            [collider.game.entity :as entity]
            [clojure.data.int-map :as i]
            [collider.config :as config]
            [collider.data :as data]
            [collider.game.command.tree :as commands]
            [collider.game.deltas :as deltas]
            [collider.game.schema :as schema]
            [collider.game.state :as state]
            [collider.game.gamerules :as rules]
            [collider.log :as log]
            [collider.proto.entitydata :as ed]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.biome :as biome])
  (:import (collider.game.deltas Deltas)))

(set! *warn-on-reflection* true)

(defn- block-state ^long [world pos]
  (chunk/chunks-get-block (:chunks world) pos))

(defn- players [world]
  (vec (sort (vals (:players world)))))

(def ^:private chunk-level-keys [:min-y :max-y :sky? :dim :chunks])

(defn- chunk-packet [world id]
  (let [[x z] (chunk/id->pos id)]
    {:packet         :level-chunk-with-light :cx x :cz z
     :chunk          (get-in world [:chunks id] chunk/empty-chunk)
     :block-entities (be/wire (get-in world [:block-entities id]))
     :level          (select-keys world chunk-level-keys)}))

(defn- forget-chunk-packet [id]
  (let [[x z] (chunk/id->pos id)]
    {:packet :forget-level-chunk :cx x :cz z}))

(defn- added-chunk-packets [world add]
  (when (seq add)
    (concat
      [{:packet :chunk-batch-start}]
      (map #(chunk-packet world %) add)
      [{:packet :chunk-batch-finished :size (count add)}])))

(defn- center-packet [id]
  (let [[cx cz] (chunk/id->pos id)]
    {:packet :set-chunk-cache-center :cx cx :cz cz}))

(defn- chunk-packets
  "Returns the packets of one step of chunk sending. The view
  center goes only when it moved."
  [world [_ _ add drop center]]
  (concat
    (when center [(center-packet center)])
    (added-chunk-packets world add)
    (map forget-chunk-packet drop)))

(def ^:private ^:table entity-type
  (delay
    {:player        (data/registry-id "entity_type" :player)
     :sheep         (data/registry-id "entity_type" :sheep)
     :cow           (data/registry-id "entity_type" :cow)
     :mooshroom     (data/registry-id "entity_type" :mooshroom)
     :item          (data/registry-id "entity_type" :item)
     :tnt           (data/registry-id "entity_type" :tnt)
     :falling-block (data/registry-id "entity_type" :falling-block)
     :snowball      (data/registry-id "entity_type" :snowball)
     :egg           (data/registry-id "entity_type" :egg)
     :ender-pearl   (data/registry-id "entity_type" :ender-pearl)
     :splash-potion (data/registry-id "entity_type" :splash-potion)
     :lingering-potion
     (data/registry-id "entity_type" :lingering-potion)
     :area-effect-cloud
     (data/registry-id "entity_type" :area-effect-cloud)}))

(def ^:private ^:table entity-effect-particle
  (delay (data/registry-id "particle_type" :entity-effect)))

(defn- kind-of [e]
  (let [t (:type e)]
    (if (contains? @entity-type t) t :player)))

(defn- flags-byte [meta]
  (bit-or (if (:burning? meta) 0x01 0)
          (if (:sneaking? meta) 0x02 0)
          (if (:sprinting? meta) 0x08 0)
          (if (:swimming? meta) 0x10 0)))

(def ^:private flag-keys
  [:burning? :sneaking? :sprinting? :swimming?])

(defn- flags? [meta] (boolean (some #(contains? meta %) flag-keys)))

(def ^:private pose-id
  {:standing 0 :sleeping 2 :swimming 3 :crouching 5})

(defn- using-item-byte ^long [meta]
  (case (:using-item? meta) :off 0x03 (nil false) 0 0x01))

(defn- common-fields [meta]
  (cond-> {} (flags? meta) (assoc :shared-flags (flags-byte meta))))

(defn- player-fields [meta]
  (cond-> (common-fields meta)
          (contains? meta :pose)
          (assoc :pose (pose-id (:pose meta) 0))
          (contains? meta :using-item?)
          (assoc :living-flags (using-item-byte meta))
          (contains? meta :sleeping-pos)
          (assoc :sleeping-pos (:sleeping-pos meta))))

(defn- color-byte ^long [meta]
  (bit-or (bit-and (long (or (:color meta) 0)) 15)
          (if (:sheared? meta) 0x10 0)))

(defn- coat-id ^long [v] (data/datapack-id "cow_variant" v))

(defn- voice-id ^long [v]
  (data/datapack-id "cow_sound_variant" v))

(defn- animal-fields [meta]
  (cond-> (common-fields meta)
          (contains? meta :baby?)
          (assoc :baby (boolean (:baby? meta)))))

(defn- cow-fields [meta]
  (cond-> (animal-fields meta)
          (contains? meta :cow-variant)
          (assoc :variant (coat-id (:cow-variant meta)))
          (contains? meta :cow-sound)
          (assoc :sound-variant (voice-id (:cow-sound meta)))))

(defn- stack-fields [meta]
  (cond-> {} (contains? meta :stack) (assoc :item (:stack meta))))

(defn- tnt-fields [meta]
  (let [f (:fuse meta)]
    (if (and f (not= f (ed/default :primed-tnt :fuse)))
      {:fuse f}
      {})))

(defn- cloud-fields [meta]
  (cond-> {}
          (contains? meta :radius) (assoc :radius (:radius meta))
          (contains? meta :waiting?) (assoc :waiting (:waiting? meta))
          (contains? meta :color)
          (assoc :particle [@entity-effect-particle (:color meta)])))

(def ^:private entity-class
  {:player :player :sheep :sheep :cow :cow :mooshroom :mushroom-cow
   :item :item-entity :tnt :primed-tnt :falling-block :falling-block
   :area-effect-cloud :area-effect-cloud})

(defn- class-of [kind]
  (or (entity-class kind)
      (when (entity/thrown-types kind) :throwable-item-projectile)))

(defn- sheep-fields [meta]
  (cond-> (animal-fields meta)
    (contains? meta :color) (assoc :wool (color-byte meta))))

(defn- mooshroom-fields [meta]
  (cond-> (animal-fields meta)
    (contains? meta :variant) (assoc :type (long (:variant meta)))))

(defn- falling-fields [meta]
  (cond-> {}
    (contains? meta :start) (assoc :start-pos (:start meta))))

(defn- entity-fields [kind meta]
  (case kind
    :player (player-fields meta)
    :cow (cow-fields meta)
    :sheep (sheep-fields meta)
    :mooshroom (mooshroom-fields meta)
    :item (merge (common-fields meta) (stack-fields meta))
    :tnt (tnt-fields meta)
    :falling-block (falling-fields meta)
    :area-effect-cloud (cloud-fields meta)
    (stack-fields meta)))

(defn- entity-data [kind meta]
  (if-let [cls (class-of kind)]
    (ed/entries cls (entity-fields kind meta))
    []))

(def ^:private equipment-slots [0 2 3 4 5])

(defn- spawn-rotation
  "Returns in degrees the angle a thrown thing began with.
  It flies in the tick of the throw, so the entity already turned.
  Everything else answers with the angle it holds now."
  [tr kind k now]
  (if-let [a (and tr (entity/thrown-types kind) (get tr k))]
    (/ (* (double a) 360.0) 256.0)
    now))

(defn- add-entity-packet [eid e tr kind]
  {:packet :add-entity :eid eid :uuid (entity/uuid-of eid e)
   :type   (@entity-type kind)
   :pos    (if tr (mapv double (:pos tr)) (:pos e))
   :vel    (or (when tr (:vel-sent tr)) (:vel e) [0.0 0.0 0.0])
   :yaw    (spawn-rotation tr kind :yaw (:yaw e 0.0))
   :pitch  (spawn-rotation tr kind :pitch (:pitch e 0.0))
   :head-yaw (if (entity/thrown-types kind)
               0.0
               (or (:head-yaw e) (:yaw e 0.0)))
   :data   (cond (= :falling-block kind) (:block e)
                 (entity/thrown-types kind) (long (:owner e 0))
                 :else 0)})

(defn- equipment-of [tr]
  (let [equip (if tr (:equip tr) [])]
    (keep-indexed (fn [i s] (when s [(equipment-slots i) s]))
                  equip)))

(defn- spawn-packets [world eid]
  (when-let [e (get-in world [:entities eid])]
    (let [kind (kind-of e)
          tr (:track e)
          d (entity-data kind (if tr (:mdata tr) {}))
          equip (equipment-of tr)]
      (concat
        [{:packet :bundle-delimiter}
         (add-entity-packet eid e tr kind)]
        (when (seq d) [{:packet :set-entity-data :eid eid :data d}])
        (when (seq equip)
          [{:packet :set-equipment :eid eid :slots equip}])
        [{:packet :bundle-delimiter}]))))

(defn- tracking-packets [world [_ _ add _]]
  (mapcat #(spawn-packets world %) add))

(defn- forget-packets [deltas]
  (for [[eid ds] deltas
        [tag _ _ gone] ds
        :when (and (= :tracking tag) (seq gone))]
    [eid {:packet :remove-entities :eids gone}]))

(def sound-table
  {:player/hurt                   [:entity.player.hurt 7]
   :player/hurt-on-fire           [:entity.player.hurt-on-fire 7]
   :player/death                  [:entity.player.death 7]
   :sheep/say                     [:entity.sheep.ambient 6]
   :sheep/step                    [:entity.sheep.step 6]
   :sheep/hurt                    [:entity.sheep.hurt 6]
   :sheep/death                   [:entity.sheep.death 6]
   :sheep/shear                   [:entity.sheep.shear 7]
   :cow/say                       [:entity.cow.ambient 6]
   :cow/step                      [:entity.cow.step 6]
   :cow/hurt                      [:entity.cow.hurt 6]
   :cow/death                     [:entity.cow.death 6]
   :cow/milk                      [:entity.cow.milk 7]
   :cow-moody/say                 [:entity.cow-moody.ambient 6]
   :cow-moody/step                [:entity.cow-moody.step 6]
   :cow-moody/hurt                [:entity.cow-moody.hurt 6]
   :cow-moody/death               [:entity.cow-moody.death 6]
   :mooshroom/milk                [:entity.mooshroom.milk 6]
   :mooshroom/suspicious
   [:entity.mooshroom.suspicious-milk 6]
   :mooshroom/shear               [:entity.mooshroom.shear 7]
   :mooshroom/eat                 [:entity.mooshroom.eat 6]
   :mooshroom/convert             [:entity.mooshroom.convert 6]
   :tnt/primed                    [:entity.tnt.primed 4]
   :snowball/throw                [:entity.snowball.throw 6]
   :egg/throw                     [:entity.egg.throw 7]
   :ender-pearl/throw             [:entity.ender-pearl.throw 6]
   :splash-potion/throw           [:entity.splash-potion.throw 7]
   :lingering-potion/throw        [:entity.lingering-potion.throw 7]
   :player/teleport               [:entity.player.teleport 7]
   :hoe/till                      [:item.hoe.till 4]
   :candle/extinguish             [:block.candle.extinguish 4]
   :eyeblossom/open               [:block.eyeblossom.open 4]
   :eyeblossom/close              [:block.eyeblossom.close 4]
   :eyeblossom/open-long          [:block.eyeblossom.open-long 4]
   :eyeblossom/close-long         [:block.eyeblossom.close-long 4]
   :cake/add-candle               [:block.cake.add-candle 4]
   :cave-vines/pick-berries       [:block.cave-vines.pick-berries 4]
   :big-dripleaf/tilt-down        [:block.big-dripleaf.tilt-down 4]
   :big-dripleaf/tilt-up          [:block.big-dripleaf.tilt-up 4]
   :sweet-berry-bush/pick-berries
   [:block.sweet-berry-bush.pick-berries 4]
   :bottle/fill                   [:item.bottle.fill 4]
   :bottle/empty                  [:item.bottle.empty 4]
   :copper-golem/statue
   [:entity.copper-golem-become-statue 4]
   :axe/strip                     [:item.axe.strip 4]
   :axe/scrape                    [:item.axe.scrape 4]
   :axe/wax-off                   [:item.axe.wax-off 4]
   :honeycomb/wax-on              [:item.honeycomb.wax-on 4]
   :dye/use                       [:item.dye.use 4]
   :glow-ink/use                  [:item.glow-ink-sac.use 4]
   :ink-sac/use                   [:item.ink-sac.use 4]
   :sign/waxed                    [:block.sign.waxed-interact-fail 4]
   :decorated-pot/insert          [:block.decorated-pot.insert 4]
   :decorated-pot/insert-fail     [:block.decorated-pot.insert-fail 4]
   :shelf/place-item              [:block.shelf.place-item 4]
   :shelf/single-swap             [:block.shelf.single-swap 4]
   :shelf/take-item               [:block.shelf.take-item 4]
   :bookshelf/insert              [:block.chiseled-bookshelf.insert 4]
   :bookshelf/insert-enchanted
   [:block.chiseled-bookshelf.insert.enchanted 4]
   :bookshelf/pickup              [:block.chiseled-bookshelf.pickup 4]
   :bookshelf/pickup-enchanted
   [:block.chiseled-bookshelf.pickup.enchanted 4]
   :bell/use                      [:block.bell.use 4]
   :bucket/empty                  [:item.bucket.empty 4]
   :bucket/fill                   [:item.bucket.fill 4]
   :bucket/empty-lava             [:item.bucket.empty-lava 4]
   :bucket/fill-lava              [:item.bucket.fill-lava 4]
   :bucket/empty-snow             [:item.bucket.empty-powder-snow 4]
   :bucket/fill-snow              [:item.bucket.fill-powder-snow 4]
   :pumpkin/carve                 [:block.pumpkin.carve 4]
   :composter/fill                [:block.composter.fill 4]
   :composter/fill-success        [:block.composter.fill-success 4]
   :composter/ready               [:block.composter.ready 4]
   :composter/empty               [:block.composter.empty 4]
   :shovel/flatten                [:item.shovel.flatten 4]
   :fire/ignite                   [:item.flintandsteel.use 4]
   :firecharge/use                [:item.firecharge.use 4]
   :generic/extinguish-fire       [:entity.generic.extinguish-fire 4]
   :wet-sponge/dries              [:block.wet-sponge.dries 4]
   :generic/burn                  [:entity.generic.burn 8]
   :explosion                     [:entity.generic.explode 4]
   :splash                        [:entity.generic.splash 6]
   :swim                          [:entity.generic.swim 6]})

(defn- spawn-info
  "Returns the fields a join or respawn gives of level lv.
  ServerPlayer.createCommonSpawnInfo."
  [lv]
  (let [dim (:dim lv :overworld)]
    {:dimension-type (data/datapack-id "dimension_type" dim)
     :dimension dim :sea-level (biome/sea-level-of dim)}))

(def ^:private ^:table explosion-block-particles
  (delay [[(data/registry-id "particle_type" :poof) 0.5 1.0 1]
          [(data/registry-id "particle_type" :smoke) 1.0 1.0 1]]))

(def ^:private ^:table explosion-particle
  (delay (data/registry-id "particle_type" :explosion-emitter)))

(defn- particles-packet [m]
  {:packet   :level-particles
   :particle (data/registry-id "particle_type" (:kind m))
   :state    (:state m)
   :pos      (:pos m) :count (:count m) :speed (:speed m)})

(def ^:private unhandled (atom #{}))

(defn- once! [kind]
  (when-not (@unhandled kind)
    (swap! unhandled conj kind)
    (log/warn "render:" kind "not rendered yet")))

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

(defn- status-packet [m]
  (case (:kind m)
    :hurt {:packet :hurt-animation :eid (:eid m) :yaw 0.0}
    :death {:packet :entity-event :eid (:eid m) :event 3}
    :break {:packet :entity-event :eid (:eid m) :event 3}
    :eat {:packet :entity-event :eid (:eid m) :event 10}
    :break-main {:packet :entity-event :eid (:eid m) :event 47}
    :break-off {:packet :entity-event :eid (:eid m) :event 48}
    :love {:packet :entity-event :eid (:eid m) :event 18}
    nil))

(defn- sound-id [kind]
  (let [reg (get (data/registries) "sound_event")]
    (if-let [[ev src] (get sound-table kind)]
      (when-let [id (get reg ev)] [id src])
      (when-let [id (get reg kind)] [id 4]))))

(def ^:private sound-sources {:blocks 4 :neutral 6 :players 7})

(defn- sound-packet [m]
  (if-let [[id src] (sound-id (:kind m))]
    {:packet :sound :sound id :pos (:pos m) :seed 0
     :volume (:volume m) :pitch (:pitch m)
     :source (get sound-sources (:source m) src)}
    (once! [:sound (:kind m)])))

(defn- explode-packet [m eid]
  (let [k (get (:motions m) eid)]
    {:packet    :explode :center (:center m) :radius (:radius m)
     :blocks    (:blocks m)
     :knockback (when (and k (some #(not (zero? (double %))) k)) k)
     :particle  @explosion-particle
     :sound     (first (sound-id :explosion))
     :block-particles @explosion-block-particles}))

(defn- dist-sq [a b]
  (let [dx (- (v/x a) (v/x b))
        dy (- (v/y a) (v/y b))
        dz (- (v/z a) (v/z b))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(def ^:private ^:const explosion-range-sq 4096.0)

(defn- in-earshot? [world center eid]
  (when-let [p (get-in world [:entities eid :pos])]
    (< (dist-sq p center) explosion-range-sq)))

(def ^:private ^:const level-range-sq (* 64.0 64.0))

(defn- in-level-range? [world center eid]
  (when-let [p (get-in world [:entities eid :pos])]
    (< (dist-sq p center) level-range-sq)))

(defn- sound-range-sq [volume]
  (let [v (double volume)
        r (if (> v 1.0) (* 16.0 v) 16.0)]
    (* r r)))

(defn- in-sound-range? [world center volume eid]
  (when-let [p (get-in world [:entities eid :pos])]
    (< (dist-sq p center) (sound-range-sq volume))))

(def ^:private ^:const particle-range-sq (* 32.0 32.0))

(defn- block-center [pos]
  [(+ (Math/floor (v/x pos)) 0.5)
   (+ (Math/floor (v/y pos)) 0.5)
   (+ (Math/floor (v/z pos)) 0.5)])

(defn- in-particle-range? [world center eid]
  (when-let [p (get-in world [:entities eid :pos])]
    (< (dist-sq (block-center p) center) particle-range-sq)))

(defn- chunk-of-msg [m]
  (case (:msg m)
    :blocks-changed (:cp m)
    :block-entity (chunk/block-chunk (:pos m))))

(defn- tracking-chunk? [world cp eid]
  (contains? (get-in world [:entities eid :sent-chunks]) cp))

(defn- ranged-recipients [world ps m]
  (case (:msg m)
    (:blocks-changed :block-entity)
    (let [cp (chunk-of-msg m)]
      (filterv #(tracking-chunk? world cp %) ps))
    (:level-event :break-effect :fizz :bonemeal :extinguish
     :block-event)
    (filterv #(in-level-range? world (:pos m) %) ps)
    :sound
    (filterv #(in-sound-range? world (:pos m) (:volume m) %) ps)
    :particles
    (filterv #(in-particle-range? world (:pos m) %) ps)
    :explosion
    (filterv #(in-earshot? world (:center m) %) ps)
    ps))

(defn- rule-pair [[k v]]
  [(rules/wire-name k) (rules/serialize k v)])

(def ^:private difficulty-packet
  {:packet :change-difficulty :difficulty 0 :locked false})

(def ^:private abilities-packet
  {:packet       :player-abilities :flags (bit-or 1 4 8)
   :flying-speed 0.05 :walking-speed 0.1})

(def ^:private ^:table command-tree (delay (commands/tree)))

(def ^:private world-border-size 5.9999968E7)

(def ^:private world-border-max 29999984)

(def ^:private op-level-event 24)

(defn- join-spawn [world]
  (let [[x y z] (state/respawn-at world)]
    [(long (Math/floor (double x)))
     (long (Math/floor (double y)))
     (long (Math/floor (double z)))]))

(defn- permission-packets
  "PlayerList.sendPlayerPermissionLevel: the op level and the
  commands it allows."
  [eid]
  [{:packet :entity-event :eid eid :event (+ op-level-event 4)}
   {:packet :commands :nodes @command-tree}])

(def ^:private border-packet
  {:packet     :initialize-border :center-x 0.0 :center-z 0.0
   :old-size   world-border-size :size world-border-size
   :max-size   world-border-max :warning-blocks 5
   :warning-time 300})

(defn- spawn-pos-packet [world]
  (let [[yaw pitch] (state/spawn-turn world)]
    {:packet :set-default-spawn-position
     :dimension (:world-spawn-dimension world :overworld)
     :pos (join-spawn world) :yaw yaw :pitch pitch}))

(def ^:private ticking-packets
  [{:packet :ticking-state :rate 20.0 :frozen? false}
   {:packet :ticking-step :steps 0}])

(defn- set-time-packet [m]
  {:packet :set-time :age (:age m) :time (:time m)})

(defn- rain-packets
  "The weather part of PlayerList.sendLevelInfo for level lv."
  [lv]
  (let [rain (double (:rain-level lv 0.0))
        thunder (* rain (double (:thunder-level lv 0.0)))]
    (when (> rain 0.2)
      [{:packet :game-event :event 1 :value 0.0}
       {:packet :game-event :event 7 :value rain}
       {:packet :game-event :event 8 :value thunder}])))

(defn- level-info-packets
  "PlayerList.sendLevelInfo: what a player entering level lv learns
  of it."
  [lv]
  (concat [border-packet
           (set-time-packet
             {:age (:tick lv) :time (:time-of-day lv 0)})
           (spawn-pos-packet lv)]
          (rain-packets lv)
          [{:packet :game-event :event 13 :value 0.0}]
          ticking-packets))

(defn- leave-packets
  "What the old level sends on the way out: the chunks and then the
  entities the player knew there."
  [m]
  (concat (map forget-chunk-packet (:forget m))
          (map (fn [id] {:packet :remove-entities :eids [id]})
               (:untrack m))))

(defn- player-info-packets
  "The inventory and held slot a player entering a level learns."
  [e]
  (let [inv (or (:inventory e) {})]
    [{:packet :container-set-content :container 0 :state-id 0
      :items (mapv inv (range menu/slot-count)) :carried (:carried e)}
     {:packet :set-held-slot :slot (long (or (:held-slot e) 0))}]))

(defn- resent-packets
  "The health and experience a player entering a level learns once
  the levels have ticked."
  [e]
  [{:packet :set-health :health (double (:health e 20.0))
    :food 20 :saturation 5.0}
   {:packet :set-experience :progress 0.0 :level 0 :total 0}])

(defn- arrival-packets [m]
  [{:packet :player-position :teleport-id 0
    :pos (:pos m) :vel [0.0 0.0 0.0] :yaw (:yaw m)
    :pitch (:pitch m) :relative (:relative m 0)}
   (center-packet (chunk/pos-chunk (:pos m)))
   abilities-packet])

(defn- change-dimension-packets
  "Returns the packets that move a player into level lv, in the
  order the client expects them."
  [lv m]
  (let [eid (:to m)]
    (concat
      [(assoc (spawn-info lv) :packet :respawn :keep 3)
       difficulty-packet]
      (permission-packets eid)
      (leave-packets m)
      (arrival-packets m)
      (level-info-packets lv)
      (player-info-packets (get-in lv [:entities eid])))))

(def ^:private session-fx
  {:teleport      (fn [_ m]
                    [{:packet :player-position :teleport-id 0
                      :pos (:pos m) :vel [0.0 0.0 0.0]
                      :yaw (:yaw m) :pitch (:pitch m)
                      :relative (:relative m 0)}])
   :keepalive     (fn [_ m] [{:packet :keep-alive :id (:id m)}])
   :disconnect    (fn [_ m] [{:packet :disconnect :text (:text m)}])
   :system-chat   (fn [_ m]
                    [{:packet :system-chat :overlay false
                      :text (:text m)}])
   :overlay       (fn [_ m]
                    [{:packet :system-chat :overlay true
                      :text (:text m)}])
   :player-chat   (fn [_ m]
                    [{:packet :system-chat :overlay false
                      :text (:text m)}])
   :stats         (fn [_ m]
                    [{:packet :award-stats :stats (:stats m)}])
   :suggestions   (fn [_ m]
                    [{:packet :command-suggestions :id (:id m)
                      :start (:start m) :length (:length m)
                      :matches (:matches m)}])
   :game-rules    (fn [_ m]
                    [{:packet :game-rule-values
                      :values (into {} (map rule-pair) (:rules m))}])
   :health        (fn [_ m]
                    [{:packet :set-health :health (:health m)
                      :food 20 :saturation 5.0}])
   :cooldown      (fn [_ m]
                    [{:packet :cooldown :group (:group m)
                      :duration (:ticks m)}])
   :change-dimension change-dimension-packets
   :respawn       (fn [lv _]
                    [(assoc (spawn-info lv) :packet :respawn :keep 0)
                     {:packet :game-event :event 13 :value 0.0}])
   :default-spawn (fn [_ m]
                    [{:packet :set-default-spawn-position
                      :dimension (:dimension m) :pos (:pos m)
                      :yaw (:yaw m 0.0) :pitch (:pitch m 0.0)}])
   :joined        (fn [_ _] nil)
   :close         (fn [_ _] [:close])})

(defn- event-block [world pos]
  (data/registry-id "block" (block/block-of (block-state world pos))))

(defn- block-entity-packet [pos e]
  {:packet :block-entity-data :pos pos
   :type (be/type-id e) :nbt (be/nbt e)})

(defn- block-entity-fx [world m]
  (when-let [e (be/at world (:pos m))]
    (when (be/on-wire? e)
      [(block-entity-packet (:pos m) e)])))

(defn- game-event-packet [event value]
  {:packet :game-event :event event :value value})

(defn- level-event-packet [event pos data]
  {:packet :level-event :event event :pos pos :data data})

(defn- sign-editor-packet [m]
  {:packet :open-sign-editor :pos (:pos m) :front? (:front? m)})

(def ^:private world-fx
  {:rain-started   (fn [_ _] [(game-event-packet 1 0.0)])
   :rain-stopped   (fn [_ _] [(game-event-packet 2 0.0)])
   :rain-level     (fn [_ m] [(game-event-packet 7 (:level m))])
   :thunder-level  (fn [_ m] [(game-event-packet 8 (:level m))])
   :time           (fn [_ m] [(set-time-packet m)])
   :blocks-changed (fn [_ m]
                     (let [cp (chunk/id->pos (:cp m))]
                       (block-records cp (:records m))))
   :break-effect   (fn [_ m]
                     [(level-event-packet 2001 (:pos m) (:state m))])
   :fizz           (fn [_ m] [(level-event-packet 1501 (:pos m) 0)])
   :bonemeal       (fn [_ m] [(level-event-packet 1505 (:pos m) 15)])
   :extinguish     (fn [_ m] [(level-event-packet 1009 (:pos m) 0)])
   :level-event    (fn [_ m]
                     [(level-event-packet
                        (:event m) (:pos m) (:data m 0))])
   :sign-editor    (fn [_ m] [(sign-editor-packet m)])
   :block-event    (fn [world m]
                     [{:packet :block-event :pos (:pos m)
                       :action (:action m) :param (:param m)
                       :block  (event-block world (:pos m))}])
   :block-entity   block-entity-fx
   :sound          (fn [_ m] (when-let [p (sound-packet m)] [p]))
   :particles      (fn [_ m] [(particles-packet m)])
   :explosion      (fn [_ _] nil)
   :load-chunk     (fn [_ _] nil)
   :store-chunk    (fn [_ _] nil)})

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

(defn- tab-add-packet [m]
  {:packet :player-info-update :players (:entries m)})

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
   :tab-latency       (fn [_ m] [(tab-latency-packet m)])
   :tab-header        (fn [_ m] [(tab-header-packet m)])})

(defn- animate-action ^long [kind]
  (case kind :swing 0 :wake-up 2 :swing-off 3 :crit 4 0))

(defn- head-look-packet [m]
  {:packet :rotate-head :eid (:eid m) :yaw (:yaw m)})

(defn- velocity-packet [m]
  {:packet :set-entity-motion :eid (:eid m) :vel (:vel m)})

(defn- collect-packet [m]
  {:packet :take-item-entity :item (:eid m)
   :collector (:collector m) :amount 1})

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
   :meta      (fn [_ m]
                (let [d (entity-data (kind-of m) (:meta m))]
                  (when (seq d)
                    [{:packet :set-entity-data :eid (:eid m)
                      :data d}])))
   :equipment (fn [_ m]
                (let [slot (equipment-slots (:slot m))]
                  [{:packet :set-equipment :eid (:eid m)
                    :slots [[slot (:stack m)]]}]))
   :animation (fn [_ m]
                [{:packet :animate :eid (:eid m)
                  :action (animate-action (:kind m))}])
   :status    (fn [_ m] (when-let [p (status-packet m)] [p]))
   :collect   (fn [_ m] [(collect-packet m)])})

(def ^:private fx-table
  (merge session-fx world-fx container-fx entity-fx))

(defn- fx-packets [world m]
  (if-let [f (fx-table (:msg m))]
    (f world m)
    (once! (:msg m))))

(defn- join-login-packets [cfg lv eid]
  (let [{:keys [max-players view-distance simulation-distance]} cfg]
    [(merge (spawn-info lv)
            {:packet              :login :eid eid
             :levels              schema/dims
             :max-players         (min 255 (long max-players))
             :view-distance       view-distance
             :simulation-distance simulation-distance})
     difficulty-packet
     abilities-packet]))

(defn- join-world-packets [world eid motd]
  (concat
    [(assoc (data/recipes) :packet :update-recipes)]
    (permission-packets eid)
    [{:packet :server-data :motd motd}
     border-packet
     (spawn-pos-packet world)
     {:packet :game-event :event 13 :value 0.0}]
    ticking-packets))

(defn- join-player-packets [eid]
  [{:packet :set-health :health 20.0 :food 20 :saturation 5.0}
   {:packet :set-experience :progress 0.0 :level 0 :total 0}
   {:packet     :update-attributes :eid eid
    :attributes
    [[:entity-interaction-range state/entity-range
      [[:creative-mode-entity-range state/creative-entity-range 0]]]
     [:movement-speed 0.1 []]
     [:block-interaction-range state/block-range
      [[:creative-mode-block-range state/creative-block-range 0]]]]}])

(defn- join-packets [world lv eid]
  (let [cfg (merge config/defaults (:config world))]
    (concat (join-login-packets cfg lv eid)
            (join-world-packets world eid (:motd cfg))
            (join-player-packets eid))))

(declare own-level)

(defn- join-bursts [world sight ^Deltas deltas]
  (for [m (deltas/out-of deltas)
        :when (= :joined (:msg m))
        :let [eid (:to m)]
        p (join-packets world (own-level sight eid) eid)]
    [eid p]))

(def ^:private home (first schema/dims))

(def ^:private everyone
  #{:time :rain-started :rain-stopped :player-chat :system-chat
    :tab-add :tab-remove :tab-latency :tab-header :default-spawn
    :game-rules})

(defn- sight-of
  "Returns what a render call needs of world.
  That is every level, the dimension of each player and the players
  of each level."
  [world]
  (let [ps (players world)
        dim-of #(or (state/dim-of world %) home)
        of (into {} (map (fn [p] [p (dim-of p)])) ps)]
    {:ps     ps
     :of     of
     :by-dim (group-by of ps)
     :levels (into {} (map (fn [d] [d (state/level world d)]))
                   schema/dims)}))

(defn- level-of [sight dim]
  (get (:levels sight) (or dim home)))

(defn- own-level [sight eid]
  (level-of sight (get (:of sight) eid)))

(defn- forgotten [deltas pid]
  (for [[tag _ _ gone] (get deltas pid)
        :when (= :tracking tag)
        eid gone]
    eid))

(defn- add-viewer [a eid pid]
  (assoc! a eid (conj (get a eid []) pid)))

(defn- viewer-index [sight deltas]
  (persistent!
    (reduce (fn [acc pid]
              (let [lv (own-level sight pid)
                    seen (get-in lv [:entities pid :tracking])
                    all (concat seen (forgotten deltas pid))]
                (reduce (fn [a eid] (add-viewer a eid pid))
                        acc all)))
            (transient (i/int-map))
            (:ps sight))))

(def ^:private entity-msgs
  #{:move :move-look :look :sync-pos :head-look :velocity :meta
    :equipment :animation :status :collect})

(defn- level-audience [sight dim m]
  (ranged-recipients (level-of sight dim)
                     (get (:by-dim sight) dim []) m))

(defn- audience
  "Returns the players an effect m without an address reaches.
  A level's effect stays in its level, the server's reaches every
  level, and some kinds reach everyone whatever their level."
  [sight m]
  (let [dim (:dim m)]
    (cond (everyone (:msg m)) (:ps sight)
          (some? dim) (level-audience sight dim m)
          :else (into [] (mapcat #(level-audience sight % m))
                      schema/dims))))

(defn- recipients [sight viewers m]
  (cond
    (:to m) [(:to m)]
    (entity-msgs (:msg m)) (get @viewers (long (:eid m)) [])
    :else
    (let [base (audience sight m)]
      (if (:except m) (remove #{(:except m)} base) base))))

(defn- entity-delta-packets [sight deltas pick]
  (for [[eid ds] deltas
        :when (pick eid)
        :let [lv (own-level sight eid)]
        d ds
        p (case (first d)
            :chunks-sent (chunk-packets lv d)
            :tracking (tracking-packets lv d)
            nil)]
    [eid p]))

(defn- explosion-packets [sight m]
  (for [eid (audience sight m)]
    [eid (explode-packet m eid)]))

(defn- msg-packets [sight viewers m]
  (if (= :explosion (:msg m))
    (explosion-packets sight m)
    (let [pkts (fx-packets (level-of sight (:dim m)) m)]
      (when (seq pkts)
        (for [eid (recipients sight viewers m)
              p pkts]
          [eid p])))))

(defn- level-entry-packets
  "The health and experience resent to the players that entered a
  level this tick, after all else the tick sends them."
  [sight ^Deltas deltas]
  (for [m (deltas/out-of deltas)
        :when (identical? :change-dimension (:msg m))
        :let [eid (:to m)
              e (get-in (own-level sight eid) [:entities eid])]
        p (resent-packets e)]
    [eid p]))

(defn- arrivals
  "Returns the players that entered a level in the tick of deltas."
  [^Deltas deltas]
  (into #{} (keep #(when (identical? :change-dimension (:msg %))
                     (:to %)))
        (deltas/out-of deltas)))

(defn- position? [p]
  (and (map? p) (identical? :player-position (:packet p))))

(defn- teleport-id
  "Returns the id of the teleport k before the last one player eid
  was sent, as its connection counts them."
  ^long [sight eid ^long k]
  (let [e (get-in (own-level sight eid) [:entities eid])]
    (mod (- (long (:tp-id e 1)) k) (long Integer/MAX_VALUE))))

(defn- numbered
  "Returns the pairs with the teleports in them numbered. The last
  one a player is sent carries the id the world holds for it."
  [sight pairs]
  (let [of (fn [[eid p]] (when (position? p) eid))
        left (frequencies (keep of pairs))
        id #(assoc %2 :teleport-id (teleport-id sight %1 %3))
        step (fn [[acc left] [eid p :as x]]
               (if (position? p)
                 (let [k (dec (long (left eid)))]
                   [(conj acc [eid (id eid p k)]) (assoc left eid k)])
                 [(conj acc x) left]))]
    (if (empty? left) pairs (first (reduce step [[] left] pairs)))))

(def ^:private moves-player #{:teleport :change-dimension})

(defn- teleports? [^Deltas deltas]
  (some #(moves-player (:msg %)) (deltas/out-of deltas)))

(defn- ordered [world sight ^Deltas deltas]
  (let [es (deltas/entities-of deltas)
        viewers (delay (viewer-index sight es))
        new (arrivals deltas)]
    (vec (concat
           (join-bursts world sight deltas)
           (entity-delta-packets sight es (complement new))
           (mapcat (fn [m] (msg-packets sight viewers m))
                   (deltas/out-of deltas))
           (entity-delta-packets sight es new)
           (forget-packets es)
           (level-entry-packets sight deltas)))))

(defn render
  "Returns [eid packet] for every player after a tick.
  It reads the world after the tick and the deltas of that tick.
  A player entering a level gets its chunks after all else."
  [world ^Deltas deltas]
  (let [sight (sight-of world)
        pairs (ordered world sight deltas)]
    (if (teleports? deltas) (numbered sight pairs) pairs)))
