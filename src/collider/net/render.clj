(ns collider.net.render
  "Packets for each player from the tick."
  (:require [collider.game.block.blockentity :as be]
            [clojure.data.int-map :as i]
            [collider.config :as config]
            [collider.data :as data]
            [collider.game.command.tree :as commands]
            [collider.game.deltas]
            [collider.game.state :as state]
            [collider.game.gamerules :as rules]
            [collider.log :as log]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.game.deltas Deltas)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- block-state ^long [world pos]
  (chunk/chunks-get-block (:chunks world) pos))

(defn- players [world]
  (vec (sort (vals (:players world)))))

(defn- text-of [runs]
  (if-let [t (first (filter :translate runs))]
    (select-keys t [:translate :with])
    (apply str (map :text runs))))

(defn- chunk-packet [world id]
  (let [[x z] (chunk/id->pos id)]
    {:packet         :level-chunk-with-light :cx x :cz z
     :chunk          (get-in world [:chunks id] chunk/empty-chunk)
     :block-entities (be/wire (get-in world [:block-entities id]))}))

(defn- forget-chunk-packet [id]
  (let [[x z] (chunk/id->pos id)]
    {:packet :forget-level-chunk :cx x :cz z}))

(defn- added-chunk-packets [world add]
  (when (seq add)
    (concat
      [{:packet :chunk-batch-start}]
      (map #(chunk-packet world %) add)
      [{:packet :chunk-batch-finished :size (count add)}])))

(defn- chunk-packets [world [_ eid add drop]]
  (let [[cx cz] (chunk/id->pos (get-in world [:entities eid :chunk-pos]))]
    (concat
      [{:packet :set-chunk-cache-center :cx cx :cz cz}]
      (added-chunk-packets world add)
      (map forget-chunk-packet drop))))

(def ^:private ^:table entity-type
  (delay
    {:player        (data/registry-id "entity_type" :player)
     :sheep         (data/registry-id "entity_type" :sheep)
     :cow           (data/registry-id "entity_type" :cow)
     :mooshroom     (data/registry-id "entity_type" :mooshroom)
     :item          (data/registry-id "entity_type" :item)
     :tnt           (data/registry-id "entity_type" :tnt)
     :falling-block (data/registry-id "entity_type" :falling-block)}))

(defn- kind-of [e]
  (let [t (:type e)]
    (if (contains? @entity-type t) t :player)))

(defn- uuid-of [eid e]
  (or (:uuid e) (UUID. (long eid) (long eid))))

(defn- flags-byte [meta]
  (bit-or (if (:burning? meta) 0x01 0)
          (if (:sneaking? meta) 0x02 0)
          (if (:sprinting? meta) 0x08 0)
          (if (:swimming? meta) 0x10 0)))

(def ^:private flag-keys [:burning? :sneaking? :sprinting? :swimming?])
(defn- flags? [meta] (boolean (some #(contains? meta %) flag-keys)))

(def ^:private pose-id
  {:standing 0 :sleeping 2 :swimming 3 :crouching 5})

(defn- player-data [meta]
  (cond-> []
          (flags? meta) (conj [0 :byte (flags-byte meta)])
          (contains? meta :pose) (conj [6 :pose (pose-id (:pose meta) 0)])
          (contains? meta :using-item?)
          (conj [8 :byte (case (:using-item? meta) :off 0x03 (nil false) 0 0x01)])
          (contains? meta :sleeping-pos)
          (conj [14 :optional-block-pos (:sleeping-pos meta)])))

(defn- animal-data [meta]
  (cond-> []
          (flags? meta) (conj [0 :byte (flags-byte meta)])
          (contains? meta :baby?) (conj [16 :boolean (boolean (:baby? meta))])
          (contains? meta :variant) (conj [17 :int (long (:variant meta))])
          (contains? meta :color)
          (conj [18 :byte (bit-or (bit-and (long (or (:color meta) 0)) 15)
                                  (if (:sheared? meta) 0x10 0))])))

(defn- entity-data [kind meta]
  (case kind
    :player (player-data meta)
    (:sheep :cow :mooshroom) (animal-data meta)
    :item (cond-> []
                  (flags? meta) (conj [0 :byte (flags-byte meta)])
                  (contains? meta :stack) (conj [8 :item (:stack meta)]))
    :tnt (let [f (:fuse meta 80)]
           (if (or (= 80 f) (not (contains? meta :fuse))) [] [[8 :int f]]))
    :falling-block (if (contains? meta :start)
                     [[8 :block-pos (:start meta)]] [])
    []))

(def ^:private equipment-slots [0 2 3 4 5])
(defn- add-entity-packet [eid e tr kind]
  {:packet :add-entity :eid eid :uuid (uuid-of eid e) :type (@entity-type kind)
   :pos    (if tr (mapv double (:pos tr)) (:pos e))
   :vel    (or (when tr (:vel-sent tr)) (:vel e) [0.0 0.0 0.0])
   :yaw    (:yaw e 0.0) :pitch (:pitch e 0.0) :head-yaw (or (:head-yaw e) (:yaw e 0.0))
   :data   (if (= :falling-block kind) (:block e) 0)})

(defn- equipment-of [tr]
  (keep-indexed (fn [i s] (when s [(equipment-slots i) s])) (if tr (:equip tr) [])))

(defn- spawn-packets [world eid]
  (when-let [e (get-in world [:entities eid])]
    (let [kind (kind-of e)
          tr (:track e)
          d (entity-data kind (if tr (:mdata tr) {}))
          equip (equipment-of tr)]
      (concat
        [{:packet :bundle-delimiter} (add-entity-packet eid e tr kind)]
        (when (seq d) [{:packet :set-entity-data :eid eid :data d}])
        (when (seq equip) [{:packet :set-equipment :eid eid :slots equip}])
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
   :cow/say                       [:entity.cow.ambient 6]
   :cow/step                      [:entity.cow.step 6]
   :cow/hurt                      [:entity.cow.hurt 6]
   :cow/death                     [:entity.cow.death 6]
   :cow/milk                      [:entity.cow.milk 6]
   :mooshroom/milk                [:entity.mooshroom.milk 6]
   :mooshroom/suspicious          [:entity.mooshroom.suspicious-milk 6]
   :mooshroom/shear               [:entity.mooshroom.shear 6]
   :mooshroom/eat                 [:entity.mooshroom.eat 6]
   :tnt/primed                    [:entity.tnt.primed 4]
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
   :sweet-berry-bush/pick-berries [:block.sweet-berry-bush.pick-berries 4]
   :bottle/fill                   [:item.bottle.fill 4]
   :bottle/empty                  [:item.bottle.empty 4]
   :copper-golem/statue           [:entity.copper-golem-become-statue 4]
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
   :bookshelf/insert-enchanted    [:block.chiseled-bookshelf.insert.enchanted 4]
   :bookshelf/pickup              [:block.chiseled-bookshelf.pickup 4]
   :bookshelf/pickup-enchanted    [:block.chiseled-bookshelf.pickup.enchanted 4]
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
   :generic/burn                  [:entity.generic.burn 8]
   :explosion                     [:entity.generic.explode 4]
   :splash                        [:entity.generic.splash 6]
   :swim                          [:entity.generic.swim 6]})

(def ^:private ^:table overworld (delay (data/datapack-id "dimension_type" :overworld)))
(def ^:private ^:table explosion-block-particles
  (delay [[(data/registry-id "particle_type" :poof) 0.5 1.0 1]
          [(data/registry-id "particle_type" :smoke) 1.0 1.0 1]]))

(def ^:private ^:table explosion-particle
  (delay (data/registry-id "particle_type" :explosion-emitter)))
(defn- particles-packet [m]
  {:packet :level-particles :particle (data/registry-id "particle_type" (:kind m)) :state (:state m)
   :pos    (:pos m) :count (:count m) :speed (:speed m)})

(def ^:private unhandled (atom #{}))
(defn- once! [kind]
  (when-not (@unhandled kind)
    (swap! unhandled conj kind)
    (log/warn "render:" kind "not rendered yet")))

(defn- block-records [[cx cz] records]
  (if (= 1 (count records))
    (let [[[pos st]] records] [{:packet :block-update :pos pos :state st}])
    (for [[sy recs] (group-by (fn [[[_ y _] _]] (bit-shift-right (long y) 4)) records)]
      {:packet  :section-blocks-update :section [cx sy cz]
       :changes (map (fn [[[x y z] st]]
                       [(bit-or (bit-shift-left (bit-and (long x) 15) 8)
                                (bit-shift-left (bit-and (long z) 15) 4)
                                (bit-and (long y) 15))
                        st])
                     recs)})))

(defn- status-packet [m]
  (case (:kind m)
    :hurt {:packet :hurt-animation :eid (:eid m) :yaw 0.0}
    :death {:packet :entity-event :eid (:eid m) :event 3}
    :eat {:packet :entity-event :eid (:eid m) :event 10}
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
    {:packet :sound :sound id :pos (:pos m) :volume (:volume m) :pitch (:pitch m)
     :source (get sound-sources (:source m) src)}
    (once! [:sound (:kind m)])))

(defn- explode-packet [m eid]
  (let [k (get (:motions m) eid)]
    {:packet    :explode :center (:center m) :radius (:radius m) :blocks (:blocks m)
     :knockback (when (and k (some #(not (zero? (double %))) k)) k)
     :particle  @explosion-particle :sound (first (sound-id :explosion))
     :block-particles @explosion-block-particles}))

(def ^:private ^:const explosion-range-sq 4096.0)
(defn- in-earshot? [world center eid]
  (when-let [p (get-in world [:entities eid :pos])]
    (let [dx (- (v/x p) (double (center 0)))
          dy (- (v/y p) (double (center 1)))
          dz (- (v/z p) (double (center 2)))]
      (< (+ (* dx dx) (* dy dy) (* dz dz)) explosion-range-sq))))

(defn- explosion-packets [world ps m]
  (for [eid ps
        :when (in-earshot? world (:center m) eid)]
    [eid (explode-packet m eid)]))

(def ^:private session-fx
  {:teleport      (fn [world m] [{:packet :player-position :teleport-id (long (:tick world))
                                  :pos    (:pos m) :yaw (:yaw m) :pitch (:pitch m)}])
   :keepalive     (fn [_ m] [{:packet :keep-alive :id (:id m)}])
   :disconnect    (fn [_ m] [{:packet :disconnect :text (:text m)}])
   :system-chat   (fn [_ m] [{:packet :system-chat :text (text-of (:runs m)) :overlay false}])
   :overlay       (fn [_ m] [{:packet :system-chat :text (text-of (:runs m)) :overlay true}])
   :player-chat   (fn [_ m] [{:packet :system-chat :text (str "<" (:name m) "> " (text-of (:runs m)))
                              :overlay false}])
   :stats         (fn [_ m] [{:packet :award-stats :stats (:stats m)}])
   :suggestions   (fn [_ m] [{:packet  :command-suggestions :id (:id m) :start (:start m)
                              :length  (:length m) :matches (:matches m)}])
   :game-rules    (fn [_ m] [{:packet :game-rule-values
                              :values (map (fn [[k v]] [(rules/wire-name k) (rules/serialize k v)])
                                           (:rules m))}])
   :health        (fn [_ m] [{:packet :set-health :health (:health m) :food 20 :saturation 5.0}])
   :respawn       (fn [_ _] [{:packet :respawn :dimension-type @overworld :keep 0}
                             {:packet :game-event :event 13 :value 0.0}])
   :default-spawn (fn [_ m] [{:packet :set-default-spawn-position :pos (:pos m)}])
   :joined        (fn [_ _] nil)
   :close         (fn [_ _] [:close])})

(def ^:private world-fx
  {:rain-started   (fn [_ _] [{:packet :game-event :event 1 :value 0.0}])
   :rain-stopped   (fn [_ _] [{:packet :game-event :event 2 :value 0.0}])
   :rain-level     (fn [_ m] [{:packet :game-event :event 7 :value (:level m)}])
   :thunder-level  (fn [_ m] [{:packet :game-event :event 8 :value (:level m)}])
   :time           (fn [_ m] [{:packet :set-time :age (:age m) :time (:time m)}])
   :blocks-changed (fn [_ m] (block-records (chunk/id->pos (:cp m)) (:records m)))
   :break-effect   (fn [_ m] [{:packet :level-event :event 2001 :pos (:pos m) :data (:state m)}])
   :fizz           (fn [_ m] [{:packet :level-event :event 1501 :pos (:pos m) :data 0}])
   :bonemeal       (fn [_ m] [{:packet :level-event :event 1505 :pos (:pos m) :data 15}])
   :extinguish     (fn [_ m] [{:packet :level-event :event 1009 :pos (:pos m) :data 0}])
   :level-event    (fn [_ m] [{:packet :level-event :event (:event m) :pos (:pos m) :data (:data m 0)}])
   :sign-editor    (fn [_ m] [{:packet :open-sign-editor :pos (:pos m) :front? (:front? m)}])
   :block-event    (fn [world m] [{:packet :block-event :pos (:pos m) :action (:action m) :param (:param m)
                                   :block  (data/registry-id "block"
                                                             (block/block-of (block-state world (:pos m))))}])
   :block-entity   (fn [world m] (when-let [e (be/at world (:pos m))]
                                   (when (be/on-wire? e)
                                     [{:packet :block-entity-data :pos (:pos m)
                                       :type   (be/type-id e) :nbt (be/nbt e)}])))
   :sound          (fn [_ m] (when-let [p (sound-packet m)] [p]))
   :particles      (fn [_ m] [(particles-packet m)])
   :explosion      (fn [_ _] nil)
   :load-chunk     (fn [_ _] nil)
   :store-chunk    (fn [_ _] nil)})

(def ^:private container-fx
  {:set-slot          (fn [_ m] [{:packet :container-set-slot :slot (:slot m) :stack (:stack m)}])
   :carried           (fn [_ m] [{:packet :set-cursor-item :stack (:stack m)}])
   :open-screen       (fn [_ m] [{:packet :open-screen :container (:container m)
                                  :menu   (data/registry-id "menu" (:menu m)) :title (:title m)}])
   :container-content (fn [_ m] [{:packet   :container-set-content :container (:container m)
                                  :state-id (:state-id m) :items (:items m) :carried (:carried m)}])
   :container-slot    (fn [_ m] [{:packet   :container-set-slot :container (:container m)
                                  :state-id (:state-id m) :slot (:slot m) :stack (:stack m)}])
   :container-data    (fn [_ m] [{:packet :container-set-data :container (:container m)
                                  :id     (:id m) :value (:value m)}])
   :container-close   (fn [_ m] [{:packet :container-close :container (:container m)}])
   :held-slot         (fn [_ m] [{:packet :set-held-slot :slot (:slot m)}])
   :block-ack         (fn [_ m] [{:packet :block-changed-ack :sequence (:sequence m)}])
   :inventory         (fn [_ m] [{:packet :container-set-content :items (:slots m) :carried (:carried m)}])
   :tab-add           (fn [_ m] [{:packet :player-info-update :players (:entries m)}])
   :tab-remove        (fn [_ m] [{:packet :player-info-remove :uuids (:uuids m)}])
   :tab-latency       (fn [_ m] [{:packet :player-info-update :action :latency :players (:entries m)}])
   :tab-header        (fn [_ m] [{:packet :tab-list :header (:header m) :footer (:footer m)}])})

(def ^:private entity-fx
  {:move      (fn [_ m] [{:packet :move-entity-pos :eid (:eid m) :dx (:dx m) :dy (:dy m) :dz (:dz m)
                          :on-ground (:on-ground m)}])
   :move-look (fn [_ m] [{:packet :move-entity-pos-rot :eid (:eid m) :dx (:dx m) :dy (:dy m) :dz (:dz m)
                          :yaw    (:yaw m) :pitch (:pitch m) :on-ground (:on-ground m)}])
   :look      (fn [_ m] [{:packet :move-entity-rot :eid (:eid m) :yaw (:yaw m) :pitch (:pitch m)
                          :on-ground (:on-ground m)}])
   :sync-pos  (fn [_ m] [{:packet    :entity-position-sync :eid (:eid m) :pos (:pos m)
                          :yaw       (:yaw m) :pitch (:pitch m) :on-ground (:on-ground m)}])
   :head-look (fn [_ m] [{:packet :rotate-head :eid (:eid m) :yaw (:yaw m)}])
   :velocity  (fn [_ m] [{:packet :set-entity-motion :eid (:eid m) :vel (:vel m)}])
   :meta      (fn [_ m] (let [d (entity-data (kind-of m) (:meta m))]
                          (when (seq d) [{:packet :set-entity-data :eid (:eid m) :data d}])))
   :equipment (fn [_ m] [{:packet :set-equipment :eid (:eid m)
                          :slots  [[(equipment-slots (:slot m)) (:stack m)]]}])
   :animation (fn [_ m] [{:packet :animate :eid (:eid m)
                          :action (case (:kind m) :swing 0 :wake-up 2 :crit 4 0)}])
   :status    (fn [_ m] (when-let [p (status-packet m)] [p]))
   :collect   (fn [_ m] [{:packet :take-item-entity :item (:eid m) :collector (:collector m) :amount 1}])})

(def ^:private fx-table (merge session-fx world-fx container-fx entity-fx))

(defn- fx-packets [world m]
  (if-let [f (fx-table (:msg m))]
    (f world m)
    (once! (:msg m))))

(def ^:private ^:table command-tree (delay (commands/tree)))
(def ^:private world-border-size 5.9999968E7)
(def ^:private world-border-max 29999984)
(def ^:private op-level-event 24)
(defn- join-spawn [world]
  (let [[x y z] (or (:world-spawn world) state/spawn-pos)]
    [(long (Math/floor (double x))) (long (Math/floor (double y))) (long (Math/floor (double z)))]))

(defn- join-login-packets [cfg eid]
  (let [{:keys [max-players view-distance simulation-distance]} cfg]
    [{:packet              :login :eid eid
      :max-players         (min 255 (long max-players))
      :view-distance       view-distance
      :simulation-distance simulation-distance
      :dimension-type      @overworld}
     {:packet :change-difficulty :difficulty 0 :locked false}
     {:packet       :player-abilities :flags (bit-or 1 4 8)
      :flying-speed 0.05 :walking-speed 0.1}]))

(defn- join-world-packets [world eid motd]
  (let [[x y z] (join-spawn world)]
    [(assoc (data/recipes) :packet :update-recipes)
     {:packet :entity-event :eid eid :event (+ op-level-event 4)}
     {:packet :commands :nodes @command-tree}
     {:packet :server-data :motd motd}
     {:packet :initialize-border :size world-border-size :max-size world-border-max}
     {:packet :set-default-spawn-position :pos [x y z]}
     {:packet :game-event :event 13 :value 0.0}
     {:packet :ticking-state :rate 20.0 :frozen? false}
     {:packet :ticking-step :steps 0}]))

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

(defn- join-packets [world eid]
  (let [cfg (merge config/defaults (:config world))]
    (concat (join-login-packets cfg eid)
            (join-world-packets world eid (:motd cfg))
            (join-player-packets eid))))

(defn- join-bursts [world ^Deltas deltas]
  (for [m (.out deltas)
        :when (= :joined (:msg m))
        p (join-packets world (:to m))]
    [(:to m) p]))

(defn- forgotten [deltas pid]
  (for [[tag _ _ gone] (get deltas pid) :when (= :tracking tag) eid gone] eid))

(defn- viewer-index [world deltas ps]
  (persistent!
    (reduce (fn [acc pid]
              (reduce (fn [a eid] (assoc! a eid (conj (get a eid []) pid)))
                      acc
                      (concat (get-in world [:entities pid :tracking]) (forgotten deltas pid))))
            (transient (i/int-map))
            ps)))

(def ^:private entity-msgs
  #{:move :move-look :look :sync-pos :head-look :velocity :meta :equipment :animation :status :collect})

(defn- recipients [ps viewers m]
  (cond
    (:to m) [(:to m)]
    (:except m) (remove #{(:except m)} ps)
    (entity-msgs (:msg m)) (get @viewers (long (:eid m)) [])
    :else ps))

(defn- entity-delta-packets [world deltas]
  (for [[eid ds] deltas
        d ds
        p (case (first d)
            :chunks-sent (chunk-packets world d)
            :tracking (tracking-packets world d)
            nil)]
    [eid p]))

(defn- msg-packets [world ps viewers m]
  (if (= :explosion (:msg m))
    (explosion-packets world ps m)
    (let [pkts (fx-packets world m)]
      (when (seq pkts)
        (for [eid (recipients ps viewers m)
              p pkts]
          [eid p])))))

(defn render [world ^Deltas deltas]
  (let [ps (players world)
        viewers (delay (viewer-index world (.entities deltas) ps))]
    (concat
      (join-bursts world deltas)
      (entity-delta-packets world (.entities deltas))
      (mapcat (fn [m] (msg-packets world ps viewers m)) (.out deltas))
      (forget-packets (.entities deltas)))))
