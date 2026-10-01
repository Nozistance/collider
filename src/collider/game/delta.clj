(ns collider.game.delta
  "The delta tags, what each means, and the effect messages."
  (:require [clojure.data.int-map :as i]
            [clojure.set :as set]
            [collider.game.entity :as entity]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.game.schedule :as schedule]
            [collider.vec :as v]
            [collider.world.env.weather :as weather]
            [malli.core :as m]
            [malli.error :as me])
  (:import (collider V3)))

(set! *warn-on-reflection* true)

(def Pos [:tuple :int :int :int])

(defn- vec3? [v]
  (or (instance? V3 v)
      (and (sequential? v) (= 3 (count v)) (every? number? v))))

(def Vec3
  [:fn {:gen/schema
        [:tuple
         [:double {:min -64.0 :max 64.0}]
         [:double {:min -64.0 :max 64.0}]
         [:double {:min -64.0 :max 64.0}]]}
   vec3?])

(def Eid :int)

(def State :int)

(def Stack [:map [:item :keyword] [:count :int]])

(def Records [:sequential [:tuple Pos State]])

(def Coll [:fn coll?])

(def Text [:or :string :map])

(defn- merge-diff [cur add drop]
  (let [s (or cur (i/int-set))
        s (if (seq add) (into s add) s)]
    (if (seq drop) (set/difference s (set drop)) s)))

(defn- new-eids [w w' _]
  (range (long (:next-eid w 1000000)) (long (:next-eid w' 1000000))))

(def registry
  "What each delta tag means: its :schema, its :scope and its :apply.
  A :world delta changes the world as a whole, a :level one the level
  it is applied to, an :entity one the entity of its eid and an :input
  one is an event of a player. :apply takes the world and the delta,
  for an :entity delta the tick, the entity and the delta. :eids tells
  which entities a delta may add and :by-eid that it belongs to the
  level of its eid."
  {:set-blocks
   {:scope :level :schema [:cat Records [:? Coll] [:? Coll]]
    :apply level/set-blocks}
   :ticks-flushed
   {:scope :level
    :schema [:cat [:enum :block-ticks :fluid-ticks] :int Coll]
    :apply (fn [w [_ k t parked]]
             (update w k schedule/flushed t parked))}
   :schedule-ticks
   {:scope :level :schema [:cat [:map-of :int Coll]]
    :apply level/schedule-ticks}
   :schedule-copied
   {:scope :level :schema [:cat Coll]
    :apply (fn [w [_ es]]
             (update w :block-ticks schedule/add-ordered es))}
   :openers
   {:scope :level :schema [:cat Pos :int] :apply level/openers}
   :shulker-anim
   {:scope :level :schema [:cat Pos [:maybe :map]]
    :apply level/shulker-anim}
   :changed-blocks-flushed
   {:scope :level :schema [:cat]
    :apply (fn [w _] (assoc w :changed-blocks nil))}
   :block-events-run
   {:scope :level :schema [:cat]
    :apply (fn [w _] (assoc w :block-events nil))}
   :set-clock
   {:scope :world :schema [:cat :keyword :map]
    :apply (fn [w [_ k m]] (update-in w [:clocks k] merge m))}
   :set-rule
   {:scope :world :schema [:cat :keyword :any]
    :apply (fn [w [_ rule value]] (assoc-in w [:rules rule] value))}
   :set-config
   {:scope :world :schema [:cat :map]
    :apply (fn [w [_ m]] (assoc w :config m))}
   :set-world-spawn
   {:scope :world
    :schema [:cat :keyword Pos [:tuple number? number?]]
    :apply player/set-world-spawn}
   :add-chunk
   {:scope :level :schema [:cat :int :any] :apply level/add-chunk}
   :chunk-requested
   {:scope :level :schema [:cat :int]
    :apply (fn [w [_ id]]
             (update w :loading (fnil conj (i/int-set)) id))}
   :chunk-ticket
   {:scope :level :schema [:cat :int :int]
    :apply (fn [w [_ id n]]
             (update w :unknown assoc (long id) (long n)))}
   :purge-tickets
   {:scope :level :schema [:cat [:map-of :int :int]]
    :apply (fn [w [_ held]] (assoc w :unknown held))}
   :restore-chunk
   {:scope :level :schema [:cat :int :map]
    :apply level/restore-chunk}
   :player-placed
   {:scope :level :schema [:cat Eid :string Vec3]
    :apply player/placed :eids (fn [_ _ d] [(nth d 1)])}
   :spawn-progress
   {:scope :world :schema [:cat Eid [:maybe :map]]
    :apply (fn [w [_ eid req]]
             (if req
               (assoc-in w [:spawning eid] req)
               (update w :spawning dissoc eid)))}
   :unload-chunk
   {:scope :level :schema [:cat :int] :apply level/unload-chunk}
   :set-weather
   {:scope :level :schema [:cat :map]
    :apply (fn [w [_ m]] (merge w (select-keys m weather/fields)))}
   :set-block-entity
   {:scope :level :schema [:cat Pos [:maybe :map]]
    :apply level/set-block-entity}
   :spawn-entity
   {:scope :level :schema [:cat :map] :apply level/spawn-entity
    :eids new-eids}
   :xp-award
   {:scope :level :schema [:cat Vec3 :int :any [:? Vec3]]
    :apply level/xp-award :eids new-eids}
   :remove-entity
   {:scope :level :schema [:cat Eid] :by-eid true
    :apply (fn [w [_ eid]] (player/quit w eid))}
   :level-deltas
   {:scope :world :schema [:cat :keyword [:sequential :any]]}
   :change-dimension
   {:scope :world :schema [:cat Eid :keyword Vec3 number? number?]}
   :listed
   {:scope :world :schema [:cat [:map-of Eid :uuid] Coll]
    :apply (fn [w [_ add drop]]
             (update w :listed #(apply dissoc (merge % add) drop)))}
   :advance-tick
   {:scope :world :schema [:cat]
    :apply (fn [w _] (dissoc (level/advance w) :input))}
   :advance-weather
   {:scope :level :schema [:cat [:? :map]]
    :apply level/advance-weather}
   :observed
   {:scope :level :schema [:cat :map]
    :apply (fn [w [_ m]] (assoc w :observed m))}
   :merge-entity
   {:scope :entity :schema [:cat :map]
    :apply (fn [_ e [_ _ m]] (entity/merged e m))}
   :teleport
   {:scope :entity :schema [:cat Vec3]
    :apply (fn [tick e [_ _ pos]]
             (assoc e :pos (v/v3 pos) :tp-target pos :tp-at tick
                      :tp-id (player/next-teleport-id e)))}
   :client-slots
   {:scope :entity
    :schema [:cat [:map-of :int [:maybe Stack]] [:maybe Stack]]
    :apply (fn [_ e [_ _ slots carried]]
             (player/client-slots e slots carried))}
   :award
   {:scope :entity :schema [:cat :keyword :int]
    :apply (fn [_ e [_ _ k n]]
             (update e :awards (fnil conj []) [k n]))}
   :track
   {:scope :entity :schema [:cat :map]
    :apply (fn [_ e [_ _ tr]]
             (assoc (cond-> e
                      (some? (:kept-mdata e)) (dissoc :kept-mdata))
                    :track tr))}
   :tracking
   {:scope :entity :schema [:cat Coll Coll]
    :apply (fn [_ e [_ _ add drop]]
             (update e :tracking merge-diff add drop))}
   :set-slot
   {:scope :entity :schema [:cat :int [:maybe Stack]]
    :apply (fn [_ e [_ _ slot stack]]
             (if stack
               (assoc-in e [:inventory slot] stack)
               (update e :inventory dissoc slot)))}
   :chunks-sent
   {:scope :entity :schema [:cat Coll Coll [:? [:maybe :int]]]
    :apply (fn [_ e [_ _ add drop]]
             (update e :sent-chunks merge-diff add drop))}
   :damage
   {:scope :entity :schema [:cat number? [:? [:maybe :map]]]
    :apply (fn [tick e [_ eid amount src]]
             (entity/hurt e amount src tick eid))}
   :knockback
   {:scope :entity :schema [:cat number? number? number?]
    :apply (fn [tick e [_ eid power xd zd]]
             (entity/knocked e power xd zd tick eid))}
   :rest
   {:scope :entity :schema [:cat]
    :apply (fn [_ e _] (entity/rested e))}
   :push
   {:scope :entity :schema [:cat Vec3]
    :apply (fn [_ e [_ _ vel]]
             (update e :vel (fnil v/+ [0.0 0.0 0.0]) vel))}
   :player-join {:scope :input :apply player/join}
   :player-quit
   {:scope :input :apply (fn [w [_ eid]] (player/quit w eid))}
   :move {:scope :input :apply player/move}
   :client-tick-end {:scope :input :apply player/client-tick-end}
   :abilities {:scope :input :apply player/abilities}
   :player-loaded
   {:scope :input
    :apply (fn [w [_ eid]]
             (level/update-entity w eid dissoc :loaded-at))}
   :teleport-ack {:scope :input :apply player/teleport-ack}
   :respawn {:scope :input :apply player/respawn}
   :keepalive-echo {:scope :input :apply player/keepalive-echo}
   :chunk-batch-ack {:scope :input :apply player/chunk-batch-ack}
   :entity-action {:scope :input :apply player/entity-action}
   :input
   {:scope :input
    :apply (fn [w [_ eid flags]]
             (level/update-entity w eid merge flags))}
   :client-settings {:scope :input :apply player/client-settings}
   :place {:scope :input :apply player/place}
   :use-item {:scope :input :apply player/use-item}
   :release-use {:scope :input :apply player/release-use}})

(defn- applies [scopes]
  (into {} (keep (fn [[tag {:keys [scope apply]}]]
                   (when (and apply (contains? scopes scope))
                     [tag apply])))
        registry))

(def world-apply
  "Returns the apply of a :world or :level tag, by tag."
  (applies #{:world :level}))

(def entity-apply
  "Returns the apply of an :entity tag, by tag."
  (applies #{:entity}))

(def input-apply
  "Returns the apply of an :input tag, by tag."
  (applies #{:input}))

(def fx-messages
  {:blocks-changed    [[:cp :int] [:records Records]]
   :load-chunk        [[:id :int]]
   :store-chunk       [[:id :int] [:payload :map]]
   :break-effect      [[:pos Pos] [:state State]]
   :explosion         [[:center Vec3] [:radius number?]
                       [:blocks :int] [:motions :map]
                       [:pitch number?]]
   :sound             [[:kind :keyword] [:pos Vec3]
                       [:volume number?] [:pitch number?]
                       [:source {:optional true} :keyword]
                       [:entity {:optional true} :int]]
   :particles         [[:kind :keyword] [:state [:maybe State]]
                       [:pos Vec3] [:count :int]
                       [:speed number?]]
   :trail             [[:pos Vec3] [:target Vec3] [:color :int]
                       [:ticks :int]]
   :extinguish        [[:pos Pos]]
   :fizz              [[:pos Pos]]
   :bonemeal          [[:pos Pos]]
   :level-event       [[:event :int] [:pos Pos] [:data :int]]
   :sign-editor       [[:pos Pos] [:front? :boolean]]
   :open-book         [[:hand [:enum :main :off]]]
   :block-event       [[:pos Pos] [:action :int] [:param :int]]
   :block-entity      [[:pos Pos]]
   :time              [[:age :int] [:clocks :map]]
   :teleport          [[:pos Vec3] [:yaw number?] [:pitch number?]
                       [:relative :int]]
   :health            [[:health number?]]
   :experience        [[:progress number?] [:level :int]
                       [:total :int]]
   :respawn           []
   :change-dimension  [[:pos Vec3] [:yaw number?] [:pitch number?]
                       [:relative :int]
                       [:forget Coll] [:untrack Coll]]
   :default-spawn     [[:dimension :keyword] [:pos Pos]
                       [:yaw number?] [:pitch number?]]
   :rule-flag         [[:kind :keyword] [:on? :boolean]]
   :rain-started      []
   :rain-stopped      []
   :rain-level        [[:level number?]]
   :thunder-level     [[:level number?]]
   :keepalive         [[:id :int]]
   :disconnect        [[:text Text]]
   :close             []
   :joined            []
   :block-ack         [[:sequence :int]]
   :cooldown          [[:group :keyword] [:ticks :int]]
   :mob-effect        [[:eid Eid] [:effect :keyword]
                       [:amplifier :int] [:duration :int]
                       [:flags :int]]
   :mob-effect-gone   [[:eid Eid] [:effect :keyword]]
   :set-slot          [[:slot :int] [:stack [:maybe Stack]]]
   :carried           [[:stack [:maybe Stack]]]
   :held-slot         [[:slot :int]]
   :inventory         [[:slots [:sequential :any]]
                       [:carried [:maybe Stack]]]
   :suggestions       [[:id :int] [:start :int] [:length :int]
                       [:matches [:sequential :any]]]
   :system-chat       [[:text Text]]
   :player-chat       [[:text Text]]
   :overlay           [[:text Text]]
   :title             [[:kind [:enum :title :subtitle :actionbar]]
                       [:text Text]]
   :title-times       [[:fade-in :int] [:stay :int] [:fade-out :int]]
   :clear-titles      [[:reset :boolean]]
   :player-rotation   [[:yaw :double] [:relative-yaw :boolean]
                       [:pitch :double] [:relative-pitch :boolean]]
   :look-at           [[:from [:enum :feet :eyes]] [:pos :any]
                       [:id [:maybe :int]]
                       [:anchor [:maybe [:enum :feet :eyes]]]]
   :named-sound       [[:id :string] [:source :string] [:pos Vec3]
                       [:volume :double] [:pitch :double]
                       [:seed :int]]
   :stop-sound        [[:id [:maybe :string]]
                       [:source [:maybe :string]]]
   :stats             [[:stats :map]]
   :game-rules        [[:rules :map]]
   :reload            []
   :reloaded          []
   :view-distance     [[:distance :int]]
   :simulation-distance [[:distance :int]]
   :tab-add           [[:entries [:sequential :map]]]
   :tab-remove        [[:uuids [:sequential :uuid]]]
   :tab-latency       [[:entries [:sequential :map]]]
   :tab-header        [[:header Text] [:footer Text]]
   :tab-game-mode     [[:uuid :uuid] [:mode :keyword]]
   :game-mode         [[:mode :keyword]]
   :camera            [[:id :int]]
   :abilities         [[:invulnerable? :boolean] [:flying? :boolean]
                       [:may-fly? :boolean] [:instabuild? :boolean]]
   :move              [[:eid Eid] [:dx :int] [:dy :int] [:dz :int]
                       [:on-ground :boolean]]
   :move-look         [[:eid Eid] [:dx :int] [:dy :int] [:dz :int]
                       [:yaw :int] [:pitch :int]
                       [:on-ground :boolean]]
   :look              [[:eid Eid] [:yaw :int] [:pitch :int]
                       [:on-ground :boolean]]
   :sync-pos          [[:eid Eid] [:pos Vec3] [:yaw number?]
                       [:pitch number?] [:on-ground :boolean]]
   :head-look         [[:eid Eid] [:yaw number?]]
   :meta              [[:eid Eid] [:type :keyword] [:meta :map]]
   :velocity          [[:eid Eid] [:vel Vec3]]
   :attributes        [[:eid Eid] [:attributes [:sequential :any]]]
   :equipment         [[:eid Eid] [:slot :int]
                       [:stack [:maybe Stack]]]
   :animation         [[:eid Eid] [:kind :keyword]]
   :status            [[:eid Eid] [:kind :keyword]]
   :damage-event      [[:eid Eid] [:kind :keyword]
                       [:cause [:maybe Eid]] [:direct [:maybe Eid]]
                       [:pos [:maybe Vec3]]]
   :collect           [[:eid Eid] [:collector Eid]]
   :open-screen       [[:container :int] [:menu :keyword]
                       [:title :map]]
   :container-content [[:container :int] [:state-id :int]
                       [:items [:sequential [:maybe Stack]]]
                       [:carried [:maybe Stack]]]
   :container-slot    [[:container :int] [:state-id :int]
                       [:slot :int] [:stack [:maybe Stack]]]
   :container-data    [[:container :int] [:id :int] [:value :int]]
   :container-close   [[:container :int]]})

(defn- with-address [fields]
  (into [:map [:msg :keyword] [:to {:optional true} Eid]
         [:except {:optional true} Eid]
         [:dim {:optional true} [:maybe :keyword]]]
        fields))

(def Fx
  (into [:multi {:dispatch :msg}]
        (for [[msg fields] fx-messages]
          [msg (with-address fields)])))

(def Delta
  (into [:multi {:dispatch first}]
        (concat
          (for [[tag {:keys [scope schema]}] registry :when schema]
            [tag (if (identical? :entity scope)
                   (into [:cat [:= tag] Eid] (rest schema))
                   (into [:cat [:= tag]] (rest schema)))])
          [[:fx [:cat [:= :fx] Fx]]])))

(def ^:private delta-validator (delay (m/validator Delta)))

(def ^:private delta-explainer (delay (m/explainer Delta)))

(defn explain
  "Returns why delta breaks its schema, or nil when it does not."
  [delta]
  (when-let [e (@delta-explainer delta)]
    (me/humanize e)))

(def validate? (Boolean/getBoolean "collider.validate"))

(defn check!
  "Returns deltas, or throws on the first one that breaks its schema."
  [deltas]
  (doseq [d deltas]
    (when-not (@delta-validator d)
      (throw (ex-info (str "invalid delta " (first d))
                      {:delta d :why (explain d)}))))
  deltas)

