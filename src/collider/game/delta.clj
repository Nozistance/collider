(ns collider.game.delta
  (:import (collider.java V3)))

(set! *warn-on-reflection* true)

(def Pos [:tuple :int :int :int])
(defn- vec3? [v]
  (or (instance? V3 v)
      (and (sequential? v) (= 3 (count v)) (every? number? v))))
(def Vec3 [:fn vec3?])
(def Eid :int)
(def State :int)
(def Stack [:map [:item :keyword] [:count :int]])
(def Records [:sequential [:tuple Pos State]])
(def Coll [:fn coll?])
(def Runs [:sequential [:or :string :map]])
(def world-deltas
  {:set-blocks
   [:cat Records [:? :int]]
   :ticks-flushed
   [:cat :int Coll]
   :schedule-ticks
   [:cat [:map-of :int Coll]]
   :container-recheck
   [:cat Pos [:maybe :int]]
   :shulker-anim
   [:cat Pos [:maybe :map]]
   :block-events-flushed
   [:cat]
   :set-time
   [:cat :int]
   :set-rule
   [:cat :keyword :any]
   :set-world-spawn
   [:cat Pos]
   :set-weather
   [:cat :map]
   :set-block-entity
   [:cat Pos [:maybe :map]]
   :spawn-entity
   [:cat :map]
   :remove-entity
   [:cat Eid]
   :listed
   [:cat [:map-of Eid :uuid] Coll]})

(def entity-deltas
  {:merge-entity
   [:cat :map]
   :teleport
   [:cat Vec3]
   :client-slots
   [:cat [:map-of :int [:maybe Stack]] [:maybe Stack]]
   :track
   [:cat :map]
   :tracking
   [:cat Coll Coll]
   :set-slot
   [:cat :int [:maybe Stack]]
   :chunks-sent
   [:cat Coll Coll]
   :damage
   [:cat number? [:? [:cat number? number?]]]
   :push
   [:cat Vec3]})

(def fx-messages
  {:blocks-changed    [[:cp :int] [:records Records]]
   :break-effect      [[:pos Pos] [:state State]]
   :explosion         [[:center Vec3] [:radius number?] [:blocks :int] [:motions :map] [:pitch number?]]
   :sound             [[:kind :keyword] [:pos Vec3] [:volume number?] [:pitch number?]]
   :particles         [[:kind :keyword] [:state [:maybe State]] [:pos Vec3] [:count :int] [:speed number?]]
   :extinguish        [[:pos Pos]]
   :fizz              [[:pos Pos]]
   :bonemeal          [[:pos Pos]]
   :level-event       [[:event :int] [:pos Pos] [:data :int]]
   :sign-editor       [[:pos Pos] [:front? :boolean]]
   :block-event       [[:pos Pos] [:action :int] [:param :int]]
   :block-entity      [[:pos Pos]]
   :time              [[:age :int] [:time :int]]
   :teleport          [[:pos Vec3] [:yaw number?] [:pitch number?]]
   :health            [[:health number?]]
   :respawn           []
   :default-spawn     [[:pos Pos]]
   :rain-started      []
   :rain-stopped      []
   :rain-level        [[:level number?]]
   :thunder-level     [[:level number?]]
   :keepalive         [[:id :int]]
   :disconnect        [[:text [:or :string :map]]]
   :close             []
   :joined            []
   :block-ack         [[:sequence :int]]
   :set-slot          [[:slot :int] [:stack [:maybe Stack]]]
   :carried           [[:stack [:maybe Stack]]]
   :held-slot         [[:slot :int]]
   :inventory         [[:slots [:sequential :any]] [:carried [:maybe Stack]]]
   :suggestions       [[:id :int] [:start :int] [:length :int] [:matches [:sequential :any]]]
   :system-chat       [[:runs Runs]]
   :player-chat       [[:name :string] [:runs Runs]]
   :overlay           [[:runs Runs]]
   :stats             [[:stats :map]]
   :game-rules        [[:rules :map]]
   :tab-add           [[:entries [:sequential :map]]]
   :tab-remove        [[:uuids [:sequential :uuid]]]
   :tab-latency       [[:entries [:sequential :map]]]
   :tab-header        [[:header :string] [:footer :string]]
   :move              [[:eid Eid] [:dx :int] [:dy :int] [:dz :int] [:on-ground :boolean]]
   :move-look         [[:eid Eid] [:dx :int] [:dy :int] [:dz :int] [:yaw :int] [:pitch :int] [:on-ground :boolean]]
   :look              [[:eid Eid] [:yaw :int] [:pitch :int] [:on-ground :boolean]]
   :sync-pos          [[:eid Eid] [:pos Vec3] [:yaw number?] [:pitch number?] [:on-ground :boolean]]
   :head-look         [[:eid Eid] [:yaw number?]]
   :meta              [[:eid Eid] [:meta :map]]
   :velocity          [[:eid Eid] [:vel Vec3]]
   :equipment         [[:eid Eid] [:slot :int] [:stack [:maybe Stack]]]
   :animation         [[:eid Eid] [:kind :keyword]]
   :status            [[:eid Eid] [:kind :keyword]]
   :collect           [[:eid Eid] [:collector Eid]]
   :open-screen       [[:container :int] [:menu :keyword] [:title :map]]
   :container-content [[:container :int] [:state-id :int] [:items [:sequential [:maybe Stack]]] [:carried [:maybe Stack]]]
   :container-slot    [[:container :int] [:state-id :int] [:slot :int] [:stack [:maybe Stack]]]
   :container-data    [[:container :int] [:id :int] [:value :int]]
   :container-close   [[:container :int]]})

(defn- with-address [fields]
  (into [:map [:msg :keyword] [:to {:optional true} Eid] [:except {:optional true} Eid]] fields))

(def Fx
  (into [:multi {:dispatch :msg}]
        (for [[msg fields] fx-messages] [msg (with-address fields)])))

(def Delta
  (into [:multi {:dispatch first}]
        (concat (for [[tag args] world-deltas] [tag (into [:cat [:= tag]] (rest args))])
                (for [[tag args] entity-deltas] [tag (into [:cat [:= tag] Eid] (rest args))])
                [[:fx [:cat [:= :fx] Fx]]])))

(def ^:private delta-validator (delay ((requiring-resolve 'malli.core/validator) Delta)))
(def ^:private delta-explainer (delay ((requiring-resolve 'malli.core/explainer) Delta)))
(defn valid? [delta] (@delta-validator delta))
(defn explain [delta]
  (when-let [e (@delta-explainer delta)]
    ((requiring-resolve 'malli.error/humanize) e)))

(def validate? (Boolean/getBoolean "collider.validate"))

(defn check! [deltas]
  (doseq [d deltas]
    (when-not (@delta-validator d)
      (throw (ex-info (str "invalid delta " (first d)) {:delta d :why (explain d)}))))
  deltas)

