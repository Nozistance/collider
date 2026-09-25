(ns collider.game.schema
  "Schema of the world map and of the player profile."
  (:require [clojure.data.int-map :as i]
            [collider.game.entity :as entity]
            [collider.game.gamerules :as rules]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.env.weather :as weather]))

(set! *warn-on-reflection* true)

(defn chunk-entity?
  "Returns true when entity e belongs to chunk id. Players never do."
  [^long id e]
  (and (not= :player (:type e))
       (= id (chunk/pos-chunk (:pos e)))))

(defn chunk-tick?
  "Returns true when the block with packed id bid lies in chunk id."
  [^long id ^long bid]
  (= id (chunk/block-id-chunk bid)))

(defn- chunk-entities [w id t]
  (into {} (keep (fn [[eid e]]
                   (when (chunk-entity? id e)
                     [eid (entity/saved e t)])))
        (:entities w)))

(defn- tick-positions [id bids]
  (into [] (comp (filter #(chunk-tick? id %))
                 (map chunk/id->block-pos))
        bids))

(defn- chunk-ticks [w ^long id]
  (let [t (long (:tick w 0))]
    (into [] (keep (fn [[at bids]]
                     (let [ps (tick-positions id bids)]
                       (when (seq ps) [(- (long at) t) ps]))))
          (:block-ticks w))))

(defn chunk-payload
  "Returns chunk id with what belongs to it.
  That is its block entities, the entities in it except players,
  and its block ticks as delays from now."
  [w id]
  (let [id (long id) t (long (:tick w 0))]
    {:chunk          (get (:chunks w) id)
     :block-entities (into {} (get-in w [:block-entities id]))
     :entities       (chunk-entities w id t)
     :ticks          (chunk-ticks w id)}))

(defn- ticks-back [bt ^long t ticks]
  (reduce (fn [bt [dt ps]]
            (update bt (+ t (max 1 (long dt)))
                    (fnil into (i/int-set))
                    (map chunk/block-pos->id ps)))
          bt ticks))

(defn- entity-entry [t]
  (fn [[eid m]]
    (when-let [e (entity/loaded m t)]
      [(long eid) e])))

(defn refreshed
  "Returns [payload next-eid] with the bodies of the payload under
  ids no one has had yet. Only the uuid of a body outlives its
  chunk; its id does not."
  [w {:keys [entities] :as payload}]
  (let [n (long (:next-eid w 1000000))
        renumber (fn [i [_ e]] [(+ n (long i)) e])
        fresh (map-indexed renumber entities)]
    [(assoc payload :entities (into {} fresh))
     (+ n (count entities))]))

(defn- block-entity-entry [[p e]] [(vec p) e])

(defn with-chunk
  "Returns w with the saved chunk id put back. Its block ticks come
  due after the delays they were saved with."
  [w id {:keys [chunk block-entities entities ticks]}]
  (let [id (long id)
        t (long (:tick w 0))
        es (keep (entity-entry t))
        bes (into {} (map block-entity-entry) block-entities)]
    (cond-> (-> w
                (update :chunks assoc id chunk)
                (update :entities into es entities)
                (update :block-ticks ticks-back t ticks)
                (update :loading disj id))
      (seq block-entities) (assoc-in [:block-entities id] bes))))

(declare profile-of)

(defn- named-player? [e]
  (and (= :player (:type e)) (:name e)))

(defn- store-profiles [profiles world]
  (into profiles
        (for [[dim lv] (:levels world)
              [_ e] (:entities lv) :when (named-player? e)]
          [(:name e) (profile-of e dim)])))

(defn- wet? [world]
  (and (:raining? world) (weather/can-have-weather? (:dim world))))

(defn- store-rain-level [_ world]
  (if (wet? world) 1.0 0.0))

(defn- store-thunder-level [_ world]
  (if (and (wet? world) (:thundering? world)) 1.0 0.0))

(defn- store-long [v _] (long (or v 0)))

(defn- load-long [v] (long (or v 0)))

(defn- store-same [v _] v)

(defn- store-boolean [v _] (boolean v))

(defn- load-rules [v]
  (merge rules/defaults (select-keys v (keys rules/defaults))))

(def world
  {:tick               {:default 0 :store store-long
                        :load load-long :schema :int
                        :scope :shared}
   :time-ms            {:default 0 :scope :shared}
   :time-of-day        {:default 0 :store store-same
                        :load identity :schema :int
                        :scope :shared}
   :next-eid           {:default 1000000 :store store-same
                        :load identity :schema :int
                        :scope :shared}
   :rules              {:default rules/defaults :store store-same
                        :load load-rules :schema :map
                        :scope :shared}
   :profiles           {:default {} :store store-profiles
                        :load identity
                        :schema [:map-of :string :map]
                        :scope :shared}
   :chunks             {:default chunk/no-chunks :load identity
                        :scope :level}
   :entities           {:default (i/int-map) :load identity
                        :scope :level}
   :block-ticks        {:default (i/int-map) :load identity
                        :scope :level}
   :block-entities     {:default (i/int-map) :load identity
                        :scope :level}
   :stored             {:default (i/int-set) :scope :level}
   :loading            {:default (i/int-set) :scope :level}
   :unknown            {:default (i/int-map) :scope :level}
   :world-spawn        {:default [24 4 8] :store store-same
                        :load identity
                        :schema [:tuple :int :int :int]
                        :scope :shared}
   :world-spawn-dimension
   {:default :overworld :store store-same :load identity
    :schema :keyword :scope :shared}
   :clear-weather-time {:default 0 :store store-long
                        :load load-long :schema :int
                        :scope :shared}
   :rain-time          {:default 0 :store store-long
                        :load load-long :schema :int
                        :scope :shared}
   :thunder-time       {:default 0 :store store-long
                        :load load-long :schema :int
                        :scope :shared}
   :raining?           {:default false :store store-boolean
                        :load boolean :schema :boolean
                        :scope :shared}
   :thundering?        {:default false :store store-boolean
                        :load boolean :schema :boolean
                        :scope :shared}
   :rain-level         {:default 0.0 :store store-rain-level
                        :load double :schema number?
                        :scope :level}
   :o-rain-level       {:default 0.0 :store store-rain-level
                        :load double :schema number?
                        :scope :level}
   :thunder-level      {:default 0.0 :store store-thunder-level
                        :load double :schema number?
                        :scope :level}
   :o-thunder-level    {:default 0.0 :store store-thunder-level
                        :load double :schema number?
                        :scope :level}
   :container-rechecks {:default {} :scope :level}
   :shulker-anim       {:default {} :scope :level}
   :players            {:default {} :scope :shared}
   :spawning           {:default (i/int-map) :scope :shared}
   :listed             {:default {} :scope :shared}})

(defn- of-scope [scope]
  (into {} (for [[k v] world :when (= scope (:scope v))] [k v])))

(def ^:private shared-table (of-scope :shared))

(def ^:private level-table (of-scope :level))

(def level-keys
  "The keys of world that belong to a level, not the shared part.
  Includes the transient keys the tick adds and drops."
  (into #{:active-chunks :block-events :use-origins :moves :quits
          :observed}
        (keys level-table)))

(def dims
  "The dimensions of the world, in the order the tick runs them."
  [:overworld :the-nether :the-end])

(def Level
  (into [:map {:closed true}
         [:stored {:optional true} [:fn set?]]]
        (for [[k {s :schema}] level-table :when s]
          [k {:optional true} s])))

(def Meta
  (into [:map {:closed true}
         [:levels {:optional true} [:map-of :keyword Level]]]
        (for [[k {s :schema}] shared-table :when s]
          [k {:optional true} s])))

(def initial-world
  (let [lv (update-vals level-table :default)]
    (assoc (update-vals shared-table :default)
      :levels (zipmap dims (repeat lv)))))

(defn snapshot
  "Returns what w stores of the keys of scope, :shared or :level."
  [w scope]
  (into {} (for [[k {store :store s :scope}] world
                 :when (and store (= scope s))]
             [k (store (k w) w)])))

(defn- rebase-ticks [tick w]
  (let [t (long tick)
        due (fn [[dt s]] [(+ t (long dt)) s])]
    (update w :block-ticks
            #(into (i/int-map) (map due) %))))

(defn- loaded-of [table m]
  (into {} (for [[k {load :load default :default}] table
                 :when load]
             [k (if (contains? m k) (load (k m)) default)])))

(defn shared-of
  "Returns the shared keys of world a snapshot's meta holds."
  [m]
  (loaded-of shared-table m))

(defn level-of
  "Returns the level keys of world a snapshot's level meta holds.
  Its block ticks come due at their saved delay after tick."
  [tick m]
  (rebase-ticks tick (loaded-of level-table m)))

(def profile
  {:inventory    {:default {}}
   :held-slot    {:default 0}
   :spawn        {}
   :forced-spawn {}
   :stats        {:default {}}
   :ender-items  {:default []}
   :pos          {:store (fn [p] [(v/x p) (v/y p) (v/z p)])}
   :yaw          {:default 0.0}
   :pitch        {:default 0.0}
   :on-ground    {:default true :store boolean}
   :dimension    {:default :overworld}})

(defn- profile-kept? [player k]
  (or (some? (get player k))
      (contains? (profile k) :default)))

(defn- profile-value [player k {store :store default :default}]
  (let [v (get player k)]
    (if (some? v) ((or store identity) v) default)))

(defn profile-of
  "Returns what a profile keeps of player, who is in level dim."
  [player dim]
  (let [player (assoc player :dimension dim)]
    (into {} (for [[k spec] profile :when (profile-kept? player k)]
               [k (profile-value player k spec)]))))
