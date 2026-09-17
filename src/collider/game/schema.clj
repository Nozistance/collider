(ns collider.game.schema
  "Schema of the world map and of the player profile, with their stored forms."
  (:require [clojure.data.int-map :as i]
            [collider.game.entity :as entity]
            [collider.game.gamerules :as rules]
            [collider.vec :as v]
            [collider.world.chunk :as chunk])
  (:import (collider.java V3)))

(set! *warn-on-reflection* true)

(defn- plain [v] (if (instance? V3 v) (vec v) v))

(defn- plain-entity [e]
  (reduce-kv (fn [m k v] (assoc m k (plain v)))
             {} (dissoc (into {} e) :track)))

(defn chunk-entity?
  "Returns true when entity e belongs to chunk id. Players never do."
  [^long id e]
  (and (not= :player (:type e))
       (= id (chunk/pos-chunk (:pos e)))))

(defn chunk-tick?
  "Returns true when the block with packed id bid lies in chunk id."
  [^long id ^long bid]
  (= id (chunk/block-id-chunk bid)))

(defn- chunk-entities [w id]
  (into {} (keep (fn [[eid e]]
                   (when (chunk-entity? id e)
                     [eid (plain-entity e)])))
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
  "Returns chunk id with its block entities, the entities in it except players,
   and its block ticks as delays from now."
  [w id]
  (let [id (long id)]
    {:chunk          (get (:chunks w) id)
     :block-entities (into {} (get-in w [:block-entities id]))
     :entities       (chunk-entities w id)
     :ticks          (chunk-ticks w id)}))

(defn- ticks-back [bt ^long t ticks]
  (reduce (fn [bt [dt ps]]
            (update bt (+ t (max 1 (long dt)))
                    (fnil into (i/int-set))
                    (map chunk/block-pos->id ps)))
          bt ticks))

(defn- entity-entry [[eid e]] [(long eid) (entity/of e)])

(defn- block-entity-entry [[p e]] [(vec p) e])

(defn with-chunk
  "Returns w with the saved chunk id put back. Its block ticks come due after
   the delays they were saved with."
  [w id {:keys [chunk block-entities entities ticks]}]
  (let [id (long id)
        t (long (:tick w 0))
        bes (into {} (map block-entity-entry) block-entities)]
    (cond-> (-> w
                (update :chunks assoc id chunk)
                (update :entities into (map entity-entry) entities)
                (update :block-ticks ticks-back t ticks)
                (update :loading disj id))
      (seq block-entities) (assoc-in [:block-entities id] bes))))

(declare profile-of)

(defn- store-profiles [profiles world]
  (into profiles
        (for [[_ e] (:entities world) :when (and (= :player (:type e)) (:name e))]
          [(:name e) (profile-of e)])))

(defn- store-rain-level [_ world]
  (if (:raining? world) 1.0 0.0))

(defn- store-thunder-level [_ world]
  (if (and (:raining? world) (:thundering? world)) 1.0 0.0))

(def world
  {:tick               {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0)) :schema :int}
   :time-ms            {:default 0}
   :time-of-day        {:default 0 :store (fn [v _] v) :load identity :schema :int}
   :next-eid           {:default 1000000 :store (fn [v _] v) :load identity :schema :int}
   :rules              {:default rules/defaults :store (fn [v _] v) :schema :map
                        :load    #(merge rules/defaults
                                         (select-keys % (keys rules/defaults)))}
   :profiles           {:default {} :store store-profiles :load identity :schema [:map-of :string :map]}
   :chunks             {:default chunk/no-chunks :load identity}
   :entities           {:default (i/int-map) :load identity}
   :block-ticks        {:default (i/int-map) :load identity}
   :block-entities     {:default (i/int-map) :load identity}
   :stored             {:default (i/int-set)}
   :loading            {:default (i/int-set)}
   :world-spawn        {:default [24 4 8] :store (fn [v _] v) :load identity :schema [:tuple :int :int :int]}
   :clear-weather-time {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0)) :schema :int}
   :rain-time          {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0)) :schema :int}
   :thunder-time       {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0)) :schema :int}
   :raining?           {:default false :store (fn [v _] (boolean v)) :load boolean :schema :boolean}
   :thundering?        {:default false :store (fn [v _] (boolean v)) :load boolean :schema :boolean}
   :rain-level         {:default 0.0 :store store-rain-level :load double :schema number?}
   :o-rain-level       {:default 0.0 :store store-rain-level :load double :schema number?}
   :thunder-level      {:default 0.0 :store store-thunder-level :load double :schema number?}
   :o-thunder-level    {:default 0.0 :store store-thunder-level :load double :schema number?}
   :container-rechecks {:default {}}
   :shulker-anim       {:default {}}
   :players            {:default {}}
   :spawning           {:default (i/int-map)}
   :listed             {:default {}}})

(def Meta
  (into [:map {:closed true} [:format :int] [:stored {:optional true} [:fn set?]]]
        (for [[k {s :schema}] world :when s] [k {:optional true} s])))

(def initial-world (update-vals world :default))
(defn snapshot [w]
  (into {} (for [[k {store :store}] world :when store]
             [k (store (k w) w)])))

(defn- rebase-ticks [w]
  (let [t (long (:tick w 0))]
    (update w :block-ticks #(into (i/int-map) (map (fn [[dt s]] [(+ t (long dt)) s])) %))))

(defn world-of [m]
  (rebase-ticks
    (into {} (for [[k {load :load default :default}] world :when load]
               [k (if (contains? m k) (load (k m)) default)]))))

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
   :on-ground    {:default true :store boolean}})

(defn profile-of [player]
  (into {} (for [[k {store :store default :default}] profile
                 :let [val (get player k)]
                 :when (or (some? val) (contains? (profile k) :default))]
             [k (if (some? val) ((or store identity) val) default)])))
