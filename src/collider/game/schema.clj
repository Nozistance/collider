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

(defn- entity-chunk ^long [e]
  (let [p (:pos e)]
    (chunk/pos->id (bit-shift-right (long (Math/floor (v/x p))) 4)
                   (bit-shift-right (long (Math/floor (v/z p))) 4))))

(defn- chunk-ticks [w ^long id]
  (let [t (long (:tick w 0))]
    (into [] (keep (fn [[at bids]]
                     (let [ps (into [] (comp (filter #(= id (chunk/block-id-chunk %))) (map chunk/id->block-pos)) bids)]
                       (when (seq ps) [(- (long at) t) ps]))))
          (:block-ticks w))))

(defn chunk-payload
  "Returns chunk id with its block entities, the entities in it except players,
   and its block ticks as delays from now."
  [w id]
  (let [id (long id)]
    {:chunk          (get (:chunks w) id)
     :block-entities (into {} (get-in w [:block-entities id]))
     :entities       (into {} (keep (fn [[eid e]]
                                      (when (and (not= :player (:type e)) (= id (entity-chunk e)))
                                        [eid (plain-entity e)])))
                           (:entities w))
     :ticks          (chunk-ticks w id)}))

(defn- ticks-back [bt ^long t ticks]
  (reduce (fn [bt [dt ps]]
            (update bt (+ t (max 1 (long dt))) (fnil into (i/int-set)) (map chunk/block-pos->id ps)))
          bt ticks))

(defn with-chunk
  "Returns w with the saved chunk id put back. Its block ticks come due after
   the delays they were saved with."
  [w id {:keys [chunk block-entities entities ticks]}]
  (let [id (long id)]
    (cond-> (-> w
                (update :chunks assoc id chunk)
                (update :entities into (map (fn [[eid e]] [(long eid) (entity/of e)])) entities)
                (update :block-ticks ticks-back (long (:tick w 0)) ticks)
                (update :loading disj id))
            (seq block-entities)
            (assoc-in [:block-entities id] (into {} (map (fn [[p e]] [(vec p) e])) block-entities)))))

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
  {:tick               {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0))}
   :time-ms            {:default 0}
   :time-of-day        {:default 0 :store (fn [v _] v) :load identity}
   :next-eid           {:default 1000000 :store (fn [v _] v) :load identity}
   :rules              {:default rules/defaults :store (fn [v _] v) :load #(merge rules/defaults %)}
   :profiles           {:default {} :store store-profiles :load identity}
   :chunks             {:default chunk/no-chunks :load identity}
   :entities           {:default (i/int-map) :load identity}
   :block-ticks        {:default (i/int-map) :load identity}
   :block-entities     {:default (i/int-map) :load identity}
   :stored             {:default (i/int-set)}
   :loading            {:default (i/int-set)}
   :world-spawn        {:default [24 4 8] :store (fn [v _] v) :load identity}
   :clear-weather-time {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0))}
   :rain-time          {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0))}
   :thunder-time       {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0))}
   :raining?           {:default false :store (fn [v _] (boolean v)) :load boolean}
   :thundering?        {:default false :store (fn [v _] (boolean v)) :load boolean}
   :rain-level         {:default 0.0 :store store-rain-level :load double}
   :o-rain-level       {:default 0.0 :store store-rain-level :load double}
   :thunder-level      {:default 0.0 :store store-thunder-level :load double}
   :o-thunder-level    {:default 0.0 :store store-thunder-level :load double}
   :container-rechecks {:default {}}
   :shulker-anim       {:default {}}
   :players            {:default {}}
   :listed             {:default {}}})

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
