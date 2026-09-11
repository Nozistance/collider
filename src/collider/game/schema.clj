(ns collider.game.schema
  (:require [clojure.data.int-map :as i]
            [collider.game.entity :as entity]
            [collider.game.rules :as rules]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- plain-entity [e]
  (-> (into {} e)
      (dissoc :track)
      (update :pos #(some-> % vec))
      (update :vel #(some-> % vec))))

(defn- store-entities [es _]
  (into {} (keep (fn [[eid e]] (when (not= :player (:type e)) [eid (plain-entity e)]))) es))

(defn- load-entities [es]
  (into (i/int-map) (map (fn [[eid e]] [(long eid) (entity/of e)])) es))

(defn- store-ticks [ticks world]
  (let [t (long (:tick world 0))]
    (mapv (fn [[k s]] [(- (long k) t) (mapv chunk/id->block-pos s)]) ticks)))

(defn- load-ticks [ticks]
  (into (i/int-map)
        (for [[dt ps] (group-by (fn [[dt _]] (max 1 (long dt))) ticks)]
          [dt (into (i/int-set) (map chunk/block-pos->id) (mapcat second ps))])))

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
  {:tick        {:default 0}
   :time-ms     {:default 0}
   :time-of-day {:default 0 :store (fn [v _] v) :load identity}
   :next-eid    {:default 1000000 :store (fn [v _] v) :load identity}
   :rules       {:default rules/defaults :store (fn [v _] v) :load #(merge rules/defaults %)}
   :profiles    {:default {} :store store-profiles :load identity}
   :chunks      {:default (i/int-map) :store (fn [v _] (into {} v)) :load #(into (i/int-map) %)}
   :entities    {:default (i/int-map) :store store-entities :load load-entities}
   :block-ticks {:default (i/int-map) :store store-ticks :load load-ticks}
   :block-entities {:default (i/int-map)
                    :store (fn [v _] (into {} (map (fn [[k m]] [k (into {} m)])) v))
                    :load #(into (i/int-map) (map (fn [[k m]] [(long k) (into {} (map (fn [[p e]] [(vec p) e])) m)])) %)}
   :world-spawn {:default [24 4 8] :store (fn [v _] v) :load identity}
   :clear-weather-time {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0))}
   :rain-time      {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0))}
   :thunder-time   {:default 0 :store (fn [v _] (long (or v 0))) :load #(long (or % 0))}
   :raining?       {:default false :store (fn [v _] (boolean v)) :load boolean}
   :thundering?    {:default false :store (fn [v _] (boolean v)) :load boolean}
   :rain-level     {:default 0.0 :store store-rain-level :load double}
   :o-rain-level   {:default 0.0 :store store-rain-level :load double}
   :thunder-level  {:default 0.0 :store store-thunder-level :load double}
   :o-thunder-level {:default 0.0 :store store-thunder-level :load double}
   :container-rechecks {:default {}}
   :shulker-anim {:default {}}
   :players     {:default {}}
   :listed      {:default {}}})

(def initial-world (update-vals world :default))
(defn snapshot [w]
  (into {} (for [[k {store :store}] world :when store]
             [k (store (k w) w)])))

(defn world-of [m]
  (into {} (for [[k {load :load default :default}] world :when load]
             [k (if (contains? m k) (load (k m)) default)])))

(def profile
  {:inventory {:default {}}
   :held-slot {:default 0}
   :spawn     {}
   :forced-spawn {}
   :stats     {:default {}}
   :ender-items {:default []}
   :pos       {:store (fn [p] [(v/x p) (v/y p) (v/z p)])}
   :yaw       {:default 0.0}
   :pitch     {:default 0.0}
   :on-ground {:default true :store boolean}})

(defn profile-of [player]
  (into {} (for [[k {store :store default :default}] profile
                 :let [val (get player k)]
                 :when (or (some? val) (contains? (profile k) :default))]
             [k (if (some? val) ((or store identity) val) default)])))
