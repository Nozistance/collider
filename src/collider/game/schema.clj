(ns collider.game.schema
  "Fields of the world and of a player profile, each with its default and
   the way it is kept in a snapshot. initial-world, snapshot, world-of and
   the profile of a player that leaves are all derived from these tables."
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

(defn- store-ticks
  "Block ticks relative to the current tick, positions as [x y z]."
  [ticks world]
  (let [t (long (:tick world 0))]
    (mapv (fn [[k s]] [(- (long k) t) (mapv chunk/id->block-pos s)]) ticks)))

(defn- load-ticks
  "Ticks due before the save (dt <= 0) wake on the first tick."
  [ticks]
  (into (i/int-map)
        (for [[dt ps] (group-by (fn [[dt _]] (max 1 (long dt))) ticks)]
          [dt (into (i/int-set) (map chunk/block-pos->id) (mapcat second ps))])))

(declare profile-of)

(defn- store-profiles
  "Profiles with the players online folded in: a save while they play must
   not lose where they are."
  [profiles world]
  (into profiles
        (for [[_ e] (:entities world) :when (and (= :player (:type e)) (:name e))]
          [(:name e) (profile-of e)])))

(def world
  "Field to {:default v, :store (fn [v world]), :load (fn [v])}. A field
   without :store is not in the snapshot."
  {:tick        {:default 0}
   :time-ms     {:default 0}
   :time-of-day {:default 0 :store (fn [v _] v) :load identity}
   :next-eid    {:default 1000000 :store (fn [v _] v) :load identity}
   :rules       {:default rules/defaults :store (fn [v _] v) :load #(merge rules/defaults %)}
   :profiles    {:default {} :store store-profiles :load identity}
   :chunks      {:default (i/int-map) :store (fn [v _] (into {} v)) :load #(into (i/int-map) %)}
   :entities    {:default (i/int-map) :store store-entities :load load-entities}
   :block-ticks {:default (i/int-map) :store store-ticks :load load-ticks}
   :players     {:default {}}
   :listed      {:default {}}})

(def initial-world (update-vals world :default))

(defn snapshot
  "Snapshot map of the world: the fields with :store, stored."
  [w]
  (into {} (for [[k {store :store}] world :when store]
             [k (store (k w) w)])))

(defn world-of
  "World fields from a snapshot map; a field the map lacks gets its default."
  [m]
  (into {} (for [[k {load :load default :default}] world :when load]
             [k (if (contains? m k) (load (k m)) default)])))

(def profile
  "Player field to {:default v, :store (fn [v])}. These fields live on in
   :profiles when the player leaves and come back when they join."
  {:inventory {:default {}}
   :held-slot {:default 0}
   :spawn     {}
   :stats     {:default {}}
   :pos       {:store (fn [p] [(v/x p) (v/y p) (v/z p)])}
   :yaw       {:default 0.0}
   :pitch     {:default 0.0}
   :on-ground {:default true :store boolean}})

(defn profile-of
  "Profile of a player entity: each field stored, or its default when the
   player has none and a default exists."
  [player]
  (into {} (for [[k {store :store default :default}] profile
                 :let [val (get player k)]
                 :when (or (some? val) (contains? (profile k) :default))]
             [k (if (some? val) ((or store identity) val) default)])))
