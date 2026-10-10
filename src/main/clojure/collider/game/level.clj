(ns collider.game.level
  "A level of the world with its view and its entities."
  (:require [collider.data.long-map :as lm]
            [collider.game.clock :as clock]
            [collider.game.entity.stamp :as stamp]
            [collider.game.schema :as schema]
            [collider.world.env.dimension :as dimension]
            [collider.world.env.weather :as weather]
            [collider.world.gen :as gen])
  (:import (clojure.lang MapEntry)))

(set! *warn-on-reflection* true)

(defn- entries [world]
  (let [entities (:entities world)
        entry (fn [eid]
                (when-let [e (get entities eid)]
                  (MapEntry/create eid e)))]
    (into [] (keep entry) (sort (vals (:players world))))))

(defn- listed [world]
  (when-let [[ps es v] (::players (meta world))]
    (when (and (identical? ps (:players world))
               (identical? es (:entities world)))
      v)))

(defn player-entries
  "Returns [eid entity] of the players world holds, by eid."
  [world]
  (or (listed world) (entries world)))

(defn with-players
  "Returns lv that keeps its player entries as long as its players
  and its entities stay the same."
  [lv]
  (if (listed lv)
    lv
    (vary-meta lv assoc ::players
               [(:players lv) (:entities lv) (entries lv)])))

(defn advance
  "Returns the world one tick older."
  [world]
  (cond-> (update world :tick inc)
          (clock/advancing? world)
          (update :clocks clock/advanced)))

(defn- made-bounds [dim]
  (assoc (dimension/bounds dim)
         :sky? (:has-skylight (dimension/type-of dim) true)
         :dim dim))

(def ^:private dim-bounds
  (into {} (map (fn [dim] [dim (delay (made-bounds dim))]))
        schema/dims))

(defn bounds
  "Returns the height range and the sky of dimension dim."
  [dim]
  (if-let [b (get dim-bounds dim)] @b (made-bounds dim)))

(defn- bounded [lv dim]
  (if (contains? lv :dim) lv (reduce-kv assoc lv (bounds dim))))

(defn- kept [lv k v]
  (if (or (identical? :levels k) (identical? v (get lv k ::none)))
    lv
    (assoc lv k v)))

(defn- holding [world lv dim]
  (reduce-kv kept (bounded lv dim) world))

(defn level
  "Returns the level dim of world.
  It also holds the shared keys of world and the shape of
  its dimension."
  [world dim]
  (holding world (get (:levels world) dim {}) dim))

(defn synced
  "Returns world whose levels hold its shared keys as they are."
  [world]
  (let [held (fn [w dim lv]
               (let [lv' (holding world lv dim)]
                 (if (identical? lv lv')
                   w
                   (assoc-in w [:levels dim] lv'))))]
    (reduce-kv held world (:levels world))))

(defn- changed [world lv]
  (let [top (fn [ks k v]
              (if (or (identical? :levels k)
                      (identical? v (get lv k v)))
                ks
                (conj ks k)))
        added (fn [ks k]
                (if (or (contains? world k) (not (contains? lv k)))
                  ks
                  (conj ks k)))]
    (reduce added (reduce-kv top [] world) schema/shared-keys)))

(defn- refreshed [lv from ks]
  (reduce #(assoc %1 %2 (get from %2)) lv ks))

(defn with-level
  "Returns world with lv as its level dim.
  The shared keys that lv changed go to the top of world and to its
  other levels."
  [world dim lv]
  (let [ks (changed world lv)
        w (refreshed world lv ks)
        lv (if (contains? lv :dim) lv (holding w lv dim))
        levels (assoc (:levels world) dim lv)
        other (fn [ls d l]
                (if (identical? d dim) ls
                    (assoc ls d (refreshed l lv ks))))]
    (assoc w :levels (if (zero? (count ks))
                       levels
                       (reduce-kv other levels levels)))))

(defn shared-kept!
  "Throws when a level of world holds a shared key unlike its top."
  [world]
  (doseq [[dim lv] (:levels world)
          [k v] world
          :when (and (not= :levels k)
                     (not (identical? v (get lv k))))]
    (throw (ex-info "a level strayed from the shared keys"
                    {:dim dim :key k}))))

(defn idle?
  "Returns true when level lv holds nothing a tick could change."
  [lv]
  (and (zero? (count (:chunks lv))) (lm/empty? (:entities lv))
       (empty? (:loading lv)) (lm/empty? (:unknown lv))))

(defn dim-of
  "Returns the dimension whose level holds entity eid, or nil."
  [world eid]
  (when (integer? eid)
    (let [holds? (fn [dim]
                   (-> (get-in world [:levels dim :entities])
                       (contains? eid)))]
      (some #(when (holds? %) %) schema/dims))))

(defn by-type
  [entities]
  (persistent!
    (reduce-kv (fn [m eid e]
                 (let [t (:type e)]
                   (assoc! m t (conj (get m t (lm/long-set)) eid))))
               (transient {}) entities)))

(defn- untyped [types t eid]
  (let [s (disj (get types t) eid)]
    (if (seq s) (assoc types t s) (dissoc types t))))

(defn retyped
  "Returns index types after entity eid goes from a to b. A nil a or
  b means eid is absent there."
  [types eid a b]
  (if (and a b (identical? (:type a) (:type b)))
    types
    (cond-> types
      a (untyped (:type a) eid)
      b (update (:type b) (fnil conj (lm/long-set)) eid))))

(defn- caught [types es now]
  (lm/diff es now retyped types))

(defn types-by
  "Returns the eids of level lv by type."
  [lv]
  (stamp/value lv ::types by-type caught))

(defn with-types
  "Returns lv with types, its eids by type, kept as its index."
  [lv types]
  (stamp/with lv ::types types))

(defn typed
  "Returns level lv that keeps its eids by type for its entities."
  [lv]
  (stamp/kept lv ::types by-type caught))

(defn- with-ids [types acc t]
  (if-let [s (get types t)] (if acc (lm/union acc s) s) acc))

(defn- ids-of [types ts]
  (reduce #(with-ids types %1 %2) nil ts))

(defn of-types
  "Returns [eid entity] of the entities of lv whose type is in ts.
  They come by eid."
  [lv ts]
  (let [es (:entities lv)
        ids (ids-of (types-by lv) ts)
        entry (fn [eid] (MapEntry/create eid (get es eid)))]
    (into [] (map entry) ids)))

(defn holds-types?
  [lv ts]
  (let [held? (fn [_ t _] (if (contains? ts t) (reduced true) false))]
    (reduce-kv held? false (types-by lv))))

(def ^:private player-type #{:player})

(defn server-view
  "Returns the shared keys of world with all players as entities, and
  as :quits the players who left in this tick."
  [world]
  (let [players (mapcat #(of-types (val %) player-type))
        quits (mapcat #(get-in (val %) [:input :quits]))]
    (assoc (dissoc world :levels)
      :entities (into (lm/long-map) players (:levels world))
      :quits (into [] quits (:levels world)))))

(defn update-entity
  "Returns w with entity eid put through f, when w holds it."
  [w eid f & args]
  (if (get-in w [:entities eid])
    (apply update-in w [:entities eid] f args)
    w))

(defn level-ctx
  "Returns what the block rules of level w read besides its blocks,
  at tick t when given."
  ([w] (level-ctx w (:tick w)))
  ([w t]
   (merge (select-keys w weather/fields)
          {:rules (:rules w)
           :dim (:dim w)
           :tick (long t)
           :time-of-day (clock/day-ticks w)
           :players (mapv (comp :pos val) (player-entries w))})))

(def ^:private ^:const unknown-timeout 1)

(defn read-absent
  "Returns the payload of a chunk read while it is absent.
  A saved chunk is read from the store at once. Any other chunk
  is generated."
  [world id]
  (or (when (contains? (:stored world) id)
        (when-let [read (:read-chunk world)]
          (read (:dim world) id)))
      {:chunk (gen/flat-chunk (:dim world))}))

(defn read-absent-deltas
  "Returns the deltas that put chunks read while absent in place.
  Their ticket keeps them one more tick."
  [payloads]
  (mapcat (fn [[id payload]]
            [[:restore-chunk id payload]
             [:chunk-ticket id unknown-timeout]])
          payloads))
