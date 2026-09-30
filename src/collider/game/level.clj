(ns collider.game.level
  "A level of the world: its view, its entities and its writes."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.tickers :as tickers]
            [clojure.data.int-map :as i]
            [collider.game.clock :as clock]
            [collider.game.entity :as entity]
            [collider.game.orb :as orb]
            [collider.game.schedule :as schedule]
            [collider.game.schema :as schema]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.dimension :as dimension]
            [collider.world.env.weather :as weather]
            [collider.world.light :as light]
            [collider.world.neighbors :as neighbors])
  (:import (clojure.lang MapEntry)))

(set! *warn-on-reflection* true)

(defn player-entries
  "Returns [eid entity] of the players world holds, by eid."
  [world]
  (let [entities (:entities world)
        entry (fn [eid]
                (when-let [e (get entities eid)]
                  (MapEntry/create eid e)))]
    (into [] (keep entry) (sort (vals (:players world))))))

(defn advance
  "Returns the world one tick older."
  [world]
  (cond-> (update world :tick inc)
          (clock/advancing? world)
          (update :clocks clock/advanced)))

(defn- made-bounds [dim]
  (let [t (dimension/type-of dim)
        lo (long (:min-y t chunk/min-y))]
    {:min-y lo
     :max-y (+ lo (long (:height t 384)) -1)
     :sky? (:has-skylight t true)
     :dim dim}))

(def ^:private dim-bounds
  (into {} (map (fn [dim] [dim (delay (made-bounds dim))]))
        schema/dims))

(defn bounds
  "Returns the height range and the sky of dimension dim."
  [dim]
  (if-let [b (get dim-bounds dim)] @b (made-bounds dim)))

(defn level
  "Returns the level dim of world.
  It also holds the shared keys of world and the shape of
  its dimension."
  [world dim]
  (let [part (get-in world [:levels dim])
        lv (as-> (dissoc! (transient world) :levels) t
             (reduce-kv assoc! t part)
             (persistent! (reduce-kv assoc! t (bounds dim))))
        lv (if-let [m (meta world)] (with-meta lv m) lv)]
    (if-let [t (::types (meta part))]
      (vary-meta lv assoc ::types t)
      lv)))

(def ^:private shared-out
  (into schema/level-keys [:dim :min-y :max-y :sky? :server]))

(defn- level-part [lv]
  (persistent!
    (reduce (fn [m k]
              (if-let [e (find lv k)] (assoc! m k (val e)) m))
            (transient {}) schema/level-keys)))

(defn- transient-of [t m] (or t (transient m)))

(defn- got [m k] (get m k ::none))

(defn- part-of
  "Returns the level keys of lv as a level part, old when they match."
  [old lv]
  (if (nil? old)
    (level-part lv)
    (let [put (fn [t k]
                (let [v (got lv k)]
                  (cond (identical? v (got old k)) t
                        (identical? v ::none)
                        (dissoc! (transient-of t old) k)
                        :else (assoc! (transient-of t old) k v))))
          t (reduce put nil schema/level-keys)]
      (if t (persistent! t) old))))

(defn- typed-part [part lv]
  (let [t (::types (meta lv))]
    (if (or (nil? t) (identical? t (::types (meta part)))
            (not (identical? (:entities part) (nth t 0))))
      part
      (vary-meta part assoc ::types t))))

(def ^:private bound-keys [:dim :min-y :max-y :sky? :server])

(defn- shared-count
  "Returns how many keys of lv are shared keys of the world."
  ^long [lv part]
  (reduce (fn [^long n k] (if (contains? lv k) (dec n) n))
          (- (count lv) (count part)) bound-keys))

(defn- shared-into
  "Returns world with the shared keys of lv, or nil if one is gone."
  [world lv]
  (let [put (fn [t k v]
              (let [v' (if (identical? :levels k) v (got lv k))]
                (cond (identical? v v') t
                      (identical? v' ::none) (reduced ::none)
                      :else (assoc! (transient-of t world) k v'))))
        t (reduce-kv put nil world)]
    (cond (nil? t) world
          (identical? ::none t) nil
          :else (persistent! t))))

(defn- same-shared-keys? [world lv part]
  (= (dec (count world)) (shared-count lv part)))

(defn- split-off
  "Returns the shared keys of lv as a world with part as level dim."
  [lv levels dim part]
  (assoc (persistent! (reduce dissoc! (transient lv) shared-out))
         :levels (assoc levels dim part)))

(defn with-level
  "Returns world with level dim replaced by lv.
  The keys of lv that are not level keys become the shared part
  of world."
  [world dim lv]
  (let [levels (:levels world)
        old (get levels dim)
        part (typed-part (part-of old lv) lv)
        w (when (same-shared-keys? world lv part)
            (shared-into world lv))]
    (cond (nil? w) (split-off lv levels dim part)
          (identical? part old) w
          :else (assoc w :levels (assoc levels dim part)))))

(defn- vacant? [m]
  (reduce-kv (fn [_ _ _] (reduced false)) true m))

(defn idle?
  "Returns true when level lv holds nothing a tick could change.
  Such a level has no chunks, no entities and no chunk on its way."
  [lv]
  (and (zero? (count (:chunks lv))) (vacant? (:entities lv))
       (empty? (:loading lv)) (vacant? (:unknown lv))))

(defn dim-of
  "Returns the dimension whose level holds entity eid, or nil."
  [world eid]
  (when (integer? eid)
    (let [holds? (fn [dim]
                   (-> (get-in world [:levels dim :entities])
                       (contains? eid)))]
      (some #(when (holds? %) %) schema/dims))))

(defn by-type
  "Returns the eids of entities by type."
  [entities]
  (persistent!
    (reduce-kv (fn [m eid e]
                 (let [t (:type e)]
                   (assoc! m t (conj (get m t (i/int-set)) eid))))
               (transient {}) entities)))

(defn types-by
  "Returns the eids of lv by type as its apply left them, or nil."
  [lv]
  (let [t (::types (meta lv))]
    (when (and t (identical? (:entities lv) (nth t 0)))
      (nth t 1))))

(defn with-types
  "Returns lv with types, its eids by type, kept as its index."
  [lv types]
  (vary-meta lv assoc ::types [(:entities lv) types]))

(defn- ids-of [types ts]
  (reduce (fn [acc t]
            (if-let [s (get types t)] (if acc (i/union acc s) s) acc))
          nil ts))

(defn of-types
  "Returns [eid entity] of the entities of lv whose type is in ts.
  They come by eid."
  [lv ts]
  (let [es (:entities lv)
        ids (ids-of (or (types-by lv) (by-type es)) ts)
        entry (fn [eid] (MapEntry/create eid (get es eid)))]
    (into [] (map entry) ids)))

(defn holds-types?
  "Returns true when lv holds an entity whose type is in set ts."
  [lv ts]
  (let [held? (fn [_ t _] (if (contains? ts t) (reduced true) false))]
    (if-let [types (types-by lv)]
      (reduce-kv held? false types)
      (reduce-kv (fn [_ _ e] (held? nil (:type e) nil))
                 false (:entities lv)))))

(def ^:private player-type #{:player})

(defn server-view
  "Returns the shared keys of world with all players as entities."
  [world]
  (let [players (mapcat #(of-types (val %) player-type))]
    (assoc (dissoc world :levels)
      :entities (into (i/int-map) players (:levels world)))))

(defn update-entity
  "Returns w with entity eid put through f, when w holds it."
  [w eid f & args]
  (if (get-in w [:entities eid])
    (clojure.core/apply update-in w [:entities eid] f args)
    w))

(defn- kind-changed? [old st]
  (and (be/kind old)
       (not= (block/block-of old) (block/block-of (long st)))
       (not= (be/kind old) (be/kind (long st)))))

(defn- openers-outside [openers id]
  (into {} (remove #(= id (chunk/block-chunk (key %)))) openers))

(defn- drop-block-entity [w pos]
  (-> w
      (update-in [:block-entities (chunk/block-chunk pos)] dissoc pos)
      (update :openers dissoc pos)
      (update :tickers tickers/without pos)))

(defn- reticked [w pos]
  (let [e (be/at w pos)
        st (chunk/chunks-get-block (:chunks w) pos)]
    (update w :tickers tickers/bound pos
            (and e (tickers/ticks? e st)))))

(defn- made-block-entity [w pos k]
  (if (be/at w pos)
    w
    (assoc-in w [:block-entities (chunk/block-chunk pos) pos]
              (be/made k (:tick w)))))

(defn- block-entity-changed [w [pos old st]]
  (let [k (be/kind st)]
    (cond-> w
      (kind-changed? old st) (drop-block-entity pos)
      k (made-block-entity pos k)
      (or k (be/kind old)) (reticked pos))))

(defn- block-entities-changed [w real]
  (reduce block-entity-changed w real))

(defn- real-changes [w changes]
  (chunk/changed (:chunks w) w changes))

(defn level-ctx
  "Returns what the block rules of level w read besides its blocks.
  When given, base stands for the tick."
  ([w] (level-ctx w (:tick w)))
  ([w base]
   (merge (select-keys w weather/fields)
          {:rules (:rules w)
           :dim (:dim w)
           :tick (long base)
           :time-of-day (clock/day-ticks w)
           :players (mapv (comp :pos val) (player-entries w))})))

(defn- add-changed [ev events]
  (reduce-kv (fn [ev k es] (update ev k #(if % (into % es) es)))
             (or ev (i/int-map)) (chunk/by-chunk events)))

(defn- ticks-added [w ticks]
  (reduce-kv (fn [w k es]
               (update w k schedule/add-all
                       (mapv (fn [[_ at id ty]] [at id ty]) es)))
             w (group-by first ticks)))

(defn- unquiet [changes quiet]
  (if (seq quiet)
    (let [quiet (set quiet)]
      (filterv #(not (quiet (first %))) changes))
    changes))

(defn- with-changes [w real ticks quiet]
  (let [told (unquiet real quiet)]
    (cond-> (-> w
                (update :chunks chunk/editable)
                (update :chunks chunk/chunks-set-blocks real 2)
                (update :chunks light/relight-batch real
                        (:sky? w true))
                (update :chunks chunk/frozen)
                (block-entities-changed real)
                (ticks-added ticks))
      (seq told) (update :changed-blocks add-changed told))))

(defn- settled
  ([w changes ticks] (settled w changes ticks nil))
  ([w changes ticks quiet]
   (let [real (real-changes w changes)]
     (if (and (empty? real) (empty? ticks))
       w
       (with-changes w real ticks quiet)))))

(defn- apply-set-blocks [w changes]
  (let [s (neighbors/set-blocks (:chunks w) (level-ctx w) changes)
        changes (mapv (fn [[p st]] [p st]) (:records s))]
    (settled w changes (:ticks s))))

(defn- drop-entities [es id]
  (reduce-kv (fn [es eid e]
               (if (schema/chunk-entity? id e) (dissoc es eid) es))
             es es))

(defn- spawned [w spec]
  (let [eid (long (:next-eid w 1000000))]
    (-> w
        (assoc-in [:entities eid]
                  (entity/of (assoc spec :born (:tick w))))
        (assoc :next-eid (inc eid)))))

(defn- block-or-zero ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y)
    (chunk/chunks-get-block chunks p)
    0))

(defn- block-tick [w at id]
  (let [p (chunk/id->block-pos id)
        st (block-or-zero (:chunks w) p)]
    (update w :block-ticks schedule/add at id (block/block-of st))))

(defn set-blocks
  "Returns level w after the delta [:set-blocks changes ticks quiet].
  Without ticks the changes update their neighbours first."
  [w [_ changes ticks quiet]]
  (if ticks
    (settled w changes ticks quiet)
    (apply-set-blocks w changes)))

(defn spawn-entity
  "Returns level w after the delta [:spawn-entity spec]."
  [w [_ spec]]
  (spawned w spec))

(defn xp-award
  "Returns level w after the delta [:xp-award pos amount salt dir]."
  [w [_ pos amount salt roughly]]
  (let [t (:tick w)]
    (orb/awarded w spawned pos (or roughly [0.0 0.0 0.0])
                 (long amount) #(random/of-key t pos salt %))))

(defn schedule-ticks
  "Returns level w after the delta [:schedule-ticks at-ids]."
  [w [_ at-ids]]
  (reduce (fn [w [at ids]]
            (reduce #(block-tick %1 at %2) w ids))
          w at-ids))

(defn openers
  "Returns level w after the delta [:openers pos step]."
  [w [_ pos step]]
  (let [n (+ (long (get-in w [:openers pos] 0)) (long step))]
    (if (zero? n)
      (update w :openers dissoc pos)
      (assoc-in w [:openers pos] n))))

(defn shulker-anim
  "Returns level w after the delta [:shulker-anim pos a]."
  [w [_ pos a]]
  (if a
    (let [base {:progress (float 0.0)}]
      (update-in w [:shulker-anim pos] #(merge base % a)))
    (update w :shulker-anim dissoc pos)))

(defn add-chunk
  "Returns level w after the delta [:add-chunk id c]."
  [w [_ id c]]
  (if (contains? (:chunks w) id) w (update w :chunks assoc id c)))

(defn restore-chunk
  "Returns level w after the delta [:restore-chunk id payload]."
  [w [_ id payload]]
  (if (contains? (:chunks w) id)
    (update w :loading disj id)
    (let [[fresh n] (schema/refreshed w payload)]
      (schema/with-chunk (assoc w :next-eid n) id fresh))))

(defn- tickers-outside [w id]
  (reduce tickers/without (:tickers w)
          (keys (get-in w [:block-entities id]))))

(defn unload-chunk
  "Returns level w after the delta [:unload-chunk id]."
  [w [_ id]]
  (let [id (long id)]
    (-> w
        (assoc :tickers (tickers-outside w id))
        (update :chunks dissoc id)
        (update :block-entities dissoc id)
        (update :openers openers-outside id)
        (update :entities drop-entities id)
        (update :block-ticks schedule/dropped id)
        (update :fluid-ticks schedule/dropped id)
        (update :unknown dissoc id)
        (update :stored (fnil conj (i/int-set)) id))))

(defn set-block-entity
  "Returns level w after the delta [:set-block-entity pos e]."
  [w [_ pos e]]
  (if (and e (be/kind (chunk/chunks-get-block (:chunks w) pos)))
    (-> w
        (assoc-in [:block-entities (chunk/block-chunk pos) pos] e)
        (reticked pos))
    (drop-block-entity w pos)))

(defn advance-weather
  "Returns level w after the delta [:advance-weather m].
  Without m the weather takes its own next step."
  [w [_ m]]
  (reduce-kv (fn [w k v] (if (= v (get w k)) w (assoc w k v)))
             w (or m (weather/advance w))))
