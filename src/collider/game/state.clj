(ns collider.game.state
  "The world value and the application of deltas to it."
  (:refer-clojure :exclude [apply])
  (:require [collider.game.block.blockentity :as be]
            [collider.data :as data]
            [collider.world.env.dimension :as dimension]
            [clojure.core.reducers :as r]
            [collider.vec :as v]
            [clojure.data.int-map :as i]
            [clojure.set :as set]
            [collider.game.clock :as clock]
            [collider.game.entity :as entity]
            [collider.game.game-mode :as game-mode]
            [collider.game.orb :as orb]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.game.schedule :as schedule]
            [collider.game.schema :as schema]
            [collider.game.stack :as stack]
            [collider.game.deltas :as deltas]
            [collider.game.delta :as delta]
            [collider.log :as log]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.climb :as climb]
            [collider.world.light :as light]
            [collider.world.neighbors :as neighbors]
            [collider.world.space.spawn :as spawn]
            [collider.world.env.weather :as weather])
  (:import (clojure.lang MapEntry)
           (collider.game.deltas.record Deltas)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn player-entries
  "Returns [eid entity] of the players world holds, by eid."
  [world]
  (let [entities (:entities world)
        entry (fn [eid]
                (when-let [e (get entities eid)]
                  (MapEntry/create eid e)))]
    (into [] (keep entry) (sort (vals (:players world))))))

(def ^:private ^:const fold-leaf 64)

(def spawn-pos [24.5 4.0 8.5])

(def activation-radius 2)

(defn view-radius
  "Returns the view distance in chunks the config asks for.
  It stays between 2 and 32."
  ^long [world]
  (-> (long (get-in world [:config :view-distance] 7))
      (max 2)
      (min 32)))

(defn- loads-chunks? [world e]
  (and (= :player (:type e))
       (or (get-in world [:rules :spectators-generate-chunks] true)
           (not (game-mode/spectator? e)))))

(defn- player-chunks [world]
  (let [es (:entities world)
        at (fn [eid]
             (when-let [e (get es eid)]
               (when (loads-chunks? world e)
                 (chunk/pos-chunk (:pos e)))))]
    (into (i/int-set) (keep at) (vals (:players world)))))

(defn- zone-at [ids ^long r]
  (into (i/int-set)
        (mapcat (fn [id]
                  (let [[cx cz] (chunk/id->pos id)]
                    (chunk/around-ids (long cx) (long cz) r))))
        ids))

(defn- sim-radius ^long [world]
  (long (get-in world [:config :simulation-distance]
                activation-radius)))

(defn- compute-areas [world]
  (let [ps (player-chunks world)
        zone (zone-at ps (+ 2 (view-radius world)))
        s (sim-radius world)
        live? #(and (contains? zone %)
                    (contains? (:chunks world) %))
        in? #(into (i/int-set) (filter live?) %)
        absent (into (i/int-set)
                     (remove #(contains? (:chunks world) %)) zone)
        active (in? (zone-at ps s))
        broadcast (zone-at ps (inc (view-radius world)))]
    {:active active :ticking (in? (zone-at ps (inc s)))
     :broadcast broadcast :zone zone :absent absent
     :active-ids (vec active)}))

(defn- areas-key
  "Returns what the chunk areas of world are a function of.
  Block writes keep the shape of chunks, so they do not count."
  [world]
  {:player-chunks (player-chunks world)
   :shape (chunk/shape (:chunks world))
   :config (:config world) :rules (:rules world)
   :players (:players world) :entities (:entities world)})

(defn- same-shape? [k world]
  (and (identical? (:shape k) (chunk/shape (:chunks world)))
       (identical? (:config k) (:config world))
       (identical? (:rules k) (:rules world))))

(defn- same? [k world]
  (and (identical? (:entities k) (:entities world))
       (identical? (:players k) (:players world))
       (same-shape? k world)))

(defn- reusable? [k world ps]
  (and (same-shape? k world) (= (:player-chunks k) ps)))

(defn- fresh? [world had]
  (when had
    (let [k (::key (meta had))]
      (or (same? k world)
          (reusable? k world (player-chunks world))))))

(defn- areas [world]
  (let [had (:active-chunks world)]
    (if (fresh? world had) had (compute-areas world))))

(defn loaded-zone
  "Returns the ids of the chunks the players keep at full status.
  That zone reaches two rings past the view distance."
  [world]
  (:zone (areas world)))

(defn absent-chunks
  "Returns the ids of the loaded zone that world holds no chunk for."
  [world]
  (:absent (areas world)))

(defn active-chunks
  "Returns the chunks that run entity and random ticks."
  [world]
  (:active (areas world)))

(defn active-chunk-ids
  "Returns the active chunks as a vector, in the order of the set."
  [world]
  (:active-ids (areas world)))

(defn ticking-chunks
  "Returns the chunks that run scheduled block and fluid ticks.
  They reach one chunk further than the entity ticking ones."
  [world]
  (:ticking (areas world)))

(defn broadcast-chunks
  "Returns the chunks whose block changes reach the clients.
  They reach one ring past the view distance."
  [world]
  (:broadcast (areas world)))

(defn- kept [world had now]
  (let [k (::key (meta had))]
    (if (and had (reusable? k world (:player-chunks now)))
      had
      (compute-areas world))))

(defn cache-active-chunks
  "Returns the world with its chunk areas up to date.
  The areas follow its players and chunks. The key they were made
  from is metadata, so equal worlds stay equal."
  [world]
  (let [had (:active-chunks world)]
    (if (and had (same? (::key (meta had)) world))
      world
      (let [now (areas-key world)]
        (assoc world :active-chunks
               (with-meta (kept world had now) {::key now}))))))

(defn advance
  "Returns the world one tick older."
  [world]
  (cond-> (update world :tick inc)
          (clock/advancing? world)
          (update :clocks clock/advanced)))

(defn active-at?
  "Returns true when the chunk of block pos is in active."
  [active pos]
  (contains? active (chunk/pos-chunk pos)))

(defn active-id?
  "Returns true when the chunk of block id bid is in active."
  [active ^long bid]
  (contains? active (chunk/block-id-chunk bid)))

(defn offline-uuid
  "Returns the uuid an offline player of that name always gets."
  ^UUID [^String name]
  (let [s (str "OfflinePlayer:" name)]
    (UUID/nameUUIDFromBytes
     (String/.getBytes s StandardCharsets/UTF_8))))

(def initial-world schema/initial-world)

(defn- bounds [dim]
  (let [t (dimension/type-of dim)
        lo (long (:min-y t chunk/min-y))]
    {:min-y lo
     :max-y (+ lo (long (:height t 384)) -1)
     :sky? (:has-skylight t true)
     :dim dim}))

(def ^:private dim-bounds
  (into {} (map (fn [dim] [dim (delay (bounds dim))])) schema/dims))

(defn- bounds-of [dim]
  (if-let [b (get dim-bounds dim)] @b (bounds dim)))

(defn level
  "Returns the level dim of world.
  It also holds the shared keys of world and the shape of
  its dimension."
  [world dim]
  (let [part (get-in world [:levels dim])
        lv (as-> (dissoc! (transient world) :levels) t
             (reduce-kv assoc! t part)
             (persistent! (reduce-kv assoc! t (bounds-of dim))))
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

(defn idle?
  "Returns true when level lv holds nothing a tick could change.
  Such a level has no chunks, no entities and no chunk on its way."
  [lv]
  (and (zero? (count (:chunks lv))) (deltas/vacant? (:entities lv))
       (empty? (:loading lv)) (deltas/vacant? (:unknown lv))))

(defn dim-of
  "Returns the dimension whose level holds entity eid, or nil."
  [world eid]
  (when (integer? eid)
    (let [holds? (fn [dim]
                   (-> (get-in world [:levels dim :entities])
                       (contains? eid)))]
      (some #(when (holds? %) %) schema/dims))))

(defn- by-type [entities]
  (persistent!
    (reduce-kv (fn [m eid e]
                 (let [t (:type e)]
                   (assoc! m t (conj (get m t (i/int-set)) eid))))
               (transient {}) entities)))

(defn- types-by [lv]
  (let [t (::types (meta lv))]
    (when (and t (identical? (:entities lv) (nth t 0)))
      (nth t 1))))

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

(defn active-of-types
  "Returns the entries of type ts whose entity is in an active chunk."
  [world ts]
  (let [active (active-chunks world)
        live? (fn [[_ e]] (active-at? active (:pos e)))]
    (into [] (filter live?) (of-types world ts))))

(def ^:private player-type #{:player})

(defn server-view
  "Returns the shared keys of world with all players as entities."
  [world]
  (let [players (mapcat #(of-types (val %) player-type))]
    (assoc (dissoc world :levels)
      :entities (into (i/int-map) players (:levels world)))))

(defn- update-entity [w eid f & args]
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
      (update :openers dissoc pos)))

(defn- drop-block-entities [w real]
  (reduce (fn [w [pos old st]]
            (if (kind-changed? old st) (drop-block-entity w pos) w))
          w real))

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

(defn- add-block-events [ev events]
  (reduce-kv (fn [ev k es] (update ev k #(if % (into % es) es)))
             (or ev (i/int-map)) (chunk/by-chunk events)))

(defn- ticks-added [w ticks]
  (reduce (fn [w [k at id ty]] (update w k schedule/add at id ty))
          w ticks))

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
                (drop-block-entities real)
                (ticks-added ticks))
      (seq told) (update :block-events add-block-events told))))

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

(defn spawn-seed
  "Returns the random seed of a spawn by entity eid this tick."
  ^double [w eid]
  (random/of-longs (long (:tick w 0)) (long eid) (hash :spawn)))

(defn joins
  "Returns [:player-join eid name] for every player placed in d."
  [d]
  (for [[tag eid name] (:world d) :when (= :player-placed tag)]
    [:player-join eid name]))

(defn- new-player [name tick pos]
  {:type         :player :name name :uuid (offline-uuid name)
   :pos          pos :yaw 0.0 :pitch 0.0 :on-ground true
   :chunk-pos    nil :sent-chunks (i/int-set)
   :chunk-rate   9.0 :chunk-quota 0.0 :batches-unacked 0
   :batches-max  1
   :tracking (i/int-set) :track nil
   :inventory    {} :held-slot 0
   :sneaking?    false :sprinting? false :skin-parts 0 :ping 0
   :view-distance 2 :chunk-view nil
   :health       20.0
   :health-sent  20.0
   :keepalive-at tick :keepalive-pending? false})

(def ^:private border-edge 29999984)

(defn- in-border? [[x _ z]]
  (and (<= (- border-edge) (long x)) (< (long x) border-edge)
       (<= (- border-edge) (long z)) (< (long z) border-edge)))

(defn- respawn-dimension [w]
  (let [dim (:world-spawn-dimension w :overworld)]
    (if (some #{dim} schema/dims) dim :overworld)))

(defn- respawn-level [w]
  (let [dim (respawn-dimension w)
        server (or (:server w) (when (:levels w) w))]
    (cond (= dim (:dim w)) w
          server (level server dim)
          :else (select-keys (bounds dim) [:min-y]))))

(defn- centre-top [lv]
  [0 (max (long (:min-y lv chunk/min-y))
          (spawn/motion-blocking-height (:chunks lv {}) 0 0))
   0])

(defn spawn-turn
  "Returns [yaw pitch] of the world spawn."
  [w]
  (:world-spawn-turn w [0.0 0.0]))

(defn respawn-at
  "Returns the world spawn as players respawn at it.
  It moves inside the world border as the chunks of its level stand
  now."
  [w]
  (let [pos (vec (:world-spawn w [24 4 8]))]
    (if (in-border? pos) pos (centre-top (respawn-level w)))))

(defn- world-spawn-set [w [_ dim pos turn]]
  (assoc w :world-spawn (vec pos) :world-spawn-dimension dim
           :world-spawn-turn (mapv double turn)))

(defn- player-join [w eid name settings]
  (let [{:keys [pos dimension]} (get-in w [:profiles name])]
    (assoc-in w [:spawning eid]
              (cond-> {:name name :seed (spawn-seed w eid)
                       :dim (or dimension (respawn-dimension w))
                       :settings settings}
                pos (assoc :pos pos)))))

(def ^:private ^:const client-load-timeout 60)

(defn- load-awaited [e tick]
  (assoc e :loaded-at (+ (long tick) client-load-timeout)))

(defn- awaiting-join [e tick]
  (let [p (:pos e)]
    (assoc e :tp-target [(v/x p) (v/y p) (v/z p)] :tp-id 1
             :tp-at tick)))

(defn- moded [w e]
  (let [mode (game-mode/joining-mode w (:game-mode e))]
    (assoc e :game-mode mode
             :flying (game-mode/flying-in mode (:flying e)))))

(defn- player-placed [w eid name pos]
  (let [[yaw pitch] (spawn-turn w)
        fresh (merge (new-player name (:tick w) pos)
                     {:yaw yaw :pitch pitch}
                     (get-in w [:spawning eid :settings]))
        saved (dissoc (get-in w [:profiles name]) :dimension)
        e (entity/of (moded w (merge fresh saved)))
        tick (:tick w)]
    (-> w
        (assoc-in [:entities eid]
                  (-> e (awaiting-join tick) (load-awaited tick)
                      (assoc :born tick :xp-sent (:xp-total e 0))))
        (assoc-in [:players name] eid)
        (update :spawning dissoc eid))))

(defn- respawn-requested [w eid]
  (let [e (get-in w [:entities eid])]
    (if (and e (not (pos? (double (:health e))))
             (not (get-in w [:spawning eid])))
      (-> w
          (assoc-in [:spawning eid]
                    {:respawn? true :seed (spawn-seed w eid)
                     :dim :overworld})
          (update-entity eid load-awaited (:tick w)))
      w)))

(defn- drop-entities [es id]
  (reduce-kv (fn [es eid e]
               (if (schema/chunk-entity? id e) (dissoc es eid) es))
             es es))

(defn- unloaded [w id]
  (let [id (long id)]
    (-> w
        (update :chunks dissoc id)
        (update :block-entities dissoc id)
        (update :openers openers-outside id)
        (update :entities drop-entities id)
        (update :block-ticks schedule/dropped id)
        (update :fluid-ticks schedule/dropped id)
        (update :unknown dissoc id)
        (update :stored (fnil conj (i/int-set)) id))))

(defn- stored-profile [e dim]
  (schema/profile-of
    (-> e
        (update-in [:stats :custom/leave-game] (fnil inc 0))
        (update :inventory
                #(clojure.core/apply dissoc % (range 5))))
    dim))

(defn- forget-player [ps name eid]
  (if (= eid (get ps name)) (dissoc ps name) ps))

(defn- kept-profile [w name e]
  (let [dim (:dim w :overworld)]
    (assoc-in w [:profiles name] (stored-profile e dim))))

(defn- player-quit [w eid]
  (let [{:keys [name] :as e} (get-in w [:entities eid])]
    (cond-> (-> w
                (update :spawning dissoc eid)
                (update :entities dissoc eid)
                (update :players forget-player name eid))
            name (kept-profile name e))))

(def ^:private ^:table swords
  (delay (set (data/tag-values "item" "swords"))))

(defn- sword? [item]
  (contains? @swords item))

(defn- stopped-use [e]
  (assoc e :using-item? false :using nil))

(defn- switched [e ^long slot]
  (cond-> {:held-slot slot}
          (and (not= slot (long (or (:held-slot e) 0)))
               (not= :off (get-in e [:using :hand])))
          (assoc :using-item? false :using nil)))

(defn- wrap-degrees ^double [^double d]
  (let [r (rem d 360.0)]
    (cond (>= r 180.0) (- r 360.0)
          (< r -180.0) (+ r 360.0)
          :else r)))

(defn- held-item-of [e]
  (let [slot (+ 36 (long (or (:held-slot e) 0)))]
    (get-in e [:inventory slot :item])))

(defn- snapped [e rot]
  (if (and rot (held-item-of e))
    (assoc e :yaw (wrap-degrees (double (:yaw rot)))
             :pitch (wrap-degrees (double (:pitch rot))))
    e))

(def ^:private origin-keys [:pos :yaw :pitch :sneaking? :flying])

(defn use-origin
  "Returns the eye and look of the player behind an event.
  Only place and use events have one. Others give nil."
  [w [tag & args]]
  (let [rot (case tag
              :place (nth args 6 nil)
              :use-item (nth args 3 nil)
              nil)]
    (when (#{:place :use-item} tag)
      (when-let [e (get-in w [:entities (first args)])]
        (select-keys (snapped e rot) origin-keys)))))

(defn hand-slot
  "Returns the inventory slot the hand holds."
  ^long [e hand]
  (if (= :off hand) 45 (+ 36 (long (or (:held-slot e) 0)))))

(defn hand-stack
  "Returns the stack the player holds in hand."
  [e hand]
  (get-in e [:inventory (hand-slot e hand)]))

(def ^:private ^:const default-swing 6)

(defn swing-duration
  "Returns how many ticks the swing of the item in hand lasts."
  ^long [e hand]
  (let [stack (hand-stack e hand)]
    (long (get-in (data/items)
                  [(:item stack) :components :swing-animation
                   :duration]
                  default-swing))))

(defn- swing-free? [e ^long t]
  (if-let [at (:swing-at e)]
    (let [k (- t (long at))]
      (or (zero? k)
          (>= (dec k) (quot (swing-duration e (:swing-hand e)) 2))))
    true))

(defn swing-deltas
  "Returns the deltas of the arm swing of player eid.
  There are none while the arm is still busy with the swing before it.
  The player sees its own swing only when the server, not the client,
  started it."
  [eid e hand t self?]
  (when (swing-free? e (long t))
    (let [kind (if (= :off hand) :swing-off :swing)
          fx (out/animation eid kind)]
      (cond-> [[:merge-entity eid
                {:swing-at (long t) :swing-hand hand}]
               (out/all fx)]
        self? (conj (out/to eid fx))))))

(defn consumable
  "Returns the consumable component of the stack, or nil."
  [stack]
  (when stack (get-in (data/items) [(:item stack) :consumable])))

(defn consume-ticks
  "Returns the ticks it takes to eat or drink with consumable c."
  ^long [c]
  (long (* 20.0 (double (:seconds c)))))

(defn on-cooldown?
  "Returns true when the cooldown group of item is still locked."
  [e item ^long tick]
  (boolean
    (when-let [[group _] (data/use-cooldown item)]
      (> (long (get-in e [:cooldowns group] 0)) tick))))

(defn cooldown-deltas
  "Returns the deltas that lock the cooldown group of item.
  They tell the client too. Returns nil when item has no cooldown."
  [eid e item ^long tick]
  (when-let [[group ticks] (data/use-cooldown item)]
    [[:merge-entity eid
      {:cooldowns (assoc (:cooldowns e) group (+ tick ticks))}]
     (out/to eid (out/cooldown group ticks))]))

(defn- start-use [e hand ^long tick]
  (let [stack (hand-stack e hand)
        c (consumable stack)]
    (cond
      (:using e) e
      (on-cooldown? e (:item stack) tick) e
      c (assoc e
          :using-item? true
          :using {:hand hand :item (:item stack) :started tick
                  :remaining (consume-ticks c)})
      (sword? (:item stack)) (assoc e :using-item? true)
      :else e)))

(defn- use-item [w eid hand rot]
  (-> (update-entity w eid snapped rot)
      (update-entity eid start-use hand (long (:tick w)))))

(defn- placed [w eid face rot]
  (if (= 255 (bit-and (long face) 0xFF))
    (use-item w eid :main rot)
    (update-entity w eid snapped rot)))

(defn- seen-slots [m slots]
  (reduce (fn [m [s st]] (if st (assoc m s st) (dissoc m s)))
          (or m {})
          slots))

(defn- client-slots [e slots carried]
  (if-let [tr (:track e)]
    (assoc e :track (-> tr
                        (update :slots seen-slots slots)
                        (assoc :carried carried)))
    e))

(defn- given? [^long slot stack]
  (and (<= 1 slot 45)
       (or (nil? stack)
           (and (keyword? (:item stack))
                (<= 1 (long (:count stack 1))
                    (stack/max-size stack))))))

(defn- creative-deltas [e eid ^long slot stack]
  (when (and (game-mode/creative? e) (given? slot stack))
    [[:set-slot eid slot stack]
     [:client-slots eid {slot stack} (get-in e [:track :carried])]]))

(defn- held-deltas [e eid ^long slot]
  (when (<= 0 slot 8)
    [[:merge-entity eid (switched e slot)]]))

(defn- swap-deltas [e eid]
  [[:set-slot eid (hand-slot e :main) (hand-stack e :off)]
   [:set-slot eid (hand-slot e :off) (hand-stack e :main)]
   [:merge-entity eid {:using-item? false :using nil}]])

(def ^:private ^:const swap-hands 6)

(defn- slot-event? [tag action]
  (case tag
    (:held-item :creative-slot) true
    :dig (= swap-hands (long action))
    false))

(defn slot-deltas
  "Returns the deltas of an event that touches only player slots.
  Every fold over the events of a tick replays them. Only the packet
  systems give them out."
  [world [tag eid slot stack]]
  (when (slot-event? tag slot)
    (when-let [e (get-in world [:entities eid])]
      (case tag
        :held-item (held-deltas e eid (long slot))
        :creative-slot (creative-deltas e eid (long slot) stack)
        :dig (when-not (game-mode/spectator? e)
               (swap-deltas e eid))))))

(def ^:private horizontal-limit 3.0E7)

(def ^:private vertical-limit 2.0E7)

(defn- clamp [x limit]
  (-> (double x) (max (- limit)) (min limit)))

(defn- clamped [[x y z]]
  [(clamp x horizontal-limit)
   (clamp y vertical-limit)
   (clamp z horizontal-limit)])

(defn next-teleport-id
  "Returns the id of the next teleport a player is sent.
  The ids count from 1, the join teleport, and wrap."
  ^long [e]
  (let [n (inc (long (:tp-id e 1)))]
    (if (= n Integer/MAX_VALUE) 0 n)))

(defn client-loaded?
  "Returns true when the client of player e counts as loaded at t.
  The client said so, or sixty ticks passed since the player joined
  or respawned. A dead player waits for its respawn first."
  [e ^long t]
  (and (pos? (double (:health e 0.0)))
       (>= t (long (:loaded-at e 0)))))

(def ^:private ^:const teleport-resend 20)

(defn- resent [w eid]
  (let [e (get-in w [:entities eid]) t (long (:tick w))]
    (if (and (:tp-target e)
             (> (- t (long (:tp-at e))) teleport-resend))
      (update-entity w eid assoc :tp-id (next-teleport-id e) :tp-at t)
      w)))

(defn- teleport-ack [w eid id]
  (let [e (get-in w [:entities eid])]
    (if (and (:tp-target e) (= (long id) (long (:tp-id e 1))))
      (update-entity w eid merge
                     {:pos (v/v3 (:tp-target e)) :tp-target nil
                      :client-vel [0.0 0.0 0.0] :fall 0.0})
      w)))

(defn- fall-changes [e changes vel]
  (let [fall (double (or (:fall e) 0.0))
        dy (if vel (v/y vel) 0.0)]
    (cond
      (or (:flying e) (:flying changes)) {:fall 0.0 :landed nil}
      (:on-ground changes)
      {:fall 0.0 :landed (when (pos? fall) fall)}
      (neg? dy) {:fall (- fall dy) :landed nil}
      :else {:landed nil})))

(defn- move-vel [old new]
  (when (and old new)
    (v/v3 (- (v/x new) (v/x old))
          (- (v/y new) (v/y old))
          (- (v/z new) (v/z old)))))

(defn- free-move [w eid e changes]
  (let [new (some-> (:pos changes) clamped v/v3)
        vel (move-vel (:pos e) new)
        changes (cond-> changes new (assoc :pos new))]
    (update-entity w eid merge changes
                   (when vel {:client-vel vel})
                   (fall-changes e changes vel))))

(defn- apply-move [w eid changes]
  (let [e (get-in w [:entities eid])]
    (cond
      (:tp-target e)
      (update-entity w eid merge (dissoc changes :pos :on-ground))
      (:sleeping e) (update-entity w eid merge (dissoc changes :pos))
      :else (free-move w eid e changes))))

(defn- chunk-batch-ack [w eid rate]
  (let [rate (double rate)
        rate (if (Double/isNaN rate)
               0.01
               (-> rate (max 0.01) (min 64.0)))]
    (if-let [e (get-in w [:entities eid])]
      (let [unacked (max 0 (dec (long (or (:batches-unacked e) 0))))
            m {:chunk-rate rate :batches-max 10
               :batches-unacked unacked}
            m (cond-> m (zero? unacked) (assoc :chunk-quota 1.0))]
        (update-entity w eid merge m))
      w)))

(defn- keepalive-echo [w eid id]
  (let [e (get-in w [:entities eid])]
    (if (and e (:keepalive-pending? e)
             (= (long id) (long (:keepalive-at e -1))))
      (let [rtt (* 50 (- (long (:tick w)) (long id)))
            ping (quot (+ (* 3 (long (or (:ping e) 0))) rtt) 4)]
        (update-entity w eid assoc
                       :keepalive-pending? false
                       :ping ping))
      w)))

(defn- flight-claimed [w eid changes]
  (let [may? (game-mode/may-fly? (get-in w [:entities eid]))
        flying (and may? (boolean (:flying changes)))]
    (apply-move w eid {:flying flying})))

(defn- sprinted
  "Returns player e sprinting or not.
  The sprint modifier comes off its movement speed and goes back on
  when it sprints. The attribute is left to sync unless it was not
  sprinting and does not."
  [e on?]
  (cond-> (assoc e :sprinting? on?)
    (or on? (:sprinting? e))
    (update :dirty-attributes (fnil conj #{}) :movement-speed)))

(defn- entity-action [w eid action]
  (case (long action)
    0 (update-entity w eid assoc :leave-bed? true)
    1 (update-entity w eid sprinted true)
    2 (update-entity w eid sprinted false)
    w))

(def input-apply
  {:player-join
   (fn [w [_ eid name settings]] (player-join w eid name settings))
   :player-quit (fn [w [_ eid]] (player-quit w eid))
   :move (fn [w [_ eid changes]]
           (apply-move (resent w eid) eid changes))
   :abilities (fn [w [_ eid changes]] (flight-claimed w eid changes))
   :player-loaded
   (fn [w [_ eid]] (update-entity w eid dissoc :loaded-at))
   :teleport-ack (fn [w [_ eid id]] (teleport-ack w eid id))
   :respawn (fn [w [_ eid]] (respawn-requested w eid))
   :keepalive-echo (fn [w [_ eid id]] (keepalive-echo w eid id))
   :chunk-batch-ack
   (fn [w [_ eid rate]] (chunk-batch-ack w eid rate))
   :entity-action
   (fn [w [_ eid action]] (entity-action w eid action))
   :input (fn [w [_ eid flags]] (update-entity w eid merge flags))
   :client-settings
   (fn [w [_ eid {:keys [view-distance skin-parts]}]]
     (update-entity w eid assoc
                    :view-distance (long (or view-distance 2))
                    :skin-parts (long (or skin-parts 0))))
   :place (fn [w [_ eid _ face _ _ _ rot]]
                      (placed w eid face rot))
   :use-item (fn [w [_ eid hand _ rot]]
                      (use-item w eid hand rot))
   :release-use (fn [w [_ eid]]
                      (update-entity w eid stopped-use))})

(defn- unchanged [w _]
  w)

(defn apply-event
  "Returns the world after one input delta."
  [world delta]
  ((get input-apply (nth delta 0) unchanged) world delta))

(def ^:private move-keys
  [:on-ground :sprinting? :pose :swimming? :eye-in-water? :in-water?
   :landed])

(defn- jump? [e e']
  (and (:on-ground e) (not (:on-ground e'))
       (> (v/y (:pos e')) (v/y (:pos e)))))

(defn move-of
  "Returns what a :move event did to its player.
  Returns nil when the event moved no player."
  [w w' [tag eid changes]]
  (let [e (get-in w [:entities eid]) e' (get-in w' [:entities eid])]
    (when (and (= :move tag) (:pos changes) (:pos e) e'
               (not (:tp-target e)) (not (:sleeping e)))
      (assoc (select-keys e' move-keys)
             :eid eid :from (:pos e) :to (:pos e')
             :jump? (jump? e e')
             :climbing?
             (climb/on-climbable? (:chunks w') (:pos e'))))))

(defn infinite-materials?
  "Returns true when the player builds without spending items."
  [player]
  (game-mode/creative? player))

(defn permission-level
  "Returns the permission level of player e.
  Every player is an operator of the top level unless it holds a level
  of its own."
  ^long [e]
  (long (:permission-level e 4)))

(defn block-reach
  "Returns how far the player reaches blocks."
  ^double [player]
  (if (game-mode/creative? player)
    (+ game-mode/block-range game-mode/creative-block-range)
    game-mode/block-range))

(defn entity-reach
  "Returns how far the player reaches entities."
  ^double [player]
  (if (game-mode/creative? player)
    (+ game-mode/entity-range game-mode/creative-entity-range)
    game-mode/entity-range))

(defn quit-of
  "Returns the entity that leaves with a player quit event.
  Any other event gives nil."
  [w [tag eid]]
  (when (= :player-quit tag)
    (when-let [e (get-in w [:entities eid])]
      (assoc e :eid eid))))

(defn- resend-of
  [w w' [tag eid]]
  (let [e (get-in w [:entities eid]) e' (get-in w' [:entities eid])]
    (when (and (= :move tag) e' (not= (:tp-id e) (:tp-id e')))
      {:eid eid :pos (:tp-target e)
       :yaw (:yaw e) :pitch (:pitch e)})))

(def ^:private load-gated
  "The events that count only from a client that has loaded."
  #{:move :input :dig :release-use :place :use-item :entity-action
    :attack :interact :spectate})

(defn- heeded? [w [tag eid]]
  (or (not (contains? load-gated tag))
      (when-let [e (get-in w [:entities eid])]
        (client-loaded? e (long (:tick w))))))

(defn- heard [acc w w' d]
  (let [o (use-origin w d) m (move-of w w' d) q (quit-of w d)
        r (resend-of w w' d)]
    (cond-> (update acc :heeded conj d)
      o (assoc-in [:use-origins (count (:heeded acc))] o)
      m (update :moves conj m)
      q (update :quits conj q)
      r (update :resends conj r))))

(def ^:private unheard
  {:heeded [] :use-origins {} :moves [] :quits [] :resends []})

(defn- remembered [w input {:keys [heeded quits resends] :as acc}]
  (cond-> (merge w (select-keys acc [:use-origins :moves]))
    (seq quits) (assoc :quits quits)
    (seq resends) (assoc :resends resends)
    (< (count heeded) (count input)) (assoc :heeded heeded)))

(defn dropped!
  "Logs that event ev failed in unit f and is dropped."
  [f ev ^Throwable t]
  (let [unit (log/name-of f)
        msg (str "event " (pr-str ev) " failed in " unit
                 ", the event is dropped")]
    (log/failure! unit msg t)))

(defn- heard-event [w acc d]
  (try (when (heeded? w d)
         (let [w' (apply-event w d)] [w' (heard acc w w' d)]))
       (catch Throwable t (dropped! #'apply-event d t))))

(defn- applied-input [world input]
  (loop [w world acc unheard xs (seq input)]
    (if-let [d (first xs)]
      (if-let [[w' acc'] (heard-event w acc d)]
        (recur w' acc' (next xs))
        (recur w acc (next xs)))
      (remembered w input acc))))

(defn heeded
  "Returns the input deltas d of level dim that the world heeds."
  [world dim d]
  (if-let [h (get-in world [:levels dim :heeded])]
    (assoc d :input h)
    d))

(defn- merge-diff [cur add drop]
  (let [s (or cur (i/int-set))
        s (if (seq add) (into s add) s)]
    (if (seq drop) (set/difference s (set drop)) s)))

(defn- listed [w add drop]
  (update w :listed #(clojure.core/apply dissoc (merge % add) drop)))

(def ^:const max-resist 20)

(defn- knock-back [e ^double dx ^double dz]
  (let [f (Math/sqrt (+ (* dx dx) (* dz dz)))
        v (or (:vel e) [0.0 0.0 0.0])]
    (if (zero? f)
      e
      (assoc e :vel [(- (/ (v/x v) 2.0) (* (/ dx f) 0.4))
                     (min 0.4 (+ (/ (v/y v) 2.0) 0.4))
                     (- (/ (v/z v) 2.0) (* (/ dz f) 0.4))]))))

(defn- hurt-again [e ^double health ^double amount]
  (let [last-d (double (or (:last-damage e) 0.0))]
    (if (> amount last-d)
      (assoc e :health (- health (- amount last-d))
               :last-damage amount)
      e)))

(defn- hurt-fully [e health amount dx dz]
  (let [left (max 0.0 (- (double health) (double amount)))]
    (cond-> (assoc e :health left
                     :last-damage amount
                     :hurt-resist max-resist)
            dx (knock-back (double dx) (double dz)))))

(defn- hurt-item [e ^double health ^double amount]
  (assoc e :health (double (long (- health amount)))))

(defn hurt
  "Returns entity e after amount of damage.
  It is knocked back from direction dx dz when given."
  ([e ^double amount] (hurt e amount nil nil))
  ([e ^double amount dx dz]
   (let [health (double (or (:health e) 0.0))
         resist (long (or (:hurt-resist e) 0))]
     (cond
       (not (pos? health)) e
       (contains? #{:item :experience-orb} (:type e))
       (hurt-item e health amount)
       (> resist (/ max-resist 2.0)) (hurt-again e health amount)
       :else (hurt-fully e health amount dx dz)))))

(def entity-apply
  {:merge-entity (fn [_ e [_ _ m]] (entity/merged e m))
   :teleport (fn [tick e [_ _ pos]]
               (assoc e :pos (v/v3 pos) :tp-target pos
                        :tp-id (next-teleport-id e) :tp-at tick))
   :client-slots (fn [_ e [_ _ slots carried]]
                   (client-slots e slots carried))
   :award (fn [_ e [_ _ k n]]
                   (update e :awards (fnil conj []) [k n]))
   :track (fn [_ e [_ _ tr]]
            (assoc (cond-> e
                     (some? (:kept-mdata e)) (dissoc :kept-mdata))
                   :track tr))
   :tracking (fn [_ e [_ _ add drop]]
               (update e :tracking merge-diff add drop))
   :set-slot (fn [_ e [_ _ slot stack]]
                   (if stack
                     (assoc-in e [:inventory slot] stack)
                     (update e :inventory dissoc slot)))
   :chunks-sent (fn [_ e [_ _ add drop]]
                  (update e :sent-chunks merge-diff add drop))
   :damage (fn [_ e [_ _ amount dx dz]] (hurt e amount dx dz))
   :push (fn [_ e [_ _ vel]]
                   (let [k (if (= :tnt (:type e)) :kb :vel)]
                     (update e k (fnil v/+ [0.0 0.0 0.0]) vel)))})

(defn- apply-entity-delta [tick e delta]
  ((get entity-apply (nth delta 0)) tick e delta))

(defn apply-entities
  "Returns the world with only the entity deltas folded in."
  [world deltas]
  (let [one (fn [e d] (apply-entity-delta (:tick world) e d))
        step (fn [w d]
               (if (contains? entity-apply (nth d 0))
                 (update-entity w (nth d 1) one d)
                 w))]
    (reduce step world deltas)))

(defn- spawned [w spec]
  (let [eid (long (:next-eid w 1000000))]
    (-> w
        (assoc-in [:entities eid]
                  (entity/of (assoc spec :born (:tick w))))
        (assoc :next-eid (inc eid)))))

(defn- orbs-awarded [w pos amount salt roughly]
  (let [t (:tick w)]
    (orb/awarded w spawned pos (or roughly [0.0 0.0 0.0])
                 (long amount) #(random/of-key t pos salt %))))

(defn- block-or-zero ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y)
    (chunk/chunks-get-block chunks p)
    0))

(defn- block-tick [w at id]
  (let [p (chunk/id->block-pos id)
        st (block-or-zero (:chunks w) p)]
    (update w :block-ticks schedule/add at id (block/block-of st))))

(defn- scheduled [w at-ids]
  (reduce (fn [w [at ids]]
            (reduce #(block-tick %1 at %2) w ids))
          w at-ids))

(defn- rechecked [w pos at]
  (if at
    (assoc-in w [:container-rechecks pos] (long at))
    (update w :container-rechecks dissoc pos)))

(defn- shulker-animated [w pos a]
  (if a
    (let [base {:progress (float 0.0)}]
      (update-in w [:shulker-anim pos] #(merge base % a)))
    (update w :shulker-anim dissoc pos)))

(defn- chunk-added [w id c]
  (if (contains? (:chunks w) id) w (update w :chunks assoc id c)))

(defn- chunk-requested [w id]
  (update w :loading (fnil conj (i/int-set)) id))

(defn- chunk-ticketed [w id n]
  (update w :unknown assoc (long id) (long n)))

(defn- chunk-restored [w id payload]
  (if (contains? (:chunks w) id)
    (update w :loading disj id)
    (let [[fresh n] (schema/refreshed w payload)]
      (schema/with-chunk (assoc w :next-eid n) id fresh))))

(defn- spawn-progress [w eid req]
  (if req
    (assoc-in w [:spawning eid] req)
    (update w :spawning dissoc eid)))

(defn- block-entity-set [w pos e]
  (if (and e (be/kind (chunk/chunks-get-block (:chunks w) pos)))
    (assoc-in w [:block-entities (chunk/block-chunk pos) pos] e)
    (drop-block-entity w pos)))

(defn- openers-counted [w pos ^long step]
  (let [n (+ (long (get-in w [:openers pos] 0)) step)]
    (if (zero? n)
      (update w :openers dissoc pos)
      (assoc-in w [:openers pos] n))))

(defn- weather-advanced [w m]
  (reduce-kv (fn [w k v] (if (= v (get w k)) w (assoc w k v)))
             w (or m (weather/advance w))))

(def world-apply
  {:remove-entity        (fn [w [_ eid]] (player-quit w eid))
   :listed (fn [w [_ add drop]] (listed w add drop))
   :spawn-entity (fn [w [_ spec]] (spawned w spec))
   :xp-award (fn [w [_ pos n salt dir]]
               (orbs-awarded w pos n salt dir))
   :set-blocks (fn [w [_ changes ticks quiet]]
                 (if ticks
                   (settled w changes ticks quiet)
                   (apply-set-blocks w changes)))
   :ticks-flushed (fn [w [_ k t parked]]
                    (update w k schedule/flushed t parked))
   :schedule-ticks (fn [w [_ at-ids]] (scheduled w at-ids))
   :container-recheck (fn [w [_ pos at]] (rechecked w pos at))
   :openers (fn [w [_ pos step]] (openers-counted w pos step))
   :shulker-anim (fn [w [_ pos a]] (shulker-animated w pos a))
   :block-events-flushed (fn [w _] (assoc w :block-events nil))
   :set-clock (fn [w [_ k m]] (update-in w [:clocks k] merge m))
   :set-rule (fn [w [_ rule value]] (assoc-in w [:rules rule] value))
   :set-config (fn [w [_ m]] (assoc w :config m))
   :set-world-spawn world-spawn-set
   :add-chunk (fn [w [_ id c]] (chunk-added w id c))
   :chunk-requested (fn [w [_ id]] (chunk-requested w id))
   :chunk-ticket (fn [w [_ id n]] (chunk-ticketed w id n))
   :purge-tickets (fn [w [_ held]] (assoc w :unknown held))
   :restore-chunk (fn [w [_ id p]] (chunk-restored w id p))
   :unload-chunk (fn [w [_ id]] (unloaded w id))
   :player-placed (fn [w [_ eid n p]] (player-placed w eid n p))
   :spawn-progress (fn [w [_ eid req]] (spawn-progress w eid req))
   :set-weather (fn [w [_ m]]
                  (merge w (select-keys m weather/fields)))
   :set-block-entity (fn [w [_ pos e]] (block-entity-set w pos e))
   :advance-tick (fn [w _] (dissoc (advance w) :quits))
   :advance-weather (fn [w [_ m]] (weather-advanced w m))
   :observed (fn [w [_ m]] (assoc w :observed m))
   :explode (fn [w _] w)
   :change-dimension (fn [w _] w)
   :level-deltas (fn [w _] w)})

(defn- apply-world-delta [w delta]
  (let [tag (nth delta 0)]
    (if-let [f (get world-apply tag)]
      (f w delta)
      (if-let [g (get entity-apply tag)]
        (update-entity w (nth delta 1) #(g (:tick w) % delta))
        w))))

(defn- merged-in [e m] (if m (entity/merged e m) e))

(defn- kept-mdata? [e m]
  (or (some? (:kept-mdata e)) (contains? m :kept-mdata)))

(defn- merge-of
  "Returns delta d as a map to merge into entity e, or nil.
  It is nil when d does more than set keys under the pending merge m."
  [e m d]
  (case (nth d 0)
    :merge-entity (nth d 2)
    :track (when-not (kept-mdata? e m) {:track (nth d 2)})
    nil))

(defn- entity-folded [t e ds]
  (loop [e e m nil ds (seq ds)]
    (if ds
      (let [d (first ds)]
        (if-let [dm (merge-of e m d)]
          (recur e (if m (merge m dm) dm) (next ds))
          (recur (apply-entity-delta t (merged-in e m) d) nil
                 (next ds))))
      (merged-in e m))))

(defn- stepped [entities t]
  (fn [m [eid ds]]
    (if-let [e (get entities eid)]
      (assoc! m eid (entity-folded t e ds))
      m)))

(defn- eid-at ^long [v ^long k] (long (key (nth v k))))

(defn- leaf-of [step entities v ^long j]
  (let [n (count v)
        a (* j fold-leaf)
        b (min n (+ a fold-leaf))
        lo (if (zero? j) Long/MIN_VALUE (eid-at v a))
        hi (if (= b n) Long/MAX_VALUE (dec (eid-at v b)))]
    (persistent!
      (reduce step (transient (i/range entities lo hi))
              (subvec v a b)))))

(defn- leaves [^long n]
  (vec (range (quot (+ n (dec fold-leaf)) fold-leaf))))

(defn- folded-entities [w entities by-eid]
  (let [step (stepped entities (:tick w))
        n (count by-eid)]
    (if (< n fold-leaf)
      (persistent! (reduce step (transient entities) by-eid))
      (let [v (vec by-eid)
            leaf #(i/merge %1 (leaf-of step entities v %2))]
        (r/fold 1 (r/monoid i/merge i/int-map) leaf (leaves n))))))

(defn- deltas-of [deltas]
  (if (instance? Deltas deltas)
    deltas
    (deltas/add deltas/empty-deltas deltas)))

(defn- untyped [types t eid]
  (let [s (disj (get types t) eid)]
    (if (seq s) (assoc types t s) (dissoc types t))))

(defn- retyped [types e0 e1 eid]
  (let [a (get e0 eid) b (get e1 eid)]
    (if (and a b (identical? (:type a) (:type b)))
      types
      (cond-> types
        a (untyped (:type a) eid)
        b (update (:type b) (fnil conj (i/int-set)) eid)))))

(defn- retyped-index [types w w' eids]
  (let [e0 (:entities w) e1 (:entities w')]
    (if (or (nil? types) (identical? e0 e1))
      types
      (when eids (reduce #(retyped %1 e0 e1 %2) types eids)))))

(defn- next-eid ^long [w] (long (:next-eid w 1000000)))

(defn- touched [w w' delta]
  (let [tag (nth delta 0)]
    (case tag
      (:spawn-entity :xp-award) (range (next-eid w) (next-eid w'))
      :player-placed [(nth delta 1)]
      (when (contains? entity-apply tag) []))))

(defn- world-step [[w removes types] delta]
  (if (identical? :remove-entity (nth delta 0))
    [w (conj removes (nth delta 1)) types]
    (let [w' (apply-world-delta w delta)]
      [w' removes
       (retyped-index types w w' (touched w w' delta))])))

(defn- input-eids [inp]
  (into [] (keep #(let [x (nth % 1 nil)] (when (integer? x) x))) inp))

(defn- checked [es types]
  (when (and types (not= types (by-type es)))
    (throw (ex-info "the type index strayed from the entities"
                    {:index types :entities (by-type es)})))
  types)

(defn- typed [w types]
  (let [es (:entities w)
        types (if delta/validate? (checked es types) types)]
    (if (and types (identical? es (nth (::types (meta w)) 0 nil)))
      w
      (vary-meta w assoc ::types [es (or types (by-type es))]))))

(defn- folded-in [w by-eid]
  (if (deltas/vacant? by-eid)
    w
    (assoc w :entities (folded-entities w (:entities w) by-eid))))

(defn- apply-level [lv deltas]
  (let [^Deltas d (deltas-of deltas)
        ws (deltas/world-of d)
        init [lv [] (types-by lv)]
        [lv1 removes types] (reduce world-step init ws)
        inp (deltas/input-of d)
        applied (if (seq inp) (applied-input lv1 inp) lv1)
        types (retyped-index types lv1 applied (input-eids inp))
        folded (folded-in applied (deltas/entities-of d))
        quit (reduce player-quit folded removes)
        types (retyped-index types folded quit removes)]
    (cache-active-chunks (typed quit types))))

(defn applied-in
  "Returns world with the deltas in level dim, and that level.
  lv is the level dim of world."
  [world dim lv deltas]
  (let [lv' (apply-level lv deltas)]
    [(with-level world dim lv') lv']))

(defn apply-in
  "Returns world with the deltas folded into its level dim."
  [world dim deltas]
  (with-level world dim (apply-level (level world dim) deltas)))

(defn apply
  "Returns the world with the deltas folded into it.
  The world may be a level or a whole world."
  [world deltas]
  (if (contains? world :levels)
    (apply-in world :overworld deltas)
    (apply-level world deltas)))

(defn- arrived [e tick pos yaw pitch]
  (assoc (stopped-use e)
         :pos (v/v3 pos) :yaw (double yaw) :pitch (double pitch)
         :tp-target pos :tp-id (next-teleport-id e) :tp-at tick
         :chunk-view nil
         :chunk-pos (chunk/pos-chunk pos) :chunks-pending? nil
         :sent-chunks (i/int-set) :tracking (i/int-set) :track nil
         :kept-mdata (:mdata (:track e))))

(defn- crossed [world from [_ eid dim pos yaw pitch]]
  (if-let [e (get-in world [:levels from :entities eid])]
    (-> world
        (update-in [:levels from :entities] dissoc eid)
        (assoc-in [:levels dim :entities eid]
                  (arrived e (:tick world) pos yaw pitch)))
    world))

(defn changes-of
  "Returns the dimension changes among the world deltas of d."
  [^Deltas d]
  (filterv #(identical? :change-dimension (nth % 0))
           (deltas/world-of d)))

(defn handoffs-of
  "Returns the deltas d hands to other levels, as [dim deltas]."
  [^Deltas d]
  (into [] (keep #(when (identical? :level-deltas (nth % 0))
                    [(nth % 1) (nth % 2)]))
        (deltas/world-of d)))

(defn cross
  "Returns world with the players that change dimension in d moved.
  Each keeps its eid in the new level and knows no chunk and no entity
  there yet."
  [world from changes]
  (reduce #(crossed %1 from %2) world changes))

(def ^:private input-keys
  [:quits :moves :use-origins :heeded :resends])

(defn entered
  "Returns world with the input deltas of level dim, and that level.
  Returns nil when it stays as it was. What the last input of the
  level left behind is dropped first."
  [world dim deltas]
  (if (and (deltas/inert? deltas)
           (not-any? (get-in world [:levels dim] {}) input-keys))
    [world nil]
    (let [lv (reduce dissoc (level world dim) input-keys)
          lv' (apply-level lv deltas)]
      [(with-level world dim lv') lv'])))

(defn enter
  "Returns world with the input deltas of level dim folded in.
  What the last input of the level left behind is dropped first."
  [world dim deltas]
  (nth (entered world dim deltas) 0))

(defn apply-deltas
  "Returns the world after deltas and the deltas as applied."
  [world deltas]
  (let [d (deltas-of deltas)]
    [(apply world d) d]))

(defn slot-part
  "Returns the slot deltas of event ev, none when they fail.
  A failed event keeps this part in every fold of the tick."
  [world ev]
  (try (slot-deltas world ev)
       (catch Throwable _ nil)))

(defn- event-deltas [f w x ev]
  (try (vec (f w x))
       (catch Throwable t (dropped! f ev t) [])))

(defn fold-events
  "Returns the deltas f gives for each event in order.
  Each event sees the world after the events and slot events
  before it. An event that f fails on gives no deltas."
  ([world events f] (fold-events world events f identity))
  ([world events f event-of]
   (loop [w world evs (seq events) acc []]
     (if-not evs
       acc
       (let [x (first evs) more (next evs) ev (event-of x)
             w (apply-entities w (slot-part w ev))
             ds (event-deltas f w x ev)
             step? (and more (seq ds))]
         (recur (if step? (first (apply-deltas w ds)) w)
                more (into acc ds)))))))
