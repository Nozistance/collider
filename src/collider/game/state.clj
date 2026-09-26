(ns collider.game.state
  "The world value and the application of deltas to it."
  (:refer-clojure :exclude [apply])
  (:require [collider.game.block.blockentity :as be]
            [collider.data :as data]
            [clojure.core.reducers :as r]
            [collider.vec :as v]
            [clojure.data.int-map :as i]
            [clojure.set :as set]
            [collider.game.entity :as entity]
            [collider.game.game-mode :as game-mode]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.game.schedule :as schedule]
            [collider.game.schema :as schema]
            [collider.game.stack :as stack]
            [collider.game.deltas :as deltas]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.climb :as climb]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.light :as light]
            [collider.world.rules :as rules]
            [collider.world.space.spawn :as spawn]
            [collider.world.env.weather :as weather])
  (:import (clojure.lang MapEntry)
           (collider.game.deltas Deltas)
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
  It is kept between 2 and 32."
  ^long [world]
  (-> (long (get-in world [:config :view-distance] 7))
      (max 2)
      (min 32)))

(defn- player-chunks [world]
  (into [] (keep (fn [[_ e]]
                   (when (= :player (:type e))
                     (chunk/id->pos (chunk/pos-chunk (:pos e))))))
        (:entities world)))

(defn- zone-at [world ^long r]
  (into (i/int-set)
        (mapcat (fn [[cx cz]]
                  (chunk/around-ids (long cx) (long cz) r)))
        (player-chunks world)))

(defn loaded-zone
  "Returns the ids of the chunks the players keep at full status.
  A loading ticket reaches the view distance and its level climbs
  by one per chunk beyond it, so two further rings still count."
  [world]
  (zone-at world (+ 2 (view-radius world))))

(defn- sim-radius ^long [world]
  (long (get-in world [:config :simulation-distance]
                activation-radius)))

(defn- compute-areas [world]
  (let [zone (loaded-zone world)
        s (sim-radius world)
        live? #(and (contains? zone %)
                    (contains? (:chunks world) %))
        in? #(into (i/int-set) (filter live?) %)]
    [(in? (zone-at world s)) (in? (zone-at world (inc s)))
     (zone-at world (inc (view-radius world)))]))

(defn- fresh?
  "Tells whether the areas still describe this world.
  Their reach is a config value, so a changed config outdates them
  as surely as a body that moved or a chunk that came or went."
  [world cached]
  (and cached
       (identical? (nth (key cached) 0) (:entities world))
       (identical? (nth (key cached) 1) (:chunks world))
       (identical? (nth (key cached) 2) (:config world))))

(defn- areas [world]
  (let [cached (:active-chunks world)]
    (if (fresh? world cached) (val cached) (compute-areas world))))

(defn active-chunks
  "Returns the chunks that run entity and random ticks."
  [world]
  (nth (areas world) 0))

(defn ticking-chunks
  "Returns the chunks that run scheduled block and fluid ticks.
  They reach one chunk further than the entity ticking ones."
  [world]
  (nth (areas world) 1))

(defn broadcast-chunks
  "Returns the chunks whose block changes reach the clients.
  A loading ticket puts the view distance and one ring past it
  below the block ticking level; the ring after that is only
  full, and a change there is never announced."
  [world]
  (nth (areas world) 2))

(defn cache-active-chunks
  "Returns the world with its chunk areas up to date.
  The areas follow its players and chunks."
  [world]
  (if (fresh? world (:active-chunks world))
    world
    (let [k [(:entities world) (:chunks world) (:config world)]]
      (assoc world :active-chunks
             (MapEntry/create k (compute-areas world))))))

(defn advance
  "Returns the world one tick older."
  [world]
  (cond-> (update world :tick inc)
          (get-in world [:rules :advance-time] true)
          (update :time-of-day (fnil inc 0))))

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
  (let [t (data/dimension-type dim)
        lo (long (:min-y t chunk/min-y))]
    {:min-y lo
     :max-y (+ lo (long (:height t 384)) -1)
     :sky? (:has-skylight t true)}))

(defn level
  "Returns the level dim of world.
  It holds the shared keys of world, the keys of level dim, :dim
  and the height and sky of its dimension."
  [world dim]
  (-> (dissoc world :levels)
      (into (get-in world [:levels dim]))
      (into (bounds dim))
      (assoc :dim dim)))

(defn with-level
  "Returns world with level dim replaced by lv.
  lv's non-level keys become the shared part of world, dropping
  shared keys lv no longer has; lv's level keys become level dim."
  [world dim lv]
  (let [lv (dissoc lv :dim :min-y :max-y :sky? :server)
        level-part (select-keys lv schema/level-keys)
        shared-part (clojure.core/apply dissoc lv schema/level-keys)]
    (assoc shared-part :levels
           (assoc (:levels world) dim level-part))))

(defn idle?
  "Tells whether level lv holds nothing a tick could change.
  Such a level has no chunks, no entities and no chunk on its way."
  [lv]
  (and (zero? (count (:chunks lv))) (zero? (count (:entities lv)))
       (empty? (:loading lv)) (empty? (:unknown lv))))

(defn dim-of
  "Returns the dimension whose level holds entity eid, or nil."
  [world eid]
  (when (integer? eid)
    (let [holds? (fn [dim]
                   (-> (get-in world [:levels dim :entities])
                       (contains? eid)))]
      (some #(when (holds? %) %) schema/dims))))

(defn- player-type? [[_ e]] (= :player (:type e)))

(defn server-view
  "Returns the shared keys of world with all players as entities.
  The players of every level are in it."
  [world]
  (let [players (comp (mapcat (comp :entities val))
                      (filter player-type?))]
    (assoc (dissoc world :levels)
      :entities (into (i/int-map) players (:levels world)))))

(defn- update-entity [w eid f & args]
  (if (get-in w [:entities eid])
    (clojure.core/apply update-in w [:entities eid] f args)
    w))

(def ^:private around
  "The cells a change reaches, each with the side the change is on
  as seen from it; nil for the cell of the change. They go in the
  order of NeighborUpdater.UPDATE_ORDER: west, east, down, up,
  north, south."
  [[[0 0 0] nil] [[-1 0 0] :east] [[1 0 0] :west] [[0 -1 0] :up]
   [[0 1 0] :down] [[0 0 -1] :south] [[0 0 1] :north]])

(defn- block-or-zero ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y)
    (chunk/chunks-get-block chunks p)
    0))

(defn- shifted [[x y z] [dx dy dz]]
  [(+ (long x) (long dx))
   (+ (long y) (long dy))
   (+ (long z) (long dz))])

(defn- schedule-at [w k at floor p ty]
  (if at
    (update w k schedule/add (max (long at) (long floor))
            (chunk/block-pos->id p) ty)
    w))

(defn- block-woken [w tick floor p st at]
  (if (= :neighbor at)
    (schedule-at w :block-wakes (inc (long tick)) floor p
                 (block/block-of st))
    (schedule-at w :block-ticks at floor p (block/block-of st))))

(defn- woken [w chunks st tick floor dim p old side]
  (let [at (rules/wake-tick chunks dim st tick p old side)
        fat (rules/fluid-wake-tick chunks dim st tick p old side)
        fluid (liquid/fluid-of st)]
    (-> w
        (block-woken tick floor p st at)
        (schedule-at :fluid-ticks fat floor p fluid))))

(defn- schedule-one [w tick floor dim pos old [d side]]
  (let [p (shifted pos d)
        chunks (:chunks w)
        st (block-or-zero chunks p)]
    (if (zero? st)
      w
      (woken w chunks st tick floor dim p old side))))

(defn- schedule-around [w tick floor dim [pos old _]]
  (reduce #(schedule-one %1 tick floor dim pos old %2) w around))

(defn- schedule-updates [w tick floor dim changed]
  (reduce #(schedule-around %1 tick floor dim %2) w changed))

(defn- kind-changed? [old st]
  (and (be/kind old)
       (not= (block/block-of old) (block/block-of (long st)))
       (not= (be/kind old) (be/kind (long st)))))

(defn- drop-block-entities [w real]
  (reduce (fn [w [pos old st]]
            (if-not (kind-changed? old st)
              w
              (let [cp (chunk/block-chunk pos)]
                (update-in w [:block-entities cp] dissoc pos))))
          w real))

(defn- inside? [w [pos]] (chunk/in-level? w (long (pos 1))))

(defn- real-changes [w changes]
  (let [chunks (:chunks w)
        real (fn [[pos st]]
               (let [old (chunk/chunks-get-block chunks pos)]
                 (when (not= old (long st)) [pos old st])))]
    (into [] (comp (filter #(inside? w %)) (keep real)) changes)))

(defn- derived-in [w chunks tick real]
  (let [poss (map first real)
        changes (connect/derived-changes chunks poss tick)]
    (filterv #(inside? w %) changes)))

(defn- with-derived [w tick real]
  (let [sky? (:sky? w true)
        set-real (mapv (fn [[pos _ st]] [pos st]) real)
        chunks' (-> (:chunks w)
                    (chunk/chunks-set-blocks set-real)
                    (light/relight-batch real sky?))
        derived (derived-in w chunks' tick real)
        was (fn [[pos st]]
              [pos (chunk/chunks-get-block chunks' pos) st])
        dropped (mapv was derived)]
    [(-> chunks'
         (chunk/chunks-set-blocks derived)
         (light/relight-batch dropped sky?))
     (concat (map (fn [[pos _ st]] [pos st]) real) derived)]))

(defn blocks-changed
  "Returns [chunks writes]: the chunks of level w with the changes
  and the shapes they cause set, the light left as it was, and
  each block written as [pos old st]. The blocks are those a
  :set-blocks of the changes leaves."
  [w changes]
  (let [real (real-changes w changes)
        chunks' (chunk/chunks-set-blocks
                  (:chunks w) (mapv (fn [[pos _ st]] [pos st]) real))
        derived (derived-in w chunks' (:tick w) real)
        was (fn [[pos st]]
              [pos (chunk/chunks-get-block chunks' pos) st])]
    [(chunk/chunks-set-blocks chunks' derived)
     (into real (map was) derived)]))

(defn- add-block-events [ev events]
  (reduce (fn [ev [pos st]]
            (update ev (chunk/block-chunk pos)
                    (fnil conj []) [pos st]))
          (or ev (i/int-map)) events))

(defn- with-changes [w base real]
  (let [tick (:tick w)
        next-tick (inc (long tick))
        [chunks' events] (with-derived w tick real)
        dim (:dim w)]
    (cond-> (-> w
                (assoc :chunks chunks')
                (drop-block-entities real)
                (schedule-updates base next-tick dim real))
      (seq events)
      (update :block-events add-block-events events))))

(defn- apply-set-blocks [w changes ^long base]
  (let [real (real-changes w changes)]
    (if (empty? real) w (with-changes w base real))))

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

(defn- respawn-dimension
  "Returns the level of the world spawn, where a player with no
  place of its own lands. The overworld stands in for a level the
  server lacks."
  [w]
  (let [dim (:world-spawn-dimension w :overworld)]
    (if (some #{dim} schema/dims) dim :overworld)))

(defn- respawn-level
  "Returns the level of the world spawn as w sees it: w itself, the
  level of the server w holds, or the level of w as a server."
  [w]
  (let [dim (respawn-dimension w)
        server (or (:server w) (when (:levels w) w))]
    (cond (= dim (:dim w)) w
          server (level server dim)
          :else (select-keys (bounds dim) [:min-y]))))

(defn- centre-top
  "Returns the top of the column at the border centre of level lv,
  its lowest y while the chunk there is not loaded."
  [lv]
  [0 (max (long (:min-y lv chunk/min-y))
          (spawn/motion-blocking-height (:chunks lv {}) 0 0))
   0])

(defn spawn-turn
  "Returns [yaw pitch] of the world spawn."
  [w]
  (:world-spawn-turn w [0.0 0.0]))

(defn respawn-at
  "Returns the world spawn as players respawn at it, moved inside
  the world border as the chunks of its level stand now."
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

(defn- load-awaited
  "Returns player e as its connection starts to wait, in tick, for
  the client to load."
  [e tick]
  (assoc e :loaded-at (+ (long tick) client-load-timeout)))

(defn- awaiting-join
  "Returns player e waiting for the ack of the teleport its
  connection sends first, the one to where it joins."
  [e tick]
  (let [p (:pos e)]
    (assoc e :tp-target [(v/x p) (v/y p) (v/z p)] :tp-id 1
             :tp-at tick)))

(defn- moded
  "ServerPlayer.readAdditionalSaveData: the mode player e joins in
  and the flight it keeps there."
  [w e]
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
                  (-> e (awaiting-join tick) (load-awaited tick)))
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
        (update :entities drop-entities id)
        (update :block-ticks schedule/dropped id)
        (update :block-wakes schedule/dropped id)
        (update :fluid-ticks schedule/dropped id)
        (update :unknown dissoc id)
        (update :stored (fnil conj (i/int-set)) id))))

(defn- vacated-bed [w eid]
  (if-let [pos (get-in w [:entities eid :sleeping :pos])]
    (let [st (chunk/chunks-get-block (:chunks w) pos)]
      (if (= :bed (block/type-of st))
        (let [props (assoc (block/props-of st) :occupied :false)
              free (block/state (block/block-of st) props)]
          (apply-set-blocks w [[pos free]] (long (:tick w))))
        w))
    w))

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
    (cond-> (-> (vacated-bed w eid)
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

(defn- snapped
  "Returns e turned to rot.
  The client reports rot for the use of an item."
  [e rot]
  (if (and rot (held-item-of e))
    (assoc e :yaw (wrap-degrees (double (:yaw rot)))
             :pitch (wrap-degrees (double (:pitch rot))))
    e))

(def ^:private origin-keys [:pos :yaw :pitch :sneaking? :flying])

(defn use-origin
  "Returns the eye and look of the player behind an event.
  Only place and use events have one; others give nil."
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

(defn- swing-free?
  "Tells whether the arm is done enough with its last swing.
  Vanilla lets a new one in halfway through the one before."
  [e ^long t]
  (if-let [at (:swing-at e)]
    (let [k (- t (long at))]
      (or (zero? k)
          (>= (dec k) (quot (swing-duration e (:swing-hand e)) 2))))
    true))

(defn swing-deltas
  "Returns the deltas of the arm swing of player eid.
  There are none while the arm is still busy with the swing before
  it. The player sees its own swing only when the server, not the
  client, started it."
  [eid e hand t self?]
  (when (swing-free? e (long t))
    (let [kind (if (= :off hand) :swing-off :swing)
          fx (out/animation eid kind)]
      (cond-> [[:merge-entity eid
                {:swing-at (long t) :swing-hand hand}]
               (out/all fx)]
        self? (conj (out/to eid fx))))))

(defn consumable
  "Returns the Consumable component of the stack, or nil."
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
  "Returns the deltas that lock item's cooldown group.
  They also notify the client. Returns nil when item has
  no cooldown."
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
  Every fold over the events of a tick replays these; the packet
  systems alone give them out."
  [world [tag eid slot stack]]
  (when (slot-event? tag slot)
    (when-let [e (get-in world [:entities eid])]
      (case tag
        :held-item (held-deltas e eid (long slot))
        :creative-slot (creative-deltas e eid (long slot) stack)
        :dig (swap-deltas e eid)))))

(def ^:private horizontal-limit 3.0E7)

(def ^:private vertical-limit 2.0E7)

(defn- clamp [x limit]
  (-> (double x) (max (- limit)) (min limit)))

(defn- clamped [[x y z]]
  [(clamp x horizontal-limit)
   (clamp y vertical-limit)
   (clamp z horizontal-limit)])

(defn next-teleport-id
  "Returns the id of the next teleport a player is sent. The
  connection counts them from 1, the join teleport, and wraps."
  ^long [e]
  (let [n (inc (long (:tp-id e 1)))]
    (if (= n Integer/MAX_VALUE) 0 n)))

(defn client-loaded?
  "Tells whether the client of player e counts as loaded for what it
  sends in tick t: it said so, or sixty ticks passed since it joined
  or respawned. A dead player waits for its respawn first."
  [e ^long t]
  (and (pos? (double (:health e 0.0)))
       (>= t (long (:loaded-at e 0)))))

(def ^:private ^:const teleport-resend 20)

(defn- resent
  "Returns w after a move of player eid, which sends the teleport it
  has not acked again once twenty ticks are past the last one."
  [w eid]
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

(defn- flight-claimed
  "handlePlayerAbilities: the player flies as its client says only
  when its mode lets it fly."
  [w eid changes]
  (let [may? (game-mode/may-fly? (get-in w [:entities eid]))
        flying (and may? (boolean (:flying changes)))]
    (apply-move w eid {:flying flying})))

(def ^:private entity-actions
  {0 [:leave-bed? true] 1 [:sprinting? true] 2 [:sprinting? false]})

(defn- entity-action [w eid action]
  (if-let [[k v] (entity-actions (long action))]
    (update-entity w eid assoc k v)
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
  "Returns true when the player builds without spending items.
  Player.hasInfiniteMaterials: the instabuild of creative."
  [player]
  (game-mode/creative? player))

(defn permission-level
  "Returns the permission level of player e. Every player is an
  operator of the top level unless it holds a level of its own."
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
  "Returns the teleport a :move event sent again, nil when none.
  It goes where the last one went, turned as the player was."
  [w w' [tag eid]]
  (let [e (get-in w [:entities eid]) e' (get-in w' [:entities eid])]
    (when (and (= :move tag) e' (not= (:tp-id e) (:tp-id e')))
      {:eid eid :pos (:tp-target e)
       :yaw (:yaw e) :pitch (:pitch e)})))

(def ^:private load-gated
  "The events of the packets ServerGamePacketListenerImpl takes only
  from a client that has loaded."
  #{:move :input :dig :release-use :place :use-item :entity-action
    :attack :interact})

(defn- heeded? [w [tag eid]]
  (or (not (contains? load-gated tag))
      (when-let [e (get-in w [:entities eid])]
        (client-loaded? e (long (:tick w))))))

(defn- heard
  "Returns acc with event d, which took w to w', noted."
  [acc w w' d]
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

(defn- applied-input [world input]
  (loop [w world acc unheard xs (seq input)]
    (if-let [d (first xs)]
      (if (heeded? w d)
        (let [w' (apply-event w d)]
          (recur w' (heard acc w w' d) (next xs)))
        (recur w acc (next xs)))
      (remembered w input acc))))

(defn heeded
  "Returns the input deltas d of level dim as world took them in:
  without the events it did not heed."
  [world dim d]
  (if-let [h (get-in world [:levels dim :heeded])]
    (assoc d :input h)
    d))

(defn- merge-diff [cur add drop]
  (set/difference (into (or cur (i/int-set)) add) (set drop)))

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
       (= :item (:type e)) (hurt-item e health amount)
       (> resist (/ max-resist 2.0)) (hurt-again e health amount)
       :else (hurt-fully e health amount dx dz)))))

(def entity-apply
  {:merge-entity (fn [_ e [_ _ m]] (merge e m))
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
        (assoc-in [:entities eid] (entity/of spec))
        (assoc :next-eid (inc eid)))))

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
  (let [cp (chunk/block-chunk pos)]
    (if (and e (be/kind (chunk/chunks-get-block (:chunks w) pos)))
      (assoc-in w [:block-entities cp pos] e)
      (update-in w [:block-entities cp] dissoc pos))))

(def world-apply
  {:remove-entity        (fn [w [_ eid]] (player-quit w eid))
   :listed (fn [w [_ add drop]] (listed w add drop))
   :spawn-entity (fn [w [_ spec]] (spawned w spec))
   :set-blocks (fn [w [_ changes base]]
                 (let [at (long (or base (:tick w)))]
                   (apply-set-blocks w changes at)))
   :ticks-flushed (fn [w [_ k t parked]]
                    (update w k schedule/flushed t parked))
   :schedule-ticks (fn [w [_ at-ids]] (scheduled w at-ids))
   :container-recheck (fn [w [_ pos at]] (rechecked w pos at))
   :shulker-anim (fn [w [_ pos a]] (shulker-animated w pos a))
   :block-events-flushed (fn [w _] (assoc w :block-events nil))
   :set-time (fn [w [_ t]] (assoc w :time-of-day (long t)))
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
   :advance-weather (fn [w _] (merge w (weather/advance w)))
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

(defn- folded-entities [w entities pairs]
  (let [t (:tick w)
        step (fn [e ds] (reduce #(apply-entity-delta t %1 %2) e ds))]
    (r/fold fold-leaf (r/monoid i/merge i/int-map)
            (fn [m [eid ds]]
              (if-let [e (get entities eid)]
                (assoc m eid (step e ds))
                m))
            pairs)))

(defn- deltas-of [deltas]
  (if (instance? Deltas deltas)
    deltas
    (deltas/add deltas/empty-deltas deltas)))

(defn- world-step [[w removes] delta]
  (if (identical? :remove-entity (nth delta 0))
    [w (conj removes (nth delta 1))]
    [(apply-world-delta w delta) removes]))

(defn- apply-level [lv deltas]
  (let [^Deltas d (deltas-of deltas)
        [w removes] (reduce world-step [lv []] (deltas/world-of d))
        inp (deltas/input-of d)
        w (if (seq inp) (applied-input w inp) w)
        entities (:entities w)
        updated (folded-entities
                  w entities (vec (deltas/entities-of d)))
        w (if (pos? (count updated))
            (assoc w :entities (i/merge entities updated))
            w)]
    (cache-active-chunks (reduce player-quit w removes))))

(defn apply-in
  "Returns world with the deltas folded into its level dim."
  [world dim deltas]
  (with-level world dim (apply-level (level world dim) deltas)))

(defn apply
  "Returns the world with the deltas folded into it.
  world may be a level or a whole world; a whole world is folded
  through its overworld level and put back."
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
  Each leaves level from for its new level with the same eid. It
  knows no chunk and no entity there yet."
  [world from changes]
  (reduce #(crossed %1 from %2) world changes))

(def ^:private input-keys
  [:quits :moves :use-origins :heeded :resends])

(defn enter
  "Returns world with the input deltas of level dim folded in.
  What the last input of the level left behind is dropped first."
  [world dim deltas]
  (if (and (deltas/inert? deltas)
           (not-any? (get-in world [:levels dim] {}) input-keys))
    world
    (let [lv (reduce dissoc (level world dim) input-keys)]
      (with-level world dim (apply-level lv deltas)))))

(defn apply-deltas
  "Returns the world after deltas and the deltas as applied."
  [world deltas]
  (let [d (deltas-of deltas)]
    [(apply world d) d]))

(defn fold-events
  "Returns the deltas f gives for each event in order.
  Each event sees the world after the ones before it were applied,
  and after the slot events before it, whoever gives those out."
  ([world events f] (fold-events world events f identity))
  ([world events f event-of]
   (loop [w world evs (seq events) acc []]
     (if-not evs
       acc
       (let [x (first evs) more (next evs)
             w (apply-entities w (slot-deltas w (event-of x)))
             ds (vec (f w x))
             step? (and more (seq ds))]
         (recur (if step? (first (apply-deltas w ds)) w)
                more (into acc ds)))))))
