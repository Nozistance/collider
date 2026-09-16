(ns collider.game.state
  "The world value: how deltas apply to it, and the players and active chunks
   it holds."
  (:refer-clojure :exclude [apply])
  (:require [collider.game.block.blockentity :as be]
            [collider.data :as data]
            [clojure.core.reducers :as r]
            [collider.vec :as v]
            [clojure.data.int-map :as i]
            [clojure.set :as set]
            [collider.game.entity :as entity]
            [collider.random :as random]
            [collider.game.schema :as schema]
            [collider.game.deltas :as deltas]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.climb :as climb]
            [collider.world.blocks.connect :as connect]
            [collider.world.gen :as gen]
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
  "Returns [eid entity] pairs of the online players in eid order."
  [world]
  (let [entities (:entities world)]
    (into [] (map (fn [eid] (MapEntry/create eid (get entities eid))))
          (sort (vals (:players world))))))

(def ^:private ^:const fold-leaf 64)
(def spawn-pos [24.5 4.0 8.5])
(def activation-radius 2)
(defn pos-chunk
  "Returns the id of the chunk a position stands in."
  ^long [pos]
  (chunk/pos->id (bit-shift-right (long (Math/floor (v/x pos))) 4)
                 (bit-shift-right (long (Math/floor (v/z pos))) 4)))

(defn- compute-active-chunks [world]
  (into (i/int-set)
        (mapcat (fn [[_ e]]
                  (when (= :player (:type e))
                    (let [[cx cz] (chunk/id->pos (pos-chunk (:pos e)))
                          r (long (get-in world [:config :simulation-distance]
                                          activation-radius))]
                      (chunk/around-ids (long cx) (long cz) r)))))
        (:entities world)))

(defn active-chunks
  "Returns the set of chunk ids in which the world ticks."
  [world]
  (let [cached (:active-chunks world)]
    (if (and cached (identical? (key cached) (:entities world)))
      (val cached)
      (compute-active-chunks world))))

(defn cache-active-chunks
  "Returns world with its active chunks computed."
  [world]
  (let [cached (:active-chunks world)]
    (if (and cached (identical? (key cached) (:entities world)))
      world
      (assoc world :active-chunks (MapEntry/create (:entities world) (compute-active-chunks world))))))

(defn advance
  "Returns world one tick later, with the day time advanced when the
   advance_time rule allows."
  [world]
  (cond-> (update world :tick inc)
          (get-in world [:rules :advance-time] true) (update :time-of-day (fnil inc 0))))

(defn active-at?
  "Returns true when pos lies in an active chunk."
  [active pos]
  (contains? active (pos-chunk pos)))

(defn active-id?
  "Returns true when chunk id bid is active."
  [active ^long bid]
  (contains? active (chunk/pos->id (bit-shift-right bid 42)
                                   (bit-shift-right (bit-shift-left bid 38) 42))))

(defn offline-uuid
  "Returns the offline-mode UUID of a player name."
  ^UUID [^String name]
  (UUID/nameUUIDFromBytes (.getBytes (str "OfflinePlayer:" name) StandardCharsets/UTF_8)))

(def initial-world schema/initial-world)
(defn- update-entity
  "Returns w with f applied to entity eid, or w when there is no such entity."
  [w eid f & args]
  (if (get-in w [:entities eid])
    (clojure.core/apply update-in w [:entities eid] f args)
    w))

(def ^:private around
  [[0 0 0] [1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn- block-or-zero ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y)
    (chunk/chunks-get-block chunks (gen/flat-chunk) p)
    0))

(defn- wake-tick
  "Returns the tick at which the block at p should act on a neighbour
   changing, or nil when it does not care."
  [chunks tick p old self?]
  (let [st (block-or-zero chunks p)]
    (when-not (zero? st)
      (rules/wake-tick chunks st tick p old self?))))

(defn- schedule-updates
  "Returns the scheduled block ticks with the neighbours of every changed
   block woken."
  [bt tick floor chunks changed]
  (reduce
    (fn [bt [[x y z] old _]]
      (reduce
        (fn [bt [dx dy dz :as d]]
          (let [p [(+ (long x) (long dx)) (+ (long y) (long dy)) (+ (long z) (long dz))]]
            (if-let [at (wake-tick chunks tick p old (= [0 0 0] d))]
              (update bt (max (long at) (long floor)) (fnil conj (i/int-set)) (chunk/block-pos->id p))
              bt)))
        bt
        around))
    bt
    changed))

(defn- drop-block-entities [w real]
  (reduce (fn [w [pos old st]]
            (if (and (be/kind old)
                     (not= (block/block-of old) (block/block-of (long st)))
                     (not= (be/kind old) (be/kind (long st))))
              (update-in w [:block-entities (chunk/block-chunk pos)] dissoc pos)
              w))
          w real))

(defn- real-changes
  "Returns the changes that actually change something, each with the state
   it replaces."
  [chunks changes]
  (into []
        (keep (fn [[pos st]]
                (let [old (chunk/chunks-get-block chunks (gen/flat-chunk) pos)]
                  (when (not= old (long st)) [pos old st]))))
        changes))

(defn- with-derived
  "Returns the world blocks after the changes, relit, and everything that
   changed with them."
  [chunks tick real]
  (let [chunks' (-> chunks
                    (chunk/chunks-set-blocks (gen/flat-chunk) (mapv (fn [[pos _ st]] [pos st]) real))
                    (light/relight-batch (gen/flat-chunk) real))
        derived (connect/derived-changes chunks' (map first real) tick)
        dropped (mapv (fn [[pos st]] [pos (chunk/chunks-get-block chunks' (gen/flat-chunk) pos) st]) derived)]
    [(-> chunks'
         (chunk/chunks-set-blocks (gen/flat-chunk) derived)
         (light/relight-batch (gen/flat-chunk) dropped))
     (concat (map (fn [[pos _ st]] [pos st]) real) derived)]))

(defn- add-block-events [ev events]
  (reduce (fn [ev [pos st]] (update ev (chunk/block-chunk pos) (fnil conj []) [pos st]))
          (or ev (i/int-map)) events))

(defn- apply-set-blocks
  "Returns w with blocks changed, along with the light, block ticks and
   block entities that follow."
  [w changes ^long base]
  (let [real (real-changes (:chunks w) changes)]
    (if (empty? real)
      w
      (let [[chunks' events] (with-derived (:chunks w) (:tick w) real)]
        (-> w
            (assoc :chunks chunks')
            (drop-block-entities real)
            (update :block-ticks schedule-updates base (inc (long (:tick w))) chunks' real)
            (cond-> (seq events) (update :block-events add-block-events events)))))))

(defn spawn-seed
  "Returns the random seed of the spawn search for eid this tick."
  ^double [w eid]
  (random/of-longs (long (:tick w 0)) (long eid) (hash :spawn)))

(defn world-spawn-pos
  "Returns the spawn position for player eid, searched around the world spawn
   within the respawn radius."
  [w eid]
  (spawn/find-spawn (:chunks w) (gen/flat-chunk) (:world-spawn w)
                    (long (get-in w [:rules :respawn-radius] 10))
                    (spawn-seed w eid)))

(defn- new-player [name tick pos]
  {:type         :player :name name :uuid (offline-uuid name)
   :pos          pos :yaw 0.0 :pitch 0.0 :on-ground true
   :chunk-pos    nil :sent-chunks (i/int-set) :needs-spawn? true
   :chunk-rate   9.0 :chunk-quota 0.0 :batches-unacked 0 :batches-max 1
   :tracking     (i/int-set) :track nil
   :inventory    {} :held-slot 0
   :sneaking?    false :sprinting? false :skin-parts 0 :ping 0
   :health       20.0
   :health-sent  20.0
   :keepalive-at tick :keepalive-pending? false})

(defn- player-join [w eid name]
  (-> w
      (assoc-in [:entities eid]
                (entity/of (merge (new-player name (:tick w) (world-spawn-pos w eid))
                                  (get-in w [:profiles name]))))
      (assoc-in [:players name] eid)))

(defn- vacated-bed [w eid]
  (if-let [pos (get-in w [:entities eid :sleeping :pos])]
    (let [st (chunk/chunks-get-block (:chunks w) (gen/flat-chunk) pos)]
      (if (= :bed (block/type-of st))
        (apply-set-blocks w [[pos (block/state (block/block-of st) (assoc (block/props-of st) :occupied :false))]] (long (:tick w)))
        w))
    w))

(defn- stored-profile [e]
  (schema/profile-of
    (-> e
        (update-in [:stats :custom/leave-game] (fnil inc 0))
        (update :inventory
                #(clojure.core/apply dissoc % (range 5))))))

(defn- player-quit [w eid]
  (let [{:keys [name] :as e} (get-in w [:entities eid])]
    (cond-> (-> (vacated-bed w eid)
                (update :entities dissoc eid)
                (update :players (fn [ps] (if (= eid (get ps name)) (dissoc ps name) ps))))
            name (assoc-in [:profiles name] (stored-profile e)))))

(def ^:private ^:table swords (delay (set (data/tag-values "item" "swords"))))
(defn- sword? [item]
  (contains? @swords item))

(defn- held-item [w eid slot]
  (if (<= 0 (long slot) 8)
    (update-entity w eid assoc :held-slot (long slot) :using-item? false)
    w))

(defn- wrap-degrees ^double [^double d]
  (let [r (rem d 360.0)] (cond (>= r 180.0) (- r 360.0) (< r -180.0) (+ r 360.0) :else r)))

(defn- snapped
  "Returns e turned to rot, as the client says it looked when it used an
   item."
  [e rot]
  (if (and rot (get-in e [:inventory (+ 36 (long (or (:held-slot e) 0))) :item]))
    (assoc e :yaw (wrap-degrees (double (:yaw rot))) :pitch (wrap-degrees (double (:pitch rot))))
    e))

(defn use-origin
  "Returns the pose of the player at a :place event, or nil for other events."
  [w [tag & args]]
  (when (= :place tag)
    (let [[eid _ _ _ _ _ rot] args]
      (when-let [e (get-in w [:entities eid])]
        (select-keys (snapped e rot) [:pos :yaw :pitch :sneaking? :flying])))))

(defn- use-item [w eid face item rot]
  (let [w (update-entity w eid snapped rot)]
    (if (and (= 255 (bit-and (long face) 0xFF))
             (sword? item))
      (update-entity w eid assoc :using-item? true)
      w)))

(defn- release-item [w eid status]
  (if (= 5 (long status))
    (update-entity w eid assoc :using-item? false)
    w))

(defn- set-slot [w eid slot stack]
  (update-entity w eid
                 (fn [e]
                   (if stack
                     (assoc-in e [:inventory slot] stack)
                     (update e :inventory dissoc slot)))))

(defn- client-slots [e slots carried]
  (if-let [tr (:track e)]
    (assoc e :track (-> tr
                        (update :slots (fn [m] (reduce (fn [m [s st]] (if st (assoc m s st) (dissoc m s))) (or m {}) slots)))
                        (assoc :carried carried)))
    e))

(defn- creative-slot [w eid slot stack]
  (let [slot (long slot)]
    (if (and (<= 1 slot 45)
             (or (nil? stack)
                 (and (keyword? (:item stack))
                      (<= 1 (long (:count stack 1)) 64))))
      (-> (set-slot w eid slot stack)
          (update-entity eid (fn [e] (client-slots e {slot stack} (get-in e [:track :carried])))))
      w)))

(def ^:private horizontal-limit 3.0E7)
(def ^:private vertical-limit 2.0E7)
(defn- clamped [[x y z]]
  [(-> (double x) (max (- horizontal-limit)) (min horizontal-limit))
   (-> (double y) (max (- vertical-limit)) (min vertical-limit))
   (-> (double z) (max (- horizontal-limit)) (min horizontal-limit))])

(defn- teleport-ack [w eid id]
  (let [e (get-in w [:entities eid])]
    (if (and (:tp-target e) (= (long id) (long (:tp-id e -1))))
      (update-entity w eid merge {:pos        (v/v3 (:tp-target e)) :tp-target nil :tp-id nil
                                  :client-vel [0.0 0.0 0.0] :fall 0.0})
      w)))

(defn- fall-changes [e changes vel]
  (let [fall (double (or (:fall e) 0.0))
        dy (if vel (v/y vel) 0.0)]
    (cond
      (or (:flying e) (:flying changes)) {:fall 0.0 :landed nil}
      (:on-ground changes) {:fall 0.0 :landed (when (pos? fall) fall)}
      (neg? dy) {:fall (- fall dy) :landed nil}
      :else {:landed nil})))

(defn- apply-move [w eid changes]
  (let [e (get-in w [:entities eid])
        new (:pos changes)]
    (cond
      (or (:tp-target e) (:sleeping e))
      (update-entity w eid merge (dissoc changes :pos))
      :else
      (let [old (:pos e)
            new (some-> new clamped v/v3)
            vel (when (and old new)
                  (v/v3 (- (v/x new) (v/x old)) (- (v/y new) (v/y old)) (- (v/z new) (v/z old))))
            changes (cond-> changes new (assoc :pos new))]
        (update-entity w eid merge changes (when vel {:client-vel vel}) (fall-changes e changes vel))))))

(defn- chunk-batch-ack [w eid rate]
  (let [rate (double rate)
        rate (if (Double/isNaN rate) 0.01 (-> rate (max 0.01) (min 64.0)))]
    (if-let [e (get-in w [:entities eid])]
      (let [unacked (max 0 (dec (long (or (:batches-unacked e) 0))))]
        (update-entity w eid merge (cond-> {:chunk-rate rate :batches-unacked unacked :batches-max 10}
                                           (zero? unacked) (assoc :chunk-quota 1.0))))
      w)))

(defn- keepalive-echo [w eid id]
  (let [e (get-in w [:entities eid])]
    (if (and e (:keepalive-pending? e) (= (long id) (long (:keepalive-at e -1))))
      (let [rtt (* 50 (- (long (:tick w)) (long id)))]
        (update-entity w eid assoc
                       :keepalive-pending? false
                       :ping (quot (+ (* 3 (long (or (:ping e) 0))) rtt) 4)))
      w)))

(def ^:private entity-actions
  {0 [:leave-bed? true] 1 [:sprinting? true] 2 [:sprinting? false]})

(defn- entity-action [w eid action]
  (if-let [[k v] (entity-actions (long action))]
    (update-entity w eid assoc k v)
    w))

(def input-apply
  {:player-join     (fn [w [_ eid name]] (player-join w eid name))
   :player-quit     (fn [w [_ eid]] (player-quit w eid))
   :move            (fn [w [_ eid changes]] (apply-move w eid changes))
   :teleport-ack    (fn [w [_ eid id]] (teleport-ack w eid id))
   :keepalive-echo  (fn [w [_ eid id]] (keepalive-echo w eid id))
   :chunk-batch-ack (fn [w [_ eid rate]] (chunk-batch-ack w eid rate))
   :entity-action   (fn [w [_ eid action]] (entity-action w eid action))
   :input           (fn [w [_ eid flags]] (update-entity w eid merge flags))
   :client-settings (fn [w [_ eid sp]] (update-entity w eid assoc :skin-parts sp))
   :held-item       (fn [w [_ eid slot]] (held-item w eid slot))
   :creative-slot   (fn [w [_ eid slot stack]] (creative-slot w eid slot stack))
   :place           (fn [w [_ eid _ face item _ _ rot]] (use-item w eid face item rot))
   :dig             (fn [w [_ eid status]] (release-item w eid status))})

(defn- unchanged [w _]
  w)

(defn apply-event
  "Returns world with one input delta applied."
  [world delta]
  ((get input-apply (nth delta 0) unchanged) world delta))

(def ^:private move-keys
  [:on-ground :sprinting? :pose :swimming? :eye-in-water? :in-water?
   :landed])

(defn- jump? [e e']
  (and (:on-ground e) (not (:on-ground e'))
       (> (v/y (:pos e')) (v/y (:pos e)))))

(defn move-of
  "Returns what a :move event did to its player as vanilla counts it,
   or nil when the event moved no player."
  [w w' [tag eid changes]]
  (let [e (get-in w [:entities eid]) e' (get-in w' [:entities eid])]
    (when (and (= :move tag) (:pos changes) (:pos e) e'
               (not (:tp-target e)) (not (:sleeping e)))
      (assoc (select-keys e' move-keys)
             :eid eid :from (:pos e) :to (:pos e') :jump? (jump? e e')
             :climbing?
             (climb/on-climbable? (:chunks w') (:pos e'))))))

(defn infinite-materials?
  "Returns true when a player builds and crafts without running out of
   items."
  [_player]
  true)

(defn quit-of
  "Returns the player a :player-quit event takes out of the world as
   they were, or nil for other events."
  [w [tag eid]]
  (when (= :player-quit tag)
    (when-let [e (get-in w [:entities eid])]
      (assoc e :eid eid))))

(defn- remembered [w quits]
  (cond-> w (seq quits) (assoc :quits quits)))

(defn- applied-input
  "Returns world with a tick's input events applied, remembering the pose
   each use started from, what each move did and who quit."
  [world input]
  (loop [w world i 0 origins {} moves [] quits []]
    (if-let [d (nth input i nil)]
      (let [w' (apply-event w d) m (move-of w w' d) q (quit-of w d)]
        (recur w' (inc i)
               (if-let [o (use-origin w d)]
                 (assoc origins i o)
                 origins)
               (cond-> moves m (conj m))
               (cond-> quits q (conj q))))
      (-> (assoc w :use-origins origins :moves moves)
          (remembered quits)))))

(defn- merge-diff [cur add drop]
  (set/difference (into (or cur (i/int-set)) add) (set drop)))

(defn- listed
  "Returns w with names added to and dropped from the player list."
  [w add drop]
  (update w :listed #(clojure.core/apply dissoc (merge % add) drop)))

(defn- flush-ticks [w t parked]
  (update w :block-ticks
          (fn [bt]
            (let [stale (into [] (take-while #(<= (long %) (long t))) (keys bt))
                  bt (reduce dissoc bt stale)]
              (cond-> bt (seq parked) (assoc t (into (i/int-set) parked)))))))

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
      (assoc e :health (- health (- amount last-d)) :last-damage amount)
      e)))

(defn- hurt-fully [e health amount dx dz]
  (cond-> (assoc e :health (max 0.0 (- (double health) (double amount)))
                   :last-damage amount
                   :hurt-resist max-resist)
          dx (knock-back (double dx) (double dz))))

(defn hurt
  "Returns entity e after amount of damage, knocked back from direction dx dz
   when given; no change inside the hurt resistance window."
  ([e ^double amount] (hurt e amount nil nil))
  ([e ^double amount dx dz]
   (let [health (double (or (:health e) 0.0))
         resist (long (or (:hurt-resist e) 0))]
     (cond
       (not (pos? health)) e
       (> resist (/ max-resist 2.0)) (hurt-again e health amount)
       :else (hurt-fully e health amount dx dz)))))

(def entity-apply
  {:merge-entity (fn [_ e [_ _ m]] (merge e m))
   :teleport     (fn [tick e [_ _ pos]] (assoc e :pos (v/v3 pos) :tp-target pos :tp-id tick))
   :client-slots (fn [_ e [_ _ slots carried]] (client-slots e slots carried))
   :award        (fn [_ e [_ _ k n]]
                   (update e :awards (fnil conj []) [k n]))
   :track        (fn [_ e [_ _ tr]] (assoc e :track tr))
   :tracking     (fn [_ e [_ _ add drop]] (update e :tracking merge-diff add drop))
   :set-slot     (fn [_ e [_ _ slot stack]]
                   (if stack (assoc-in e [:inventory slot] stack) (update e :inventory dissoc slot)))
   :chunks-sent  (fn [_ e [_ _ add drop]] (update e :sent-chunks merge-diff add drop))
   :damage       (fn [_ e [_ _ amount dx dz]] (hurt e amount dx dz))
   :push         (fn [_ e [_ _ vel]]
                   (update e (if (= :tnt (:type e)) :kb :vel) (fnil v/+ [0.0 0.0 0.0]) vel))})

(defn- apply-entity-delta [tick e delta]
  ((get entity-apply (nth delta 0)) tick e delta))

(defn- spawned [w spec]
  (let [eid (long (:next-eid w 1000000))]
    (-> w
        (assoc-in [:entities eid] (entity/of spec))
        (assoc :next-eid (inc eid)))))

(defn- scheduled [w at-ids]
  (update w :block-ticks
          (fn [bt] (reduce (fn [bt [at ids]] (update bt (long at) (fnil into (i/int-set)) ids))
                           bt at-ids))))

(defn- rechecked [w pos at]
  (if at
    (assoc-in w [:container-rechecks pos] (long at))
    (update w :container-rechecks dissoc pos)))

(defn- shulker-animated [w pos a]
  (if a
    (update-in w [:shulker-anim pos] #(merge {:progress (float 0.0)} % a))
    (update w :shulker-anim dissoc pos)))

(defn- block-entity-set [w pos e]
  (let [cp (chunk/block-chunk pos)]
    (if e
      (assoc-in w [:block-entities cp pos] e)
      (update-in w [:block-entities cp] dissoc pos))))

(def world-apply
  {:remove-entity        (fn [w [_ eid]] (player-quit w eid))
   :listed               (fn [w [_ add drop]] (listed w add drop))
   :spawn-entity         (fn [w [_ spec]] (spawned w spec))
   :set-blocks           (fn [w [_ changes base]] (apply-set-blocks w changes (long (or base (:tick w)))))
   :ticks-flushed        (fn [w [_ t parked]] (flush-ticks w t parked))
   :schedule-ticks       (fn [w [_ at-ids]] (scheduled w at-ids))
   :container-recheck    (fn [w [_ pos at]] (rechecked w pos at))
   :shulker-anim         (fn [w [_ pos a]] (shulker-animated w pos a))
   :block-events-flushed (fn [w _] (assoc w :block-events nil))
   :set-time             (fn [w [_ t]] (assoc w :time-of-day (long t)))
   :set-rule             (fn [w [_ rule value]] (assoc-in w [:rules rule] value))
   :set-world-spawn      (fn [w [_ pos]] (assoc w :world-spawn (vec pos)))
   :set-weather          (fn [w [_ m]] (merge w (select-keys m weather/fields)))
   :set-block-entity     (fn [w [_ pos e]] (block-entity-set w pos e))
   :advance-tick         (fn [w _] (dissoc (advance w) :quits))
   :advance-weather      (fn [w _] (merge w (weather/advance w)))
   :observed             (fn [w [_ m]] (assoc w :observed m))
   :explode              (fn [w _] w)})

(defn- apply-world-delta [w delta]
  (let [tag (nth delta 0)]
    (if-let [f (get world-apply tag)]
      (f w delta)
      (if-let [g (get entity-apply tag)]
        (update-entity w (nth delta 1) (fn [e] (g (:tick w) e delta)))
        w))))

(defn- folded-entities
  "Returns the entities that changed, each with its deltas applied."
  [w entities pairs]
  (r/fold fold-leaf (r/monoid i/merge i/int-map)
          (fn [m [eid ds]]
            (if-let [e (get entities eid)]
              (assoc m eid (reduce #(apply-entity-delta (:tick w) %1 %2) e ds))
              m))
          pairs))

(defn apply
  "Returns the world with deltas applied."
  [world deltas]
  (let [^Deltas d (if (instance? Deltas deltas) deltas (deltas/add deltas/empty-deltas deltas))
        [w removes] (reduce (fn [[w removes] delta]
                              (if (identical? :remove-entity (nth delta 0))
                                [w (conj removes (nth delta 1))]
                                [(apply-world-delta w delta) removes]))
                            [world []] (.world d))
        w (if (seq (.input d)) (applied-input w (.input d)) w)
        entities (:entities w)
        updated (folded-entities w entities (vec (.entities d)))
        w (if (pos? (count updated)) (assoc w :entities (i/merge entities updated)) w)]
    (cache-active-chunks (reduce player-quit w removes))))

(defn apply-deltas
  "Returns [world' deltas] like apply, for callers that need both."
  [world deltas]
  (let [d (if (instance? Deltas deltas) deltas (deltas/add deltas/empty-deltas deltas))]
    [(apply world d) d]))
