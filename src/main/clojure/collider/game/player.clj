(ns collider.game.player
  "Players joining and quitting, with their hands, moves and the
  events they send."
  (:require [collider.data :as data]
            [collider.data.long-map :as lm]
            [collider.game.attribute :as attribute]
            [collider.game.book :as book]
            [collider.game.entity :as entity]
            [collider.game.food :as food]
            [collider.game.mode :as game-mode]
            [collider.game.level :as level :refer [update-entity]]
            [collider.game.out :as out]
            [collider.game.schema :as schema]
            [collider.game.slots :as slots]
            [collider.game.stack :as stack]
            [collider.game.using :as using]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.space.column :as column])
  (:import (java.nio.charset StandardCharsets)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn offline-uuid
  "Returns the uuid an offline player of that name always gets."
  ^UUID [^String name]
  (let [s (str "OfflinePlayer:" name)]
    (UUID/nameUUIDFromBytes
     (String/.getBytes s StandardCharsets/UTF_8))))

(defn- spawn-seed ^double [w eid]
  (random/of-longs (long (:tick w 0)) (long eid) (hash :spawn)))

(defn joins
  "Returns [:player-join eid name] for every player placed in d."
  [d]
  (for [[tag eid name] (:world d) :when (= :player-placed tag)]
    [:player-join eid name]))

(defn- new-player [name tick pos]
  (merge
    {:type         :player :name name :uuid (offline-uuid name)
     :pos          pos :yaw 0.0 :pitch 0.0 :on-ground true
     :client-vel   [0.0 0.0 0.0]
     :chunk-pos    nil :sent-chunks (lm/long-set)
     :chunk-rate   9.0 :chunk-quota 0.0 :batches-unacked 0
     :batches-max  1
     :tracking (lm/long-set) :track nil
     :inventory    {} :held-slot 0
     :sneaking?    false :sprinting? false :skin-parts 0 :ping 0
     :view-distance 2 :chunk-view nil
     :health       20.0
     :health-sent  20.0
     :keepalive-at tick :keepalive-pending? false}
    food/fresh))

(defn- in-border? [[x _ z]]
  (let [b chunk/world-border]
    (and (<= (- b) (long x)) (< (long x) b)
         (<= (- b) (long z)) (< (long z) b))))

(defn- respawn-dimension [w]
  (let [dim (:world-spawn-dimension w :overworld)]
    (if (some #{dim} schema/dims) dim :overworld)))

(defn- respawn-level [w]
  (let [dim (respawn-dimension w)
        server (or (:server w) (when (:levels w) w))]
    (cond (= dim (:dim w)) w
          server (level/level server dim)
          :else (select-keys (level/bounds dim) [:min-y]))))

(defn- centre-top [lv]
  [0 (max (long (:min-y lv chunk/min-y))
          (column/motion-blocking-height (:chunks lv {}) 0 0))
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
  (let [pos (vec (:world-spawn w schema/world-spawn))]
    (if (in-border? pos) pos (centre-top (respawn-level w)))))

(defn set-world-spawn
  "Returns w after the delta [:set-world-spawn dim pos turn]."
  [w [_ dim pos turn]]
  (assoc w :world-spawn (vec pos) :world-spawn-dimension dim
           :world-spawn-turn (mapv double turn)))

(defn join
  "Returns w after the event [:player-join eid name settings].
  The player waits in :spawning for its chunks."
  [w [_ eid name settings]]
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

(defn- joining [w eid name pos]
  (let [[yaw pitch] (spawn-turn w)
        fresh (merge (new-player name (:tick w) pos)
                     {:yaw yaw :pitch pitch}
                     (get-in w [:spawning eid :settings]))
        saved (dissoc (get-in w [:profiles name]) :dimension)
        e (entity/of (moded w (merge fresh saved)))
        tick (:tick w)]
    (-> e (awaiting-join tick) (load-awaited tick)
        (assoc :born tick :xp-sent (:xp-total e 0)
               :hud-sent (food/hud e)))))

(defn placed
  "Returns level w after the delta [:player-placed eid name pos]."
  [w [_ eid name pos]]
  (-> w
      (assoc-in [:entities eid] (joining w eid name pos))
      (assoc-in [:players name] eid)
      (update :spawning dissoc eid)))

(defn respawn
  "Returns w after the event [:respawn eid] of a dead player."
  [w [_ eid]]
  (let [e (get-in w [:entities eid])]
    (if (and e (not (pos? (double (:health e))))
             (not (get-in w [:spawning eid])))
      (-> w
          (assoc-in [:spawning eid]
                    {:respawn? true :seed (spawn-seed w eid)
                     :dim :overworld})
          (update-entity eid load-awaited (:tick w)))
      w)))

(def ^:private crafting-slots
  (into [slots/crafting-result] slots/crafting-grid))

(defn- stored-profile [e dim]
  (schema/profile-of
    (-> e
        (update-in [:stats :custom/leave-game] (fnil inc 0))
        (update :inventory #(apply dissoc % crafting-slots)))
    dim))

(defn- forget-player [ps name eid]
  (if (= eid (get ps name)) (dissoc ps name) ps))

(defn- kept-profile [w name e]
  (let [dim (:dim w :overworld)]
    (assoc-in w [:profiles name] (stored-profile e dim))))

(defn quit
  "Returns level w without player eid, its profile kept."
  [w eid]
  (let [{:keys [name] :as e} (get-in w [:entities eid])]
    (cond-> (-> w
                (update :spawning dissoc eid)
                (update :entities dissoc eid)
                (update :players forget-player name eid))
            name (kept-profile name e))))

(def ^:private ^:table swords
  (delay (set (data/tag-values "item" "swords"))))

(defn sword?
  [item]
  (contains? @swords item))

(defn- stopped-use [e]
  (assoc e :using-item? false :using nil))

(defn- switched [e ^long slot]
  (cond-> {:held-slot slot}
          (and (not= slot (long (or (:held-slot e) 0)))
               (not= :off (get-in e [:using :hand])))
          (assoc :using-item? false :using nil)))

(defn- held-item-of [e]
  (let [slot (+ slots/hotbar (long (or (:held-slot e) 0)))]
    (get-in e [:inventory slot :item])))

(defn- snapped [e rot]
  (if (and rot (held-item-of e))
    (assoc e :yaw (v/wrap-deg (double (:yaw rot)))
             :pitch (v/wrap-deg (double (:pitch rot))))
    e))

(def ^:private origin-keys [:pos :yaw :pitch :sneaking? :flying])

(def ^:private state-keys
  [:pos :yaw :pitch :on-ground :fall :sprinting? :sneaking? :in-water?
   :eye-in-water? :pose :swimming?])

(defn use-origin
  "Returns the eye and look of the player behind an event, or for a
  dig, an attack or an interaction what its player was when the event
  came. Other events give nil."
  [w [tag & args]]
  (let [rot (case tag
              :place (nth args 6 nil)
              :use-item (nth args 3 nil)
              nil)]
    (when-let [e (get-in w [:entities (first args)])]
      (case tag
        (:place :use-item) (select-keys (snapped e rot) origin-keys)
        (:dig :attack :interact) (select-keys e state-keys)
        nil))))

(defn hand-slot
  "Returns the inventory slot the hand holds."
  ^long [e hand]
  (if (= :off hand)
    slots/offhand
    (+ slots/hotbar (long (or (:held-slot e) 0)))))

(defn hand-stack
  [e hand]
  (get-in e [:inventory (hand-slot e hand)]))

(defn equipment
  "Returns the stacks that player e wears and holds, by equipment
  slot."
  [e]
  (let [at (assoc slots/armor :mainhand (hand-slot e :main)
                  :offhand slots/offhand)]
    (into {}
          (keep (fn [[k i]]
                  (when-let [s (get-in e [:inventory i])] [k s])))
          at)))

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

(defn on-cooldown?
  "Returns true when the cooldown group of item is still locked."
  [e item ^long tick]
  (boolean
    (let [group (data/cooldown-group item)]
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
        n (using/ticks stack)]
    (cond
      (:using e) e
      (game-mode/spectator? e) e
      (on-cooldown? e (:item stack) tick) e
      (food/refuses? e (:item stack)) e
      n (assoc e
          :using-item? true
          :using {:hand hand :item (:item stack) :started tick
                  :remaining n})
      (sword? (:item stack)) (assoc e :using-item? true)
      :else e)))

(defn- used [w eid hand rot]
  (-> (update-entity w eid snapped rot)
      (update-entity eid start-use hand (long (:tick w)))))

(defn use-item
  "Returns w after the event [:use-item eid hand seq rot]."
  [w [_ eid hand _ rot]]
  (used w eid hand rot))

(def ^:private ^:const no-face 255)

(defn place
  "Returns w after the event [:place eid pos face ...] as its player
  turns and, clicking no block, uses the item in hand."
  [w [_ eid _ face _ _ _ rot]]
  (if (= no-face (bit-and (long face) 0xFF))
    (used w eid :main rot)
    (update-entity w eid snapped rot)))

(defn- seen-slots [m slots]
  (reduce (fn [m [s st]] (if st (assoc m s st) (dissoc m s)))
          (or m {})
          slots))

(defn client-slots
  "Returns player e with slots and carried as its client holds them."
  [e slots carried]
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
    (:held-item :creative-slot :edit-book) true
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
        :edit-book (book/edit-deltas e eid (long slot) stack)
        :dig (when-not (game-mode/spectator? e)
               (swap-deltas e eid))))))

(defn slot-part
  "Returns the slot deltas of event ev, none when they fail.
  A failed event keeps this part in every fold of the tick."
  [world ev]
  (try (slot-deltas world ev)
       (catch Throwable _ nil)))

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

(defn teleport-ack
  "Returns w after the event [:teleport-ack eid id]."
  [w [_ eid id]]
  (let [e (get-in w [:entities eid])]
    (if (and (:tp-target e) (= (long id) (long (:tp-id e 1))))
      (update-entity w eid merge
                     {:pos (v/v3 (:tp-target e)) :tp-target nil})
      w)))

(defn- fall-changes [e changes vel]
  (let [fall (double (or (:fall e) 0.0))
        dy (if vel (v/y vel) 0.0)
        wet? (:in-water? changes (:in-water? e))
        fall (if (and (not wet?) (neg? dy))
               (- fall (double (unchecked-float dy)))
               fall)]
    (cond
      (or (:flying e) (:flying changes)) {:fall 0.0 :landed nil}
      (:on-ground changes)
      {:fall 0.0 :landed (when (pos? fall) fall)}
      (pos? dy) {:fall 0.0 :landed nil}
      :else {:fall fall :landed nil})))

(defn- move-vel [old new]
  (when (and old new)
    (v/v3 (- (v/x new) (v/x old))
          (- (v/y new) (v/y old))
          (- (v/z new) (v/z old)))))

(def ^:private still (v/v3 0.0 0.0 0.0))

(defn- still? [m]
  (and m (== 0.0 (v/x m) (v/y m) (v/z m))))

(defn- known-move [vel]
  {:client-vel (or vel still) :client-moved? true})

(defn- free-move [w eid e changes client?]
  (let [new (some-> (:pos changes) clamped v/v3)
        vel (move-vel (:pos e) new)
        changes (cond-> changes new (assoc :pos new))]
    (update-entity w eid merge changes
                   (when client? (known-move vel))
                   (fall-changes e changes vel))))

(defn- apply-move [w eid changes client?]
  (let [e (get-in w [:entities eid])]
    (cond
      (:tp-target e)
      (update-entity w eid merge (dissoc changes :pos :on-ground))
      (:sleeping e) (update-entity w eid merge (dissoc changes :pos))
      :else (free-move w eid e changes client?))))

(defn client-tick-end
  "Returns w after the event [:client-tick-end eid]."
  [w [_ eid]]
  (let [e (get-in w [:entities eid])]
    (cond
      (nil? e) w
      (:client-moved? e)
      (update-entity w eid assoc :client-moved? false)
      (still? (:client-vel e)) w
      :else (update-entity w eid assoc :client-vel still))))

(defn chunk-batch-ack
  "Returns w after the event [:chunk-batch-ack eid rate]."
  [w [_ eid rate]]
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

(defn keepalive-echo
  "Returns w after the event [:keepalive-echo eid id]."
  [w [_ eid id]]
  (let [e (get-in w [:entities eid])]
    (if (and e (:keepalive-pending? e)
             (= (long id) (long (:keepalive-at e -1))))
      (let [rtt (* 50 (- (long (:tick w)) (long id)))
            ping (quot (+ (* 3 (long (or (:ping e) 0))) rtt) 4)]
        (update-entity w eid assoc
                       :keepalive-pending? false
                       :ping ping))
      w)))

(defn abilities
  "Returns w after the event [:abilities eid changes].
  Only a player that may fly starts flying."
  [w [_ eid changes]]
  (let [may? (game-mode/may-fly? (get-in w [:entities eid]))
        flying (and may? (boolean (:flying changes)))]
    (apply-move w eid {:flying flying} false)))

(defn- sprinted
  "Returns player e with its sprint set to on?. Its movement speed
  goes out again unless the sprint stays off."
  [e on?]
  (cond-> (assoc e :sprinting? on?)
    (or on? (:sprinting? e))
    (update :dirty-attributes (fnil conj #{}) :movement-speed)))

(defn entity-action
  "Returns w after the event [:entity-action eid action]."
  [w [_ eid action]]
  (case (long action)
    0 (update-entity w eid assoc :leave-bed? true)
    1 (update-entity w eid sprinted true)
    2 (update-entity w eid sprinted false)
    w))

(defn move
  "Returns w after the event [:move eid changes]."
  [w [_ eid changes]]
  (apply-move (resent w eid) eid changes true))

(defn release-use
  "Returns w after the event [:release-use eid]."
  [w [_ eid]]
  (update-entity w eid stopped-use))

(defn client-settings
  "Returns w after the event [:client-settings eid settings]."
  [w [_ eid {:keys [view-distance skin-parts]}]]
  (update-entity w eid assoc
                 :view-distance (long (or view-distance 2))
                 :skin-parts (long (or skin-parts 0))))

(defn infinite-materials?
  "Returns true when the player builds without spending items.
  Its mode decides it."
  [player]
  (game-mode/instabuild? player))

(defn permission-level
  "Returns the permission level of player e.
  Every player is an operator of the top level unless it holds a level
  of its own."
  ^long [e]
  (long (:permission-level e 4)))

(defn block-reach
  "Returns how far the player reaches blocks, in blocks."
  ^double [player]
  (attribute/value player (:effects player) :block-interaction-range))

(defn entity-reach
  "Returns how far the player reaches entities, in blocks."
  ^double [player]
  (let [fx (:effects player)]
    (attribute/value player fx :entity-interaction-range)))

(defn arrived
  "Returns player e as it arrives in another level at pos.
  It knows no chunk and no entity there yet."
  [e tick pos yaw pitch]
  (assoc (stopped-use e)
         :pos (v/v3 pos) :yaw (double yaw) :pitch (double pitch)
         :tp-target pos :tp-id (next-teleport-id e) :tp-at tick
         :chunk-view nil
         :chunk-pos (chunk/pos-chunk pos) :chunks-pending? nil
         :sent-chunks (lm/long-set) :tracking (lm/long-set) :track nil
         :kept-mdata (:mdata (:track e))))

(defn use-slot
  "Returns the inventory slot of the item player eid uses."
  ^long [world eid]
  (let [e (get-in world [:entities eid])]
    (hand-slot e (:use-hand e))))

(defn use-stack
  "Returns the stack in the hand that player eid uses."
  [world eid]
  (get-in world [:entities eid :inventory (use-slot world eid)]))
