(ns collider.game.player
  "Players joining and quitting, with their hands, moves and the
  events they send."
  (:require [collider.data :as data]
            [collider.data.long-map :as lm]
            [collider.game.attribute :as attribute]
            [collider.game.book :as book]
            [collider.game.glide :as glide]
            [collider.game.jump :as jump]
            [collider.game.entity :as entity]
            [collider.game.entity.size :as size]
            [collider.game.food :as food]
            [collider.game.mode :as game-mode]
            [collider.game.level :as level :refer [update-entity]]
            [collider.game.level.writes :as writes]
            [collider.game.out :as out]
            [collider.game.ride :as ride]
            [collider.game.schema :as schema]
            [collider.game.slots :as slots]
            [collider.game.stack :as stack]
            [collider.game.scoreboard :as scoreboard]
            [collider.game.team :as team]
            [collider.game.using :as using]
            [collider.random :as random]
            [collider.math :as math]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]
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

(defn sees-particles?
  "Returns true when the centre of the block a player at pos stands in
  is nearer to at than 32 blocks, or 512 when forced."
  [pos at force?]
  (let [r (if force? 512.0 32.0)]
    (< (v/dist-sq (v/centre (v/cell pos)) at) (* r r))))

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

(defn respawn-dimension
  "Returns the dimension of the world spawn."
  [w]
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
  It moves inside the world border as the chunks of its
  level stand now."
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
        saved (dissoc (get-in w [:profiles name])
                      :dimension :root-vehicle)
        e (entity/of (moded w (merge fresh saved)))
        tick (:tick w)]
    (-> e (awaiting-join tick) (load-awaited tick)
        (assoc :born tick :xp-sent (:xp-total e 0)
               :hud-sent (food/hud e)))))

(defn- born [w spec]
  (let [id (long (:next-eid w schema/first-eid))]
    [(writes/spawn-entity w [:spawn-entity spec]) id]))

(defn placed
  "Returns level w after the delta [:player-placed eid name pos]."
  [w [_ eid name pos]]
  (let [parked (get-in w [:profiles name :root-vehicle])
        player (joining w eid name pos)
        [w player] (ride/mount w eid player parked born)]
    (-> w
        (assoc-in [:entities eid] player)
        (assoc-in [:players name] eid)
        (update :spawning dissoc eid))))

(defn respawn
  "Returns w after the event [:respawn eid] of a dead player."
  [w [_ eid]]
  (let [e (get-in w [:entities eid])]
    (if (and e (not (pos? (double (:health e))))
             (not (get-in w [:spawning eid])))
      (-> w
          (assoc-in [:spawning eid]
                    {:respawn? true :seed (spawn-seed w eid)
                     :dim (:dim w :overworld)})
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

(defn- gone [w e eid]
  (let [nm (str (entity/uuid-of eid e))
        held? (contains? (scoreboard/holders (:scoreboard w)) nm)]
    (cond-> w
      (seq (:teams w)) (update :teams team/left [nm])
      held? (update :scoreboard scoreboard/forgotten nm))))

(defn- parked [w eid e]
  (let [boat (when e (ride/carried (:entities w) (:tick w) eid))
        w (if boat (ride/drop-carried w eid) w)]
    [w (cond-> e boat (assoc :root-vehicle boat))]))

(defn quit
  "Returns level w without entity eid. A player keeps its profile,
  and any other entity that goes for good, not moved to another
  level, leaves its team and its scores."
  ([w eid] (quit w eid nil))
  ([w eid why]
   (let [{:keys [name] :as e} (get-in w [:entities eid])
         [w e] (parked w eid e)]
     (cond-> (-> w
                 (update :spawning dissoc eid)
                 (update :entities dissoc eid)
                 (update :players forget-player name eid))
       name (kept-profile name e)
       (and e (not (entity/player? e)) (not= :moved why))
       (gone e eid)))))

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

(defn snap-turned
  "Returns e turned to yaw cut to under a turn either way and pitch
  clamped to straight up or down. An angle that is not finite keeps
  the old one."
  [e yaw pitch]
  (let [y (math/frem yaw 360.0)
        x (Math/clamp (double pitch) -90.0 90.0)]
    (cond-> e
      (Double/isFinite y) (assoc :yaw y)
      (Double/isFinite x) (assoc :pitch x))))

(defn hand-slot
  "Returns the inventory slot the hand holds."
  ^long [e hand]
  (if (= :off hand)
    slots/offhand
    (+ slots/hotbar (long (or (:held-slot e) 0)))))

(defn hand-stack
  [e hand]
  (get-in e [:inventory (hand-slot e hand)]))

(defn- snapped [e hand rot]
  (let [y (math/wrap-degrees (:yaw rot 0.0))
        x (math/wrap-degrees (:pitch rot 0.0))]
    (if (and rot (:item (hand-stack e hand))
             (not (and (== y (double (:yaw e 0.0)))
                       (== x (double (:pitch e 0.0))))))
      (snap-turned e y x)
      e)))

(defn equipment
  "Returns the stacks that player e wears and holds,
  by equipment slot."
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
  The player sees its own swing only when the server, not the
  client, started it."
  [eid e hand t self?]
  (when (swing-free? e (long t))
    (let [kind (if (= :off hand) :swing-off :swing)
          fx (out/animation eid kind)]
      (cond-> [[:merge-entity eid
                {:swing-at (long t) :swing-hand hand}]
               (out/all fx)]
        self? (conj (out/to eid fx))))))

(defn server-swing-deltas
  "Returns the deltas of player eid of world swinging the hand it uses,
  a swing the server starts for a use the client does not foresee."
  [world eid]
  (let [e (get-in world [:entities eid])]
    (swing-deltas eid e (:use-hand e :main) (:tick world) true)))

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

(def ^:private ^:table arrows
  (delay (set (data/tag-values "item" "arrows"))))

(def ^:private projectile-order
  (vec (concat (range slots/hotbar (inc slots/hotbar-end)) slots/main
               (map slots/armor [:feet :legs :chest :head])
               [slots/offhand])))

(defn- arrow? [stack] (contains? @arrows (:item stack)))

(defn- held-projectile? [weapon stack]
  (or (arrow? stack)
      (and (= :crossbow weapon) (= :firework-rocket (:item stack)))))

(defn- slot-where [e ok? slots]
  (first (filter #(ok? (get-in e [:inventory %])) slots)))

(defn projectile-slot
  "Returns the slot of the projectile that weapon of player e takes,
  or nil. The hands come first, the off one before the main, and hold
  a rocket for a crossbow too."
  [e weapon]
  (or (slot-where e #(held-projectile? weapon %)
                  [slots/offhand (hand-slot e :main)])
      (slot-where e arrow? projectile-order)))

(defn- unarmed? [e stack]
  (let [weapon (:item stack)]
    (and (#{:bow :crossbow} weapon)
         (or (and (= :crossbow weapon)
                  (seq (stack/component stack :charged-projectiles)))
             (and (not (game-mode/instabuild? e))
                  (nil? (projectile-slot e weapon)))))))

(defn- start-use [e hand ^long tick]
  (let [stack (hand-stack e hand)
        n (using/ticks stack)]
    (cond
      (:using e) e
      (game-mode/spectator? e) e
      (on-cooldown? e (:item stack) tick) e
      (food/refuses? e (:item stack)) e
      (unarmed? e stack) e
      n (assoc e
          :using-item? true
          :using {:hand hand :item (:item stack) :started tick
                  :remaining n})
      (sword? (:item stack)) (assoc e :using-item? true)
      :else e)))

(defn- used [w eid hand rot]
  (-> (update-entity w eid snapped hand rot)
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
    (update-entity w eid snapped :main rot)))

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
      (-> w
          (update-entity eid assoc :pos (v/v3 (:tp-target e))
                         :tp-target nil)
          (update-entity eid snap-turned (:yaw e 0.0) (:pitch e 0.0)))
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

(defn- jumps? [e changes new]
  (and new (:on-ground e) (false? (:on-ground changes))
       (> (v/y new) (v/y (:pos e)))))

(defn- came-from [pos vel]
  (v/v3 (- (v/x pos) (v/x vel)) (v/y pos) (- (v/z pos) (v/z vel))))

(defn- support-changes
  "Returns the block player e rests on after a move by vel to pos,
  looked for where it came from when none is under it, and whether
  none is. A move up or down forgets that none was."
  [w e pos vel on-ground]
  (if on-ground
    (let [half (double (first (size/box e)))
          c (phys/context e)
          at #(phys/supporting-block (:chunks w) % half c)
          s (or (at pos)
                (when (and vel (not (and (:no-blocks? e)
                                         (zero? (v/y vel)))))
                  (at (came-from pos vel))))]
      {:support s :no-blocks? (nil? s)})
    {:support nil :no-blocks? false}))

(defn- free-move [w eid e changes client?]
  (let [new (some-> (:pos changes) clamped v/v3)
        vel (move-vel (:pos e) new)
        changes (cond-> changes
                  new (assoc :pos new)
                  (jumps? e changes new)
                  (assoc :vel (jump/jumped w e (or (:vel e) [0.0 0.0 0.0]))
                         :needs-sync? true))]
    (update-entity w eid merge changes
                   (when client? (known-move vel))
                   (when (contains? changes :on-ground)
                     (support-changes w e (or new (:pos e)) vel
                                      (:on-ground changes)))
                   (fall-changes e changes vel))))

(defn- sent-back [w eid e changes]
  (let [o (:pos e) p (some-> (:pos changes) clamped)]
    (if (and p (> (v/dist-sq o p) 1.0))
      (update-entity w eid assoc
                     :tp-target [(v/x o) (v/y o) (v/z o)]
                     :tp-id (next-teleport-id e) :tp-at (:tick w)
                     :yaw (+ 0.0 (double (:yaw changes)))
                     :pitch (+ 0.0 (double (:pitch changes))))
      w)))

(defn- apply-move [w eid changes client?]
  (let [e (get-in w [:entities eid])]
    (cond
      (and (:vehicle e) client?)
      (update-entity w eid merge (select-keys changes [:yaw :pitch]))
      (:tp-target e)
      (update-entity w eid merge (dissoc changes :pos :on-ground))
      (and (:sleeping e) client?) (sent-back w eid e changes)
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
    6 (update-entity w eid glide/started)
    w))

(defn move-turn
  "Returns the yaw and pitch that move changes ask of player e, cut to
  under a turn either way. An angle the move leaves out is e's own."
  [e changes]
  [(math/wrap-degrees (:yaw changes (:yaw e 0.0)))
   (math/wrap-degrees (:pitch changes (:pitch e 0.0)))])

(defn move
  "Returns w after the event [:move eid changes]."
  [w [_ eid changes]]
  (let [e (get-in w [:entities eid])
        [y x] (move-turn e changes)
        turn (snap-turned (select-keys e [:yaw :pitch]) y x)]
    (apply-move (resent w eid) eid
                (merge (dissoc changes :yaw :pitch) turn) true)))

(defn release-use
  "Returns w after the event [:release-use eid]."
  [w [_ eid]]
  (update-entity w eid stopped-use))

(defn client-settings
  "Returns w after the event [:client-settings eid settings]."
  [w [_ eid {:keys [view-distance chat-visibility]}]]
  (update-entity w eid assoc
                 :view-distance (long (or view-distance 2))
                 :chat-visibility (or chat-visibility :full)))

(defn- hat? [skin] (bit-test (long (or skin 0)) 6))

(defn settings-deltas
  "Returns the deltas of the event [:client-settings eid settings].
  Player eid shows its skin parts and main hand, and the player list
  of everyone its hat when it came or went."
  [world [tag eid {:keys [skin-parts main-hand]}]]
  (when (= :client-settings tag)
    (when-let [e (get-in world [:entities eid])]
      (let [skin (long (or skin-parts 0))]
        (cond-> [[:merge-entity eid {:skin-parts skin
                                     :main-hand (or main-hand :right)}]]
          (not= (hat? skin) (hat? (:skin-parts e)))
          (conj (out/all (out/tab-hat (:uuid e) (hat? skin)))))))))

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
