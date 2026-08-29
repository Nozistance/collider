(ns collider.game.state
  (:require [clojure.core.reducers :as r]
            [collider.vec :as v]
            [clojure.data.int-map :as i]
            [clojure.set :as set]
            [collider.game.entity :as entity]
            [collider.game.deltas :as deltas]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.light :as light]
            [collider.world.rules :as rules])
  (:import (collider.game.deltas Deltas)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn player-entries [world]

  (let [ents (:entities world)]
    (into [] (map (fn [eid] (clojure.lang.MapEntry/create eid (get ents eid))))
          (sort (vals (:players world))))))

(defn broadcast
  ([world pkt] (broadcast world nil pkt))
  ([world exclude pkt]
   (for [eid (sort (vals (:players world)))
         :when (not= eid exclude)]
     [:send eid pkt])))

(def spawn-pos [24.5 4.0 8.5])
(def activation-radius 2)

(defn pos-chunk
  ^long [pos]
  (chunk/pos->id (bit-shift-right (long (Math/floor (v/x pos))) 4)
                 (bit-shift-right (long (Math/floor (v/z pos))) 4)))

(defn active-chunks [world]
  (into (i/int-set)
        (mapcat (fn [[_ e]]
                  (when (= :player (:type e))
                    (let [[cx cz] (chunk/id->pos (pos-chunk (:pos e)))
                          r (long (get-in world [:config :simulation-distance]
                                          activation-radius))]
                      (chunk/around-ids (long cx) (long cz) r)))))
        (:entities world)))

(defn active-at? [active pos]
  (contains? active (pos-chunk pos)))

(defn active-id? [active ^long bid]
  (contains? active (chunk/pos->id (bit-shift-right bid 42)
                                   (bit-shift-right (bit-shift-left bid 38) 42))))

(defn offline-uuid
  ^UUID [^String name]
  (UUID/nameUUIDFromBytes (.getBytes (str "OfflinePlayer:" name) StandardCharsets/UTF_8)))

(def initial-world
  {:tick        0
   :time-ms     0
   :time-of-day 0
   :entities    (i/int-map)
   :next-eid    1000000
   :players     {}
   :profiles    {}
   :listed      {}
   :chunks      (i/int-map)
   :block-ticks (i/int-map)})

(defn- update-entity [w eid f & args]
  (if (get-in w [:entities eid])
    (apply update-in w [:entities eid] f args)
    w))

(def ^:private around
  [[0 0 0] [1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn- block-or-zero ^long [chunks [_ y _ :as p]]
  (if (or (neg? (long y)) (> (long y) 255))
    0
    (chunk/chunks-get-block chunks gen/flat-chunk p)))

(defn- wake-tick [chunks tick p old self?]
  (let [st (block-or-zero chunks p)]
    (when-not (zero? st)
      (rules/wake-tick chunks st tick p old self?))))

(defn- schedule-updates [bt tick chunks changed]
  (reduce
    (fn [bt [[x y z] old _]]
      (reduce
        (fn [bt [dx dy dz :as d]]
          (let [p [(+ (long x) (long dx)) (+ (long y) (long dy)) (+ (long z) (long dz))]]
            (if-let [at (wake-tick chunks tick p old (= [0 0 0] d))]
              (update bt at (fnil conj (i/int-set)) (chunk/block-pos->id p))
              bt)))
        bt
        around))
    bt
    changed))

(defn- apply-set-blocks [w changes record-events?]
  (let [chunks (:chunks w)
        real (into []
                   (keep (fn [[pos st]]
                           (let [old (chunk/chunks-get-block chunks gen/flat-chunk pos)]
                             (when (not= old (long st))
                               [pos old st]))))
                   changes)]
    (if (empty? real)
      w
      (let [chunks' (-> chunks
                        (chunk/chunks-set-blocks gen/flat-chunk
                                                 (mapv (fn [[pos _ st]] [pos st]) real))
                        (light/relight-batch gen/flat-chunk real))]
        (-> w
            (assoc :chunks chunks')
            (update :block-ticks schedule-updates (:tick w) chunks' real)
            (cond-> record-events?
              (update :block-events
                      (fn [ev]
                        (reduce (fn [ev [pos _ st]]
                                  (update ev (chunk/block-chunk pos) (fnil conj []) [pos st]))
                                (or ev (i/int-map)) real)))))))))

(defn- new-player [name tick]
  {:type           :player :name name :uuid (offline-uuid name)
   :pos            spawn-pos :yaw 0.0 :pitch 0.0 :on-ground true
   :chunk-pos      nil :sent-chunks (i/int-set) :needs-spawn? true
   :tracking       (i/int-set) :track nil
   :inventory      {} :held-slot 0
   :sneaking?      false :sprinting? false :skin-parts 0 :ping 0
   :health         20.0
   :health-sent    20.0
   :last-echo-tick tick})

(defn- player-join [w eid name]
  (-> w
      (assoc-in [:entities eid]
                (entity/of (merge (new-player name (:tick w)) (get-in w [:profiles name]))))
      (assoc-in [:players name] eid)))

(defn- player-quit [w eid]
  (let [{:keys [name inventory held-slot pos yaw pitch on-ground]} (get-in w [:entities eid])]
    (cond-> (-> w
                (update :entities dissoc eid)
                (update :players (fn [ps] (if (= eid (get ps name)) (dissoc ps name) ps))))
            name (assoc-in [:profiles name]
                           (cond-> {:inventory (or inventory {})
                                    :held-slot (or held-slot 0)}
                             pos (assoc :pos [(v/x pos) (v/y pos) (v/z pos)]
                                        :yaw (or yaw 0.0) :pitch (or pitch 0.0)
                                        :on-ground (boolean on-ground)))))))

(def ^:private sword-ids #{267 268 272 276 283})
(defn- held-item [w eid slot]
  (if (<= 0 (long slot) 8)
    (update-entity w eid assoc :held-slot (long slot) :using-item? false)
    w))

(defn- use-item [w eid face item-id]
  (if (and (= 255 (bit-and (long face) 0xFF))
           (sword-ids (long item-id)))
    (update-entity w eid assoc :using-item? true)
    w))

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

(defn- creative-slot [w eid slot stack]
  (let [slot (long slot)]
    (if (and (<= 1 slot 44)
             (or (nil? stack)
                 (and (<= 1 (long (:count stack 1)) 64)
                      (not (neg? (long (:damage stack 0)))))))
      (set-slot w eid slot stack)
      w)))

(def ^:private tp-tolerance 0.25)
(defn- near-target? [[ax ay az] [bx by bz]]
  (let [dx (- (double ax) (double bx))
        dy (- (double ay) (double by))
        dz (- (double az) (double bz))]
    (< (+ (* dx dx) (* dy dy) (* dz dz)) tp-tolerance)))

(defn- apply-move [w eid changes]
  (let [e (get-in w [:entities eid])
        target (:tp-target e)
        new (:pos changes)]
    (cond
      (and target new (near-target? new target))
      (update-entity w eid merge changes {:tp-target nil :client-vel [0.0 0.0 0.0]})
      target
      (update-entity w eid merge (dissoc changes :pos))
      :else
      (let [old (:pos e)
            new (some-> new v/v3)
            vel (when (and old new)
                  (v/v3 (- (v/x new) (v/x old)) (- (v/y new) (v/y old)) (- (v/z new) (v/z old))))
            changes (cond-> changes new (assoc :pos new))]
        (update-entity w eid merge changes (when vel {:client-vel vel}))))))

(defn- keepalive-echo [w eid id]
  (if-let [e (get-in w [:entities eid])]
    (let [t (long (:tick w))
          rtt (* 50 (bit-and (- t (long id)) 0xFFFFF))]
      (update-entity w eid assoc
                     :last-echo-tick t
                     :ping (quot (+ (* 3 (long (or (:ping e) 0))) rtt) 4)))
    w))

(def ^:private entity-actions
  {0 [:sneaking? true] 1 [:sneaking? false]
   3 [:sprinting? true] 4 [:sprinting? false]})

(defn- entity-action [w eid action]
  (if-let [[k v] (entity-actions (long action))]
    (update-entity w eid assoc k v)
    w))

(defn apply-event [world [tag & args]]
  (case tag
    :player-join (apply player-join world args)
    :player-quit (apply player-quit world args)
    :move (let [[eid changes] args] (apply-move world eid changes))
    :keepalive-echo (apply keepalive-echo world args)
    :entity-action (apply entity-action world args)
    :client-settings (let [[eid sp] args] (update-entity world eid assoc :skin-parts sp))
    :held-item (apply held-item world args)
    :creative-slot (apply creative-slot world args)
    :place (let [[eid _ face item-id] args] (use-item world eid face item-id))
    :dig (let [[eid status] args] (release-item world eid status))
    world))

(defn- merge-diff [cur add drop]
  (set/difference (into (or cur (i/int-set)) add) (set drop)))

(defn- tab-list [w add drop]
  (update w :listed #(apply dissoc (merge % add) drop)))

(defn- defer-ticks
  ([w t deferred] (defer-ticks w t deferred nil))
  ([w t deferred parked]
   (update w :block-ticks
           (fn [bt]
             (cond-> (dissoc bt t)
                     (seq deferred)
                     (update (inc (long t)) (fnil into (i/int-set)) deferred)
                     (seq parked)
                     (update (+ (long t) 20) (fnil into (i/int-set)) parked))))))

(def ^:const max-resist 20)
(defn- knock-back [e ^double dx ^double dz]
  (let [f (Math/sqrt (+ (* dx dx) (* dz dz)))
        v (or (:vel e) [0.0 0.0 0.0])]
    (if (zero? f)
      e
      (assoc e :vel [(- (/ (v/x v) 2.0) (* (/ dx f) 0.4))
                     (min 0.4 (+ (/ (v/y v) 2.0) 0.4))
                     (- (/ (v/z v) 2.0) (* (/ dz f) 0.4))]))))

(defn hurt
  ([e ^double amount] (hurt e amount nil nil))
  ([e ^double amount dx dz]
   (let [health (double (or (:health e) 0.0))
         resist (long (or (:hurt-resist e) 0))
         last-d (double (or (:last-damage e) 0.0))]
     (cond
       (not (pos? health)) e
       (> resist (/ max-resist 2.0))
       (if (> amount last-d)
         (assoc e :health (- health (- amount last-d)) :last-damage amount)
         e)
       :else (cond-> (assoc e :health (max 0.0 (- health amount))
                              :last-damage amount
                              :hurt-resist max-resist)
               dx (knock-back (double dx) (double dz)))))))

(defn- apply-entity-delta [e [tag & args]]
  (case tag
    :merge-entity (merge e (second args))
    :track (assoc e :track (second args))
    :tracking (let [[_ add drop] args] (update e :tracking merge-diff add drop))
    :spawned (assoc e :needs-spawn? nil)
    :set-slot (let [[_ slot stack] args]
                (if stack (assoc-in e [:inventory slot] stack) (update e :inventory dissoc slot)))
    :chunks-sent (let [[_ cp add drop pending?] args]
                   (-> e (assoc :chunk-pos cp :chunks-pending? pending?)
                       (update :sent-chunks merge-diff add drop)))
    :damage (let [[_ amount dx dz] args] (hurt e amount dx dz))
    :push (let [[_ v] args]
            (update e (if (= :tnt (:type e)) :kb :vel) (fnil v/+ [0.0 0.0 0.0]) v))))

(defn- apply-world-delta [w [tag & args :as delta]]
  (case tag
    :remove-entity (apply player-quit w args)
    :tab-list (apply tab-list w args)
    :spawn-entity (let [[a b] args]
                    (if (map? a)
                      (let [eid (long (:next-eid w 1000000))]
                        (-> w
                            (assoc-in [:entities eid] (entity/of a))
                            (assoc :next-eid (inc eid))))
                      (-> w
                          (assoc-in [:entities a] (entity/of b))
                          (update :next-eid (fnil max 1000000) (inc (long a))))))
    :set-block (apply-set-blocks w [(vec args)] false)
    :set-blocks (apply-set-blocks w (first args) true)
    :ticks-flushed (apply defer-ticks w args)
    :block-events-flushed (assoc w :block-events nil)
    :chunk-flush (let [[held resent t] args]
                   (assoc w
                          :dirty-chunks (when (seq held) held)
                          :chunk-resent (reduce (fn [m cp] (assoc m cp t))
                                                (or (:chunk-resent w) (i/int-map))
                                                resent)))
    :set-time (assoc w :time-of-day (long (first args)))
    (if (deltas/entity-tags tag)
      (update-entity w (first args) apply-entity-delta delta)
      w)))

(defn apply-deltas [world deltas]
  (let [^Deltas d (if (instance? Deltas deltas) deltas (deltas/bucket-deltas deltas/empty-deltas deltas))
        [w removes _] (reduce
                       (fn [[w removes seen :as acc] [tag & args :as delta]]
                         (case tag
                           :remove-entity [w (conj removes (first args)) seen]
                           :spawn-entity
                           (let [m (first args)]
                             (if-let [k (and (map? m) (:dedup m))]
                               (if (contains? seen k)
                                 acc
                                 [(apply-world-delta w [:spawn-entity (dissoc m :dedup)]) removes (conj seen k)])
                               [(apply-world-delta w delta) removes seen]))
                           [(apply-world-delta w delta) removes seen]))
                       [world [] #{}]
                       (.world d))
        ents    (:entities w)
        updated (r/fold 1 (r/monoid i/merge i/int-map)
                        (fn [m [eid ds]]
                          (if-let [e (get ents eid)]
                            (assoc m eid (reduce apply-entity-delta e ds))
                            m))
                        (vec (.ents d)))
        w (if (pos? (count updated)) (assoc w :entities (i/merge ents updated)) w)
        w (reduce player-quit w removes)]
    [w (.out d)]))
