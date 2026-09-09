(ns collider.game.state
  (:require [collider.game.sign :as sign]
            [clojure.core.reducers :as r]
            [clojure.string]
            [collider.vec :as v]
            [clojure.data.int-map :as i]
            [clojure.set :as set]
            [collider.game.entity :as entity]
            [collider.game.schema :as schema]
            [collider.game.deltas :as deltas]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.connect :as connect]
            [collider.world.gen :as gen]
            [collider.world.light :as light]
            [collider.world.rules :as rules])
  (:import (clojure.lang MapEntry)
           (collider.game.deltas Deltas)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn player-entries [world]
  (let [entities (:entities world)]
    (into [] (map (fn [eid] (MapEntry/create eid (get entities eid))))
          (sort (vals (:players world))))))

(def spawn-pos [24.5 4.0 8.5])
(def activation-radius 2)
(defn pos-chunk ^long [pos]
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

(defn offline-uuid ^UUID [^String name]
  (UUID/nameUUIDFromBytes (.getBytes (str "OfflinePlayer:" name) StandardCharsets/UTF_8)))

(def initial-world schema/initial-world)
(defn- update-entity [w eid f & args]
  (if (get-in w [:entities eid])
    (apply update-in w [:entities eid] f args)
    w))

(def ^:private around
  [[0 0 0] [1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn- block-or-zero ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y)
    (chunk/chunks-get-block chunks gen/flat-chunk p)
    0))

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

(defn- drop-block-entities [w real]
  (reduce (fn [w [pos old st]]
            (if (and (sign/kind old) (not= (block/block-of old) (block/block-of (long st))))
              (update-in w [:block-entities (chunk/block-chunk pos)] dissoc pos)
              w))
          w real))

(defn- apply-set-blocks [w changes]
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
                        (light/relight-batch gen/flat-chunk real))
            derived (connect/derived-changes chunks' (map first real) (:tick w))
            chunks' (chunk/chunks-set-blocks chunks' gen/flat-chunk derived)
            events  (concat (map (fn [[pos _ st]] [pos st]) real) derived)]
        (-> w
            (assoc :chunks chunks')
            (drop-block-entities real)
            (update :block-ticks schedule-updates (:tick w) chunks' real)
            (cond-> (seq events)
              (update :block-events
                      (fn [ev]
                        (reduce (fn [ev [pos st]]
                                  (update ev (chunk/block-chunk pos) (fnil conj []) [pos st]))
                                (or ev (i/int-map)) events)))))))))

(defn- new-player [name tick]
  {:type           :player :name name :uuid (offline-uuid name)
   :pos            spawn-pos :yaw 0.0 :pitch 0.0 :on-ground true
   :chunk-pos      nil :sent-chunks (i/int-set) :needs-spawn? true
   :chunk-rate     9.0 :chunk-quota 0.0 :batches-unacked 0 :batches-max 1
   :tracking       (i/int-set) :track nil
   :inventory      {} :held-slot 0
   :sneaking?      false :sprinting? false :skin-parts 0 :ping 0
   :health         20.0
   :health-sent    20.0
   :keepalive-at   tick :keepalive-pending? false})

(defn- player-join [w eid name]
  (-> w
      (assoc-in [:entities eid]
                (entity/of (merge (new-player name (:tick w)) (get-in w [:profiles name]))))
      (assoc-in [:players name] eid)))

(defn- vacated-bed [w eid]
  (if-let [pos (get-in w [:entities eid :sleeping :pos])]
    (let [st (chunk/chunks-get-block (:chunks w) gen/flat-chunk pos)]
      (if (= :bed (block/type-of st))
        (apply-set-blocks w [[pos (block/state (block/block-of st) (assoc (block/props-of st) :occupied :false))]])
        w))
    w))

(defn- player-quit [w eid]
  (let [{:keys [name] :as e} (get-in w [:entities eid])]
    (cond-> (-> (vacated-bed w eid)
                (update :entities dissoc eid)
                (update :players (fn [ps] (if (= eid (get ps name)) (dissoc ps name) ps))))
      name (assoc-in [:profiles name]
                     (schema/profile-of (update-in e [:stats [:custom :leave-game]] (fnil inc 0)))))))

(defn- sword? [item]
  (and (keyword? item) (clojure.string/ends-with? (name item) "-sword")))

(defn- held-item [w eid slot]
  (if (<= 0 (long slot) 8)
    (update-entity w eid assoc :held-slot (long slot) :using-item? false)
    w))

(defn- wrap-degrees ^double [^double d]
  (let [r (rem d 360.0)] (cond (>= r 180.0) (- r 360.0) (< r -180.0) (+ r 360.0) :else r)))

(defn- snapped [e rot]
  (if (and rot (get-in e [:inventory (+ 36 (long (or (:held-slot e) 0))) :item]))
    (assoc e :yaw (wrap-degrees (double (:yaw rot))) :pitch (wrap-degrees (double (:pitch rot))))
    e))

(defn use-origin [w [tag & args]]
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
      (update-entity w eid merge {:pos (v/v3 (:tp-target e)) :tp-target nil :tp-id nil
                                  :client-vel [0.0 0.0 0.0] :fall 0.0})
      w)))

(defn- fall-changes [e changes vel]
  (let [fall (double (or (:fall e) 0.0))
        dy (if vel (v/y vel) 0.0)]
    (cond
      (or (:flying e) (:flying changes)) {:fall 0.0}
      (:on-ground changes) (if (pos? fall) {:fall 0.0 :landed fall} {:fall 0.0})
      (neg? dy) {:fall (- fall dy)}
      :else nil)))

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

(defn apply-event [world [tag & args]]
  (case tag
    :player-join (apply player-join world args)
    :player-quit (apply player-quit world args)
    :move (let [[eid changes] args] (apply-move world eid changes))
    :teleport-ack (apply teleport-ack world args)
    :keepalive-echo (apply keepalive-echo world args)
    :chunk-batch-ack (apply chunk-batch-ack world args)
    :entity-action (apply entity-action world args)
    :input (let [[eid flags] args] (update-entity world eid merge flags))
    :client-settings (let [[eid sp] args] (update-entity world eid assoc :skin-parts sp))
    :held-item (apply held-item world args)
    :creative-slot (apply creative-slot world args)
    :place (let [[eid _ face item _ _ rot] args] (use-item world eid face item rot))
    :dig (let [[eid status] args] (release-item world eid status))
    world))

(defn- merge-diff [cur add drop]
  (set/difference (into (or cur (i/int-set)) add) (set drop)))

(defn- listed [w add drop]
  (update w :listed #(apply dissoc (merge % add) drop)))

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

(defn- apply-entity-delta [tick e [tag & args]]
  (case tag
    :merge-entity (merge e (second args))
    :teleport (let [[_ pos] args] (assoc e :pos (v/v3 pos) :tp-target pos :tp-id tick))
    :client-slots (let [[_ slots carried] args] (client-slots e slots carried))
    :track (assoc e :track (second args))
    :tracking (let [[_ add drop] args] (update e :tracking merge-diff add drop))
    :set-slot (let [[_ slot stack] args]
                (if stack (assoc-in e [:inventory slot] stack) (update e :inventory dissoc slot)))
    :chunks-sent (let [[_ add drop] args] (update e :sent-chunks merge-diff add drop))
    :damage (let [[_ amount dx dz] args] (hurt e amount dx dz))
    :push (let [[_ v] args]
            (update e (if (= :tnt (:type e)) :kb :vel) (fnil v/+ [0.0 0.0 0.0]) v))))

(defn- apply-world-delta [w [tag & args :as delta]]
  (case tag
    :remove-entity (apply player-quit w args)
    :listed (apply listed w args)
    :spawn-entity (let [eid (long (:next-eid w 1000000))]
                    (-> w
                        (assoc-in [:entities eid] (entity/of (first args)))
                        (assoc :next-eid (inc eid))))
    :set-blocks (apply-set-blocks w (first args))
    :ticks-flushed (let [[t parked] args] (flush-ticks w t parked))
    :schedule-ticks (update w :block-ticks
                            (fn [bt] (reduce (fn [bt [at ids]] (update bt (long at) (fnil into (i/int-set)) ids))
                                             bt (first args))))
    :block-events-flushed (assoc w :block-events nil)
    :set-time (assoc w :time-of-day (long (first args)))
    :set-rule (let [[rule value] args] (assoc-in w [:rules rule] value))
    :set-block-entity (let [[pos e] args cp (chunk/block-chunk pos)]
                        (if e
                          (assoc-in w [:block-entities cp pos] e)
                          (update-in w [:block-entities cp] dissoc pos)))
    (if (deltas/entity-tags tag)
      (update-entity w (first args) #(apply-entity-delta (:tick w) % delta))
      w)))

(defn apply-deltas [world deltas]
  (let [^Deltas d (if (instance? Deltas deltas) deltas (deltas/add deltas/empty-deltas deltas))
        [w removes] (reduce
                     (fn [[w removes] [tag & args :as delta]]
                       (if (= :remove-entity tag)
                         [w (conj removes (first args))]
                         [(apply-world-delta w delta) removes]))
                     [world []]
                     (.world d))
        entities    (:entities w)
        updated (r/fold 1 (r/monoid i/merge i/int-map)
                        (fn [m [eid ds]]
                          (if-let [e (get entities eid)]
                            (assoc m eid (reduce #(apply-entity-delta (:tick w) %1 %2) e ds))
                            m))
                        (vec (.entities d)))
        w (if (pos? (count updated)) (assoc w :entities (i/merge entities updated)) w)
        w (reduce player-quit w removes)]
    [w d]))
