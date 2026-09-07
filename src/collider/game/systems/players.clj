(ns collider.game.systems.players
  (:require [clojure.data.int-map :as i]
            [collider.vec :as vv]
            [collider.game.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.state :as state])
  (:import (java.util ArrayList Locale)))

(set! *warn-on-reflection* true)

(def update-interval 2)
(def resync-interval 60)
(def latency-interval 600)
(def forced-teleport 400)
(def ^:private vel-zero (vv/v3 0.0 0.0 0.0))
(def ^:const ^:private pos-unit 4096.0)
(def ^:const ^:private rel-limit 32767)
(defn- fixed ^long [v] (long (Math/floor (* (double v) pos-unit))))
(defn- angle ^long [v] (long (Math/floor (* (double v) (/ 256.0 360.0)))))
(defn metadata [e]
  (cond
    (= :item (:type e))
    {:stack (:stack e)}
    (= :tnt (:type e))
    {}
    (mobs/mob-type? (:type e))
    (mobs/metadata e)
    :else
    (cond-> {:burning?    (boolean (:burning? e))
             :sneaking?   (boolean (:sneaking? e))
             :sprinting?  (boolean (:sprinting? e))
             :using-item? (boolean (:using-item? e))
             :skin-parts  (long (or (:skin-parts e) 0))}
      (:sleeping e) (assoc :sleeping-pos (get-in e [:sleeping :pos])))))

(defn- held-stack [e]
  (get-in e [:inventory (+ 36 (long (or (:held-slot e) 0)))]))

(def ^:private no-equip [nil nil nil nil nil])
(defn- equipment-stacks [e]
  (let [inv (:inventory e)]
    (if (nil? inv)
      no-equip
      [(held-stack e) (get inv 8) (get inv 7) (get inv 6) (get inv 5)])))

(defrecord Track [pos yaw pitch head on-ground mdata equip vel-sent since-tp slots carried seen])
(defn- baseline [{:keys [pos yaw pitch on-ground] :as e}]
  (let [[x y z] pos]
    (->Track [(fixed x) (fixed y) (fixed z)]
             (angle yaw) (angle pitch) (angle (or (:head-yaw e) yaw))
             (boolean on-ground)
             (metadata e)
             (equipment-stacks e)
             (:vel e)
             0
             (when (= :player (:type e)) (or (:inventory e) {}))
             (:carried e)
             e)))

(defn- as-seen [s] (when s [(:item s) (long (:count s 1))]))

(defn- slot-diff
  "Slots whose stack the client has wrong (vanilla broadcastChanges against
   remoteSlots): [[slot stack] ...], compared by item and count."
  [inv known]
  (into []
        (keep (fn [slot]
                (let [ours (get inv slot) theirs (get known slot)]
                  (when (not= (as-seen ours) (as-seen theirs)) [slot ours]))))
        (into (sorted-set) (concat (keys inv) (keys known)))))

(defn- track-of [e] (or (:track e) (baseline e)))
(defn- tracked-entries [world]
  (into [] (filter (fn [[_ e]]
                     (let [t (:type e)]
                       (or (#{:player :item :tnt} t) (mobs/mob-type? t)))))
        (:entities world)))

(defn- viewer-index [ps]
  (persistent!
   (reduce (fn [acc [oid o]]
             (reduce (fn [a eid]
                       (if-let [^ArrayList l (get a eid)]
                         (do (.add l oid) a)
                         (assoc! a eid (doto (ArrayList. 4) (.add oid)))))
                     acc
                     (seq (:tracking o))))
           (transient (i/int-map))
           ps)))

(defn- entity-chunk ^long [e]
  (state/pos-chunk (:pos e)))

(def ^:private duplicate-login-reason "You logged in from another location")

(defn- duplicate-login-deltas [world events]
  (mapcat (fn [[tag _ pname]]
            (when (= :player-join tag)
              (let [owner (get-in world [:players pname])]
                (for [[eid e] (:entities world)
                      :when (and (= :player (:type e))
                                 (= pname (:name e))
                                 (not= eid owner))
                      d [(out/to eid (out/disconnect duplicate-login-reason))
                         (out/to eid (out/close))
                         [:remove-entity eid]]]
                  d))))
          events))

(defn- add-entry [e]
  {:uuid (:uuid e) :name (:name e) :ping (or (:ping e) 0)})

(defn- join-list-deltas [joined all]
  (mapcat (fn [[eid e]]
            [(out/to eid (out/tab-add all))
             (out/except eid (out/tab-add [(add-entry e)]))])
          joined))

(defn- leave-list-deltas [world live left]
  (let [listed (:listed world)]
    (mapcat (fn [eid]
              (when-not (contains? live (get listed eid))
                [(out/all (out/tab-remove [(get listed eid)]))]))
            left)))

(defn- list-deltas [world ps]
  (let [listed (:listed world)
        cur    (into {} (map (fn [[eid e]] [eid (:uuid e)])) ps)
        joined (remove (fn [[eid _]] (contains? listed eid)) ps)
        left   (sort (remove cur (keys listed)))
        all    (mapv (comp add-entry val) ps)]
    (concat
     (join-list-deltas joined all)
     (leave-list-deltas world (into #{} (map val) cur) left)
     (when (zero? (rem (long (:tick world)) latency-interval))
       [(out/all (out/tab-latency all))])
     (when (or (seq joined) (seq left))
       [[:listed (into {} (map (fn [[eid e]] [eid (:uuid e)])) joined) left]]))))

(defn- baseline-deltas [world eid]
  (let [e (get-in world [:entities eid])]
    (when-not (:track e) [[:track eid (baseline e)]])))

(defn- entities-by-chunk [ts]
  (persistent!
   (reduce (fn [m [eid e]]
             (let [c (entity-chunk e)]
               (assoc! m c (conj (get m c []) eid))))
           (transient (i/int-map))
           ts)))

(defn- tracking-deltas [world by-chunk [oid o]]
  (let [seen (or (:sent-chunks o) (i/int-set))
        want (into (i/int-set)
                   (comp (mapcat (fn [c] (get by-chunk c)))
                         (remove (fn [eid] (= (long eid) (long oid)))))
                   (seq seen))
        have (or (:tracking o) (i/int-set))
        add  (into [] (remove #(contains? have %)) (seq want))
        gone (into [] (remove #(contains? want %)) (seq have))]
    (when (or (seq add) (seq gone))
      (into [[:tracking oid add gone]]
            (mapcat (fn [eid] (baseline-deltas world eid)))
            add))))

(defrecord Frame [x y z dx dy dz yaw pitch head ground since due? vel mdata equip
                  moved? turned? rel? head-turned? meta-changed? equip-changed?
                  vel-changed? equip-diff slot-diff carried-changed?])

(def ^:private vel-threshold 4.0E-4)
(def ^:private item-vel-threshold 1.0E-7)

(defn- vel-changed?
  ([tr vel] (vel-changed? tr vel vel-threshold))
  ([^Track tr vel ^double threshold]
   (boolean
    (when vel
      (let [sent (or (.vel-sent tr) vel-zero)
            dx (- (vv/x vel) (vv/x sent))
            dy (- (vv/y vel) (vv/y sent))
            dz (- (vv/z vel) (vv/z sent))]
        (> (+ (* dx dx) (* dy dy) (* dz dz)) threshold))))))

(defn- frame ^Frame [e ^Track tr ^long t due?]
  (let [item? (= :item (:type e))
        [bx by bz] (.pos tr)
        p (:pos e)
        x (fixed (vv/x p)) y (fixed (vv/y p)) z (fixed (vv/z p))
        dx (- x (long bx)) dy (- y (long by)) dz (- z (long bz))
        yaw (angle (:yaw e)) pitch (angle (:pitch e))
        head (angle (or (:head-yaw e) (:yaw e)))
        ground (boolean (:on-ground e))
        since (if due? (inc (long (.since-tp tr))) (long (.since-tp tr)))
        vel (:vel e)
        mdata (metadata e)
        equip (equipment-stacks e)
        moved? (boolean (and due?
                             (or (not (zero? dx)) (not (zero? dy)) (not (zero? dz))
                                 (zero? (rem t resync-interval)))))
        turned? (boolean (and due?
                              (or (not= yaw (long (.yaw tr)))
                                  (not= pitch (long (.pitch tr))))))
        rel? (boolean (and (<= (- rel-limit) dx rel-limit)
                           (<= (- rel-limit) dy rel-limit)
                           (<= (- rel-limit) dz rel-limit)
                           (<= since forced-teleport)
                           (not (and item? (not= ground (boolean (.on-ground tr)))))))
        head-turned? (boolean (and due? (not= head (long (.head tr)))))
        meta-changed? (not= mdata (.mdata tr))
        equip-changed? (not= equip (.equip tr))
        equip-diff (when equip-changed?
                     (into []
                           (keep-indexed (fn [i s]
                                           (when (not= s (get (.equip tr) i)) [i s])))
                           equip))
        self? (= :player (:type e))
        slot-diff (when (and self? (not (identical? (:inventory e) (.slots tr))))
                    (slot-diff (or (:inventory e) {}) (.slots tr)))
        carried-changed? (boolean (and self? (not= (as-seen (:carried e)) (as-seen (.carried tr)))))]
    (->Frame x y z dx dy dz yaw pitch head ground since (boolean due?) vel mdata equip
             moved? turned? rel? head-turned? meta-changed? equip-changed?
             (vel-changed? tr vel (if item? item-vel-threshold vel-threshold)) equip-diff
             slot-diff carried-changed?)))

(defn- move-msg [eid e ^Frame f]
  (let [yaw (.yaw f) pitch (.pitch f) ground (.ground f)]
    (cond
      (not (.rel? f))
      (out/sync-pos eid (:pos e) (:yaw e) (:pitch e) ground)
      (and (.moved? f) (.turned? f))
      (out/move-look eid (.dx f) (.dy f) (.dz f) yaw pitch ground)
      (.moved? f)
      (out/move eid (.dx f) (.dy f) (.dz f) ground)
      (.turned? f)
      (out/look eid yaw pitch ground))))

(defn- self-msgs [eid e ^Frame f]
  (cond-> []
    (.meta-changed? f) (conj (out/meta eid (.mdata f)))
    (.vel-changed? f)  (conj (out/velocity eid (.vel f)))
    (seq (.slot-diff f)) (into (map (fn [[slot s]] (out/set-slot slot s))) (.slot-diff f))
    (.carried-changed? f) (conj (out/carried (:carried e)))))

(defn- move-msgs [eid e ^Frame f]
  (cond-> (if-let [m (when (.due? f) (move-msg eid e f))] [m] [])
    (.head-turned? f)     (conj (out/head-look eid (.head f)))
    (.meta-changed? f)    (conj (out/meta eid (.mdata f)))
    (.vel-changed? f)     (conj (out/velocity eid (.vel f)))
    (seq (.equip-diff f)) (into (map (fn [[slot s]] (out/equipment eid slot s)) (.equip-diff f)))))

(def ^:private item-update-interval 20)
(def ^:private mob-update-interval 3)
(defn- advance-track [^Track tr e ^Frame f]
  (let [due? (.due? f) rel? (.rel? f)
        moved? (.moved? f) turned? (.turned? f)]
    (if (and (not due?)
             (not (.head-turned? f)) (not (.meta-changed? f))
             (not (.equip-changed? f)) (not (.vel-changed? f))
             (empty? (.slot-diff f)) (not (.carried-changed? f)))
      tr
      (cond-> (if (or rel? (not due?))
                (cond-> (assoc tr :since-tp (.since f))
                  moved?  (assoc :pos [(.x f) (.y f) (.z f)])
                  turned? (assoc :yaw (.yaw f) :pitch (.pitch f)))
                (assoc tr :pos [(.x f) (.y f) (.z f)] :yaw (.yaw f) :pitch (.pitch f)
                       :on-ground (.ground f) :since-tp 0))
        (.head-turned? f)   (assoc :head (.head f))
        (.meta-changed? f)  (assoc :mdata (.mdata f))
        (.equip-changed? f) (assoc :equip (.equip f))
        (.vel-changed? f)   (assoc :vel-sent (.vel f))
        (seq (.slot-diff f)) (assoc :slots (or (:inventory e) {}))
        (.carried-changed? f) (assoc :carried (:carried e))))))

(defn- move-deltas [t viewers [eid e]]
  (let [vs   (viewers eid)
        self? (= :player (:type e))]
    (when (or (some? vs) self?)
      (let [freq (case (:type e)
                   :item item-update-interval
                   :tnt 10
                   :player update-interval
                   mob-update-interval)
            item? (= :item (:type e))
            due? (or (zero? (rem (long t) (long freq)))
                     (and item? (boolean (:needs-sync? e))))
            tr   (track-of e)]
        (when-not (and (not due?) (instance? Track tr) (identical? e (:seen tr)))
          (if (and (not due?) (instance? Track tr)
                   (= (metadata e) (:mdata tr))
                   (= (equipment-stacks e) (:equip tr))
                   (or (not self?) (and (identical? (:inventory e) (:slots tr)) (= (:carried e) (:carried tr))))
                   (or item? (not (vel-changed? tr (:vel e)))))
            (when-not (identical? e (:seen tr))
              [[:track eid (assoc tr :seen e)]])
            (let [f    (frame e tr (long t) due?)
                  tr'  (advance-track tr e f)
                  msgs (move-msgs eid e f)
                  tr'  (cond
                         (not (identical? tr tr')) (assoc tr' :seen e)
                         (and (empty? msgs) (not due?) (not (identical? e (:seen tr))))
                         (assoc tr :seen e)
                         :else tr')
                  out  (transient [])
                  out  (if (identical? tr tr') out (conj! out [:track eid tr']))
                  out  (if (some? vs)
                         (reduce (fn [out m] (conj! out (out/all m))) out msgs)
                         out)
                  out  (if self?
                         (reduce (fn [out m] (conj! out (out/to eid m))) out (self-msgs eid e f))
                         out)]
              (persistent! out))))))))

(def ^:private tab-header-interval 20)
(defn- fmt ^String [^String pattern v]
  (String/format Locale/ROOT pattern
                 (to-array [(double (or v 0.0))])))

(defn- tab-header-msg [{:keys [tps p50-ms p99-ms]}]
  (out/tab-header "Collider"
                  (str "TPS " (fmt "%.1f" (or tps 20.0))
                       "  tick p50 " (fmt "%.2f" p50-ms)
                       "ms  p99 " (fmt "%.2f" p99-ms) "ms")))

(defn- tab-header-deltas [world events]
  (when-let [perf (:perf world)]
    (let [msg (tab-header-msg perf)]
      (concat
       (when (zero? (rem (long (:tick world)) tab-header-interval))
         [(out/all msg)])
       (for [[tag eid] events :when (= :player-join tag)]
         (out/to eid msg))))))

(def ^:private teleport-retry 20)

(defn- pending-teleport-deltas
  "A teleport the client has not acknowledged in 20 ticks is sent again with
   a fresh id (vanilla awaitingTeleportTime)."
  [world ps]
  (mapcat (fn [[eid e]]
            (let [target (:tp-target e) since (:tp-id e)]
              (when (and target since (>= (- (long (:tick world)) (long since)) teleport-retry))
                [[:teleport eid target]
                 (out/to eid (out/teleport target (:yaw e 0.0) (:pitch e 0.0)))])))
          ps))

(defn- swing-deltas [viewers events]
  (keep (fn [[tag eid]]
          (when (and (= :swing tag) (some? (viewers eid)))
            (out/all (out/animation eid :swing))))
        events))

(defn players [world events]
  (let [ps (state/player-entries world)
        ts (tracked-entries world)]
    (let [spawns (fn []
                   (let [by-chunk (entities-by-chunk ts)]
                     (mapv (fn [entry] #(tracking-deltas world by-chunk entry)) ps)))
          moves  (fn []
                   (let [viewers (viewer-index ps)]
                     (conj (mapv (fn [batch]
                                   #(into [] (mapcat (fn [entry] (move-deltas (long (:tick world)) viewers entry))) batch))
                                 (partition-all 32 ts))
                           #(swing-deltas viewers events))))]
      [#(duplicate-login-deltas world events)
       #(list-deltas world ps)
       #(pending-teleport-deltas world ps)
       spawns
       moves
       #(tab-header-deltas world events)])))
