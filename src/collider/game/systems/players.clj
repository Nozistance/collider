(ns collider.game.systems.players
  (:require [clojure.data.int-map :as i]
            [collider.vec :as vv]
            [collider.game.mobs :as mobs]
            [collider.game.state :as state]
            [collider.proto.packets.play :as play]))

(set! *warn-on-reflection* true)

(def update-interval 2)
(def resync-interval 60)
(def latency-interval 600)
(def forced-teleport 400)
(def ^:private vel-zero (vv/v3 0.0 0.0 0.0))
(defn- fixed ^long [v] (long (Math/floor (* (double v) 32.0))))
(defn- angle ^long [v] (long (Math/floor (* (double v) (/ 256.0 360.0)))))
(defn metadata [e]
  (cond
    (= :item (:type e))
    [[10 :slot (:stack e)]]
    (= :tnt (:type e))
    []
    (mobs/mob-type? (:type e))
    (mobs/metadata e)
    :else
    [[0 :byte (bit-or (if (:burning? e) 0x01 0)
                      (if (:sneaking? e) 0x02 0)
                      (if (:sprinting? e) 0x08 0)
                      (if (:using-item? e) 0x10 0))]
     [10 :byte (long (or (:skin-parts e) 0))]]))

(defn- held-stack [e]
  (get-in e [:inventory (+ 36 (long (or (:held-slot e) 0)))]))

(def ^:private no-equip [nil nil nil nil nil])
(defn- equipment-stacks [e]
  (let [inv (:inventory e)]
    (if (nil? inv)
      no-equip
      [(held-stack e) (get inv 8) (get inv 7) (get inv 6) (get inv 5)])))

(defrecord Track [pos yaw pitch head on-ground mdata equip vel-sent since-tp seen])
(defn- baseline [{:keys [pos yaw pitch on-ground] :as e}]
  (let [[x y z] pos]
    (->Track [(fixed x) (fixed y) (fixed z)]
             (angle yaw) (angle pitch) (angle (or (:head-yaw e) yaw))
             (boolean on-ground)
             (metadata e)
             (equipment-stacks e)
             (:vel e)
             0
             e)))

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
                       (if-let [^java.util.ArrayList l (get a eid)]
                         (do (.add l oid) a)
                         (assoc! a eid (doto (java.util.ArrayList. 4) (.add oid)))))
                     acc
                     (seq (:tracking o))))
           (transient (i/int-map))
           ps)))

(defn- entity-chunk ^long [e]
  (state/pos-chunk (:pos e)))

(def ^:private duplicate-login-reason
  "{\"text\":\"You logged in from another location\"}")

(defn- duplicate-login-deltas [world events]
  (mapcat (fn [[tag _ pname]]
            (when (= :player-join tag)
              (let [owner (get-in world [:players pname])]
                (for [[eid e] (:entities world)
                      :when (and (= :player (:type e))
                                 (= pname (:name e))
                                 (not= eid owner))
                      d [[:send eid {:packet/key ::play/disconnect
                                     :reason duplicate-login-reason}]
                         [:close eid]
                         [:remove-entity eid]]]
                  d))))
          events))

(defn- add-entry [e]
  {:uuid (:uuid e) :name (:name e) :gamemode 1 :ping (or (:ping e) 0)})

(defn- list-packet [action entries]
  {:packet/key ::play/player-list-item :action action :entries entries})

(defn- join-list-deltas [world joined all]
  (mapcat (fn [[eid e]]
            (cons [:send eid (list-packet play/list-add all)]
                  (state/broadcast world eid (list-packet play/list-add [(add-entry e)]))))
          joined))

(defn- leave-list-deltas [world live left]
  (let [listed (:listed world)]
    (mapcat (fn [eid]
              (when-not (contains? live (get listed eid))
                (state/broadcast world (list-packet play/list-remove
                                                    [{:uuid (get listed eid)}]))))
            left)))

(defn- list-deltas [world ps]
  (let [listed (:listed world)
        cur    (into {} (map (fn [[eid e]] [eid (:uuid e)])) ps)
        joined (remove (fn [[eid _]] (contains? listed eid)) ps)
        left   (sort (remove cur (keys listed)))
        all    (mapv (comp add-entry val) ps)]
    (concat
     (join-list-deltas world joined all)
     (leave-list-deltas world (into #{} (map val) cur) left)
     (when (zero? (rem (long (:tick world)) latency-interval))
       (state/broadcast world (list-packet play/list-latency all)))
     (when (or (seq joined) (seq left))
       [[:tab-list (into {} (map (fn [[eid e]] [eid (:uuid e)])) joined) left]]))))

(defn- spawn-packet [eid e]
  (let [{:keys [pos yaw pitch]} (track-of e)
        [x y z] pos]
    {:packet/key ::play/spawn-player
     :entity-id eid :uuid (:uuid e)
     :x x :y y :z z :yaw yaw :pitch pitch
     :current-item 0
     :metadata (metadata e)}))

(defn- spawn-deltas [world oid eid]
  (let [e  (get-in world [:entities eid])
        tr (track-of e)]
    (concat
     (when-not (:track e) [[:track eid tr]])
     (cond
       (= :item (:type e))
       [[:send oid (play/spawn-item eid (:pos tr) (:vel e))]
        [:send oid {:packet/key ::play/entity-metadata
                    :entity-id eid :metadata (:mdata tr)}]]
       (= :tnt (:type e))
       [[:send oid (play/spawn-tnt eid (:pos tr))]
        [:send oid (play/entity-velocity eid (:vel e))]]
       (mobs/mob-type? (:type e))
       [[:send oid (play/spawn-mob eid (mobs/net-id (:type e))
                                   (:pos tr) (:yaw tr) (:pitch tr) (:mdata tr))]]
       :else
       (cons [:send oid (spawn-packet eid e)]
             (keep-indexed (fn [slot s]
                             (when s [:send oid (play/equipment eid slot s)]))
                           (:equip tr)))))))

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
      (concat
       [[:tracking oid add gone]]
       (mapcat (fn [eid] (spawn-deltas world oid eid)) add)
       (when (seq gone)
         [[:send oid {:packet/key ::play/destroy-entities :entity-ids gone}]])))))

(defrecord Frame [x y z dx dy dz yaw pitch head ground since due? vel mdata equip
                  moved? turned? rel? head-turned? meta-changed? equip-changed?
                  vel-changed? equip-diff])

(defn- vel-changed? [^Track tr vel]
  (boolean
   (when vel
     (let [sent (or (.vel-sent tr) vel-zero)
           dx (- (vv/x vel) (vv/x sent))
           dy (- (vv/y vel) (vv/y sent))
           dz (- (vv/z vel) (vv/z sent))]
       (> (+ (* dx dx) (* dy dy) (* dz dz)) 4.0E-4)))))

(defn- frame ^Frame [e ^Track tr ^long t due?]
  (let [[bx by bz] (.pos tr)
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
                             (or (>= (abs dx) 4) (>= (abs dy) 4) (>= (abs dz) 4)
                                 (zero? (rem t resync-interval)))))
        turned? (boolean (and due?
                              (or (>= (abs (- yaw (long (.yaw tr)))) 4)
                                  (>= (abs (- pitch (long (.pitch tr)))) 4))))
        rel? (boolean (and (<= -128 dx 127) (<= -128 dy 127) (<= -128 dz 127)
                           (<= since forced-teleport)
                           (= ground (.on-ground tr))))
        head-turned? (boolean (and due? (>= (abs (- head (long (.head tr)))) 4)))
        meta-changed? (not= mdata (.mdata tr))
        equip-changed? (not= equip (.equip tr))
        equip-diff (when equip-changed?
                     (into []
                           (keep-indexed (fn [i s]
                                           (when (not= s (get (.equip tr) i)) [i s])))
                           equip))]
    (->Frame x y z dx dy dz yaw pitch head ground since (boolean due?) vel mdata equip
             moved? turned? rel? head-turned? meta-changed? equip-changed?
             (vel-changed? tr vel) equip-diff)))

(defn- move-packet [eid ^Frame f]
  (let [x (.x f) y (.y f) z (.z f) yaw (.yaw f) pitch (.pitch f) ground (.ground f)]
    (cond
      (not (.rel? f))
      {:packet/key ::play/entity-teleport :entity-id eid
       :x x :y y :z z :yaw yaw :pitch pitch :on-ground ground}
      (and (.moved? f) (.turned? f))
      {:packet/key ::play/entity-look-move :entity-id eid
       :dx (.dx f) :dy (.dy f) :dz (.dz f) :yaw yaw :pitch pitch :on-ground ground}
      (.moved? f)
      {:packet/key ::play/entity-rel-move :entity-id eid
       :dx (.dx f) :dy (.dy f) :dz (.dz f) :on-ground ground}
      (.turned? f)
      {:packet/key ::play/entity-look :entity-id eid
       :yaw yaw :pitch pitch :on-ground ground})))

(defn- self-packets [eid ^Frame f]
  (cond-> []
    (.meta-changed? f) (conj {:packet/key ::play/entity-metadata
                              :entity-id eid :metadata (.mdata f)})
    (.vel-changed? f)  (conj (play/entity-velocity eid (.vel f)))))

(defn- move-packets [eid ^Frame f]
  (cond-> (if-let [p (when (.due? f) (move-packet eid f))] [p] [])
    (.head-turned? f)     (conj {:packet/key ::play/entity-head-look :entity-id eid :yaw (.head f)})
    (.meta-changed? f)    (conj {:packet/key ::play/entity-metadata :entity-id eid :metadata (.mdata f)})
    (.vel-changed? f)     (conj (play/entity-velocity eid (.vel f)))
    (seq (.equip-diff f)) (into (map (fn [[slot s]] (play/equipment eid slot s)) (.equip-diff f)))))

(def ^:private item-update-interval 20)
(def ^:private mob-update-interval 3)
(defn- advance-track [^Track tr ^Frame f]
  (let [due? (.due? f) rel? (.rel? f)
        moved? (.moved? f) turned? (.turned? f)]
    (if (and (not due?)
             (not (.head-turned? f)) (not (.meta-changed? f))
             (not (.equip-changed? f)) (not (.vel-changed? f)))
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
        (.vel-changed? f)   (assoc :vel-sent (.vel f))))))

(defn- move-deltas [world t viewers [eid e]]
  (let [vs   (viewers eid)
        self? (= :player (:type e))]
    (when (or (pos? (count vs)) self?)
      (let [freq (case (:type e)
                   :item item-update-interval
                   :tnt 10
                   :player update-interval
                   mob-update-interval)
            due? (zero? (rem (long t) (long freq)))
            tr   (track-of e)]
        (when-not (and (not due?) (instance? Track tr) (identical? e (:seen tr)))
          (if (and (not due?) (instance? Track tr)
                   (identical? (metadata e) (:mdata tr))
                   (identical? (equipment-stacks e) (:equip tr))
                   (not (vel-changed? tr (:vel e))))
            (when-not (identical? e (:seen tr))
              [[:track eid (assoc tr :seen e)]])
          (let [f    (frame e tr (long t) due?)
                tr'  (advance-track tr f)
                pkts (move-packets eid f)
                tr'  (cond
                       (not (identical? tr tr')) (assoc tr' :seen e)
                       (and (empty? pkts) (not due?) (not (identical? e (:seen tr))))
                       (assoc tr :seen e)
                       :else tr')
                out  (transient [])
                out  (if (identical? tr tr') out (conj! out [:track eid tr']))
                out  (reduce (fn [out oid] (reduce (fn [out p] (conj! out [:send oid p])) out pkts))
                             out vs)
                out  (if self?
                       (reduce (fn [out p] (conj! out [:send eid p])) out (self-packets eid f))
                       out)]
            (persistent! out))))))))

(def ^:private tab-header-interval 20)
(defn- fmt ^String [^String pattern v]
  (String/format java.util.Locale/ROOT pattern
                 (to-array [(double (or v 0.0))])))

(defn- tab-header-packet [{:keys [tps p50-ms p99-ms]}]
  {:packet/key ::play/tab-header
   :header "{\"text\":\"Collider\"}"
   :footer (str "{\"text\":\"TPS " (fmt "%.1f" (or tps 20.0))
                "  tick p50 " (fmt "%.2f" p50-ms)
                "ms  p99 " (fmt "%.2f" p99-ms) "ms\",\"color\":\"gray\"}")})

(defn- tab-header-deltas [world events]
  (when-let [perf (:perf world)]
    (let [pkt (tab-header-packet perf)]
      (concat
       (when (zero? (rem (long (:tick world)) tab-header-interval))
         (state/broadcast world pkt))
       (for [[tag eid] events :when (= :player-join tag)]
         [:send eid pkt])))))

(defn- swing-deltas [viewers events]
  (mapcat (fn [[tag eid]]
            (when (= :swing tag)
              (for [oid (viewers eid)]
                [:send oid {:packet/key ::play/animation :entity-id eid :animation 0}])))
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
                                   #(into [] (mapcat (fn [entry] (move-deltas world (long (:tick world)) viewers entry))) batch))
                                 (partition-all 32 ts))
                           #(swing-deltas viewers events))))]
      [#(duplicate-login-deltas world events)
       #(list-deltas world ps)
       spawns
       moves
       #(tab-header-deltas world events)])))
