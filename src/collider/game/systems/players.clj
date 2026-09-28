(ns collider.game.systems.players
  "Player list, entity tracking and movement updates."
  (:require [clojure.data.int-map :as i]
            [collider.game.entity :as entity]
            [collider.game.effect :as effect]
            [collider.game.game-mode :as game-mode]
            [collider.vec :as vv]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.sleep :as sleep]
            [collider.game.systems.players.track
             :refer [->Frame ->Track]]
            [collider.world.chunk :as chunk])
  (:import (collider.game.systems.players.track Frame Track)
           (java.util ArrayList Locale)))

(set! *warn-on-reflection* true)

(def update-interval 2)

(def resync-interval 60)

(def latency-interval 600)

(def forced-teleport 400)

(def ^:private vel-zero (vv/v3 0.0 0.0 0.0))

(def ^:const ^:private pos-unit 4096.0)

(def ^:const ^:private rel-limit 32767)

(defn- fixed
  ^long [v] (Math/round (* (double v) pos-unit)))

(defn- angle
  ^long [v] (long (Math/floor (* (double v) (/ 256.0 360.0)))))

(defn- thrown-metadata [e] {:stack (:stack e)})

(defn- orb-metadata [e]
  (cond-> {:value (:value e)} (:burning? e) (assoc :burning? true)))

(defn- item-metadata [e]
  (cond-> {:stack (:stack e)}
          (:burning? e) (assoc :burning? true)))

(def ^:private simple-metadata
  (merge
    {:item          item-metadata
     :experience-orb orb-metadata
     :tnt           (fn [e] {:fuse (:fuse e)})
     :falling-block (fn [e] {:start (:start e)})
     :area-effect-cloud
     (fn [e] {:radius (:radius e) :color (:color e)
              :waiting? (boolean (:waiting? e))})}
    (zipmap entity/thrown-types (repeat thrown-metadata))))

(defn- using-hand [e]
  (if (:using-item? e) (or (get-in e [:using :hand]) :main) false))

(defn- player-metadata [e]
  (cond-> {:burning?    (boolean (:burning? e))
           :sneaking?   (boolean (:sneaking? e))
           :sprinting?  (boolean (:sprinting? e))
           :using-item? (using-hand e)
           :swimming?   (boolean (:swimming? e))
           :pose        (or (:pose e) :standing)
           :skin-parts  (long (or (:skin-parts e) 0))}
          (:sleeping e)
          (assoc :sleeping-pos (get-in e [:sleeping :pos]))
          (pos? (double (:absorption e 0.0)))
          (assoc :absorption (:absorption e))
          (not (zero? (long (:score e 0))))
          (assoc :score (:score e))
          (game-mode/spectator? e) (assoc :invisible? true)))

(def ^:private flag-keys
  [:burning? :sneaking? :sprinting? :swimming? :invisible? :glowing?
   :color :sheared? :variant])

(defn- meta-diff [mdata sent]
  (let [ks (into #{} (concat (keys mdata) (keys sent)))
        pick (fn [k]
               (let [v (get mdata k)]
                 (when (not= v (get sent k)) [k v])))
        changed (into {} (keep pick) ks)]
    (if (some #(contains? changed %) flag-keys)
      (into changed (select-keys mdata flag-keys))
      changed)))

(defn- effect-particles [e fx]
  (when-not (game-mode/spectator? e)
    (not-empty (effect/particles fx))))

(defn- effect-metadata [e fx]
  (let [ps (effect-particles e fx)]
    (cond-> {}
      ps (assoc :effect-particles ps)
      (effect/all-ambient? fx) (assoc :effect-ambience true)
      (contains? fx :invisibility) (assoc :invisible? true)
      (contains? fx :glowing) (assoc :glowing? true))))

(defn- own-metadata [e]
  (if-let [f (simple-metadata (:type e))]
    (f e)
    (if (mobs/mob-type? (:type e))
      (mobs/metadata e)
      (player-metadata e))))

(defn metadata
  "Returns the synched fields an entity would show a client."
  [e]
  (let [m (own-metadata e) fx (:effects e)]
    (cond (seq fx) (merge m (effect-metadata e fx))
          (:ambience e) (assoc m :effect-ambience true)
          :else m)))

(defn- held-stack [e]
  (get-in e [:inventory (+ 36 (long (or (:held-slot e) 0)))]))

(def ^:private no-equip [nil nil nil nil nil])

(defn- equipment-stacks [e]
  (let [inv (:inventory e)]
    (if (nil? inv)
      no-equip
      [(held-stack e) (get inv 8) (get inv 7)
       (get inv 6) (get inv 5)])))

(defmacro ^:private readers
  "Defines a prefixed reader for each field of record type t."
  [t prefix & fields]
  `(do ~@(for [f fields
               :let [dot (symbol (str "." f))]]
           `(defn- ~(symbol (str prefix f))
              {:inline
               (fn [~'r] (list '~dot (with-meta ~'r {:tag '~t})))}
              [~(with-meta 'r {:tag t})]
              (~dot ~'r)))))

(readers Track tr- pos yaw pitch head on-ground mdata equip vel-sent
         since-tp slots carried t0)

(defn- baseline [^long t {:keys [pos yaw pitch on-ground] :as e}]
  (let [[x y z] pos]
    (->Track [(double x) (double y) (double z)]
             (angle yaw) (angle pitch) (angle (or (:head-yaw e) yaw))
             (boolean on-ground)
             (or (:kept-mdata e) (metadata e))
             (equipment-stacks e)
             (:vel e)
             0
             (when (= :player (:type e)) (or (:inventory e) {}))
             (:carried e)
             t)))

(defn- as-seen [s] (when s [(:item s) (long (:count s 1))]))

(defn- slot-diff [inv known]
  (let [pick (fn [slot]
               (let [ours (get inv slot) theirs (get known slot)]
                 (when (not= (as-seen ours) (as-seen theirs))
                   [slot ours])))
        slots (into (sorted-set) (concat (keys inv) (keys known)))]
    (into [] (keep pick) slots)))

(defn- track-of [^long t e] (or (:track e) (baseline t e)))

(def ^:private tracked-types
  #{:player :item :tnt :falling-block :area-effect-cloud
    :experience-orb})

(defn- tracked? [e]
  (let [t (:type e)]
    (or (tracked-types t)
        (entity/thrown-types t)
        (mobs/mob-type? t))))

(defn- tracked-entries [world]
  (into [] (filter (fn [[_ e]] (tracked? e))) (:entities world)))

(defn- add-viewer [a eid oid]
  (if-let [^ArrayList l (get a eid)]
    (do (.add l oid) a)
    (assoc! a eid (doto (ArrayList. 4) (.add oid)))))

(defn- viewer-index [ps]
  (persistent!
    (reduce (fn [acc [oid o]]
              (reduce (fn [a eid] (add-viewer a eid oid))
                      acc
                      (seq (:tracking o))))
            (transient (i/int-map))
            ps)))

(def ^:private duplicate-login-reason
  "You logged in from another location")

(defn- kicked-deltas [world eid]
  (concat
    (sleep/vacated-deltas world (get-in world [:entities eid]))
    [(out/to eid (out/disconnect duplicate-login-reason))
     (out/to eid (out/close))
     [:remove-entity eid]]))

(defn- other-logins [world pname]
  (let [owner (get-in world [:players pname])]
    (for [[eid e] (:entities world)
          :when (and (= :player (:type e))
                     (= pname (:name e))
                     (not= eid owner))]
      eid)))

(defn- duplicate-login-deltas [world events]
  (mapcat (fn [[tag _ pname]]
            (when (= :player-join tag)
              (mapcat #(kicked-deltas world %)
                      (other-logins world pname))))
          events))

(defn- joined-deltas [events]
  (for [[tag eid] events :when (= :player-join tag)]
    (out/to eid (out/joined))))

(defn- add-entry [e]
  {:uuid (:uuid e) :name (:name e) :ping (or (:ping e) 0)
   :game-mode (:game-mode e)})

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

(defn- uuids-of [ps]
  (into {} (map (fn [[eid e]] [eid (:uuid e)])) ps))

(defn- latency-deltas [world all]
  (when (zero? (rem (long (:tick world)) latency-interval))
    [(out/all (out/tab-latency (all)))]))

(defn- listed-all? [listed ps]
  (and (= (count listed) (count ps))
       (every? (fn [[eid _]] (contains? listed eid)) ps)))

(defn- list-changes [world ps]
  (let [listed (:listed world)
        cur (uuids-of ps)
        joined (remove (fn [[eid _]] (contains? listed eid)) ps)
        left (sort (remove cur (keys listed)))
        all (mapv (comp add-entry val) ps)]
    (concat
      (join-list-deltas joined all)
      (leave-list-deltas world (into #{} (map val) cur) left)
      (latency-deltas world (constantly all))
      (when (or (seq joined) (seq left))
        [[:listed (uuids-of joined) left]]))))

(defn- list-deltas [world ps]
  (let [listed (:listed world)]
    (if (listed-all? listed ps)
      (latency-deltas world #(mapv (comp add-entry val) ps))
      (list-changes world ps))))

(defn- baseline-deltas [world pid eid]
  (let [e (get-in world [:entities eid])]
    (when-not (:track e)
      (let [mdata (metadata e)
            ground (boolean (:on-ground e))
            base [[:track eid (baseline (long (:tick world)) e)]
                  (out/to pid (out/move eid 0 0 0 ground))]]
        (if (seq mdata)
          (conj base (out/to pid (out/meta eid (:type e) mdata)))
          base)))))

(defn- entities-by-chunk [ts]
  (persistent!
    (reduce (fn [m [eid e]]
              (let [c (chunk/pos-chunk (:pos e))]
                (assoc! m c (conj (get m c []) eid))))
            (transient (i/int-map))
            ts)))

(defn- near-eids [by-chunk seen]
  (if (< (count by-chunk) (count seen))
    (eduction (comp (filter #(contains? seen (key %))) (mapcat val))
              by-chunk)
    (eduction (mapcat #(get by-chunk %)) (seq seen))))

(defn- tracking-deltas [world by-chunk [oid o]]
  (let [seen (or (:sent-chunks o) (i/int-set))
        mine? (fn [eid] (= (long eid) (long oid)))
        shown? #(game-mode/shown-to? o (get-in world [:entities %]))
        want (into (i/int-set) (comp (remove mine?) (filter shown?))
                   (near-eids by-chunk seen))
        have (or (:tracking o) (i/int-set))
        add (into [] (remove #(contains? have %)) (seq want))
        gone (into [] (remove #(contains? want %)) (seq have))]
    (when (or (seq add) (seq gone))
      (into [[:tracking oid add gone]]
            (mapcat (fn [eid] (baseline-deltas world oid eid)))
            add))))

(readers Frame f- x y z dx dy dz yaw pitch head ground since due? vel
         mdata mdiff equip moved? turned? rel? head-turned?
         meta-changed? equip-changed? vel-changed? equip-diff
         slot-diff carried-changed? first?)

(def ^:private vel-threshold 1.0E-7)

(def ^:private pos-threshold 7.6293945E-6)

(defn- still? [vel]
  (zero? (+ (* (vv/x vel) (vv/x vel))
            (* (vv/y vel) (vv/y vel))
            (* (vv/z vel) (vv/z vel)))))

(defn- vel-changed? [^Track tr vel]
  (boolean
    (when vel
      (let [sent (or (tr-vel-sent tr) vel-zero)
            dx (- (vv/x vel) (vv/x sent))
            dy (- (vv/y vel) (vv/y sent))
            dz (- (vv/z vel) (vv/z sent))
            d (+ (* dx dx) (* dy dy) (* dz dz))]
        (or (> d vel-threshold)
            (and (> d 0.0) (still? vel)))))))

(defn- near-baseline? [e ^Track tr]
  (let [[bx by bz] (tr-pos tr)
        p (:pos e)
        ex (- (vv/x p) (double bx))
        ey (- (vv/y p) (double by))
        ez (- (vv/z p) (double bz))]
    (< (+ (* ex ex) (* ey ey) (* ez ez)) pos-threshold)))

(defn- moved-now? [e ^Track tr due? ^long ticks]
  (boolean (and due?
                (or (not (near-baseline? e tr))
                    (zero? (rem ticks resync-interval))))))

(defn- turned-now? [^Track tr due? ^long yaw ^long pitch]
  (boolean (and due?
                (or (not= yaw (long (tr-yaw tr)))
                    (not= pitch (long (tr-pitch tr)))))))

(defn- in-rel-range? [^long dx ^long dy ^long dz]
  (and (<= (- rel-limit) dx rel-limit)
       (<= (- rel-limit) dy rel-limit)
       (<= (- rel-limit) dz rel-limit)))

(defn- equip-changes [equip ^Track tr]
  (into []
        (keep-indexed
          (fn [i s] (when (not= s (get (tr-equip tr) i)) [i s])))
        equip))

(defn- carried-differs? [e ^Track tr self?]
  (boolean (and self?
                (not= (as-seen (:carried e))
                      (as-seen (tr-carried tr))))))

(defn- self-slot-diff [e ^Track tr self?]
  (when (and self? (not (identical? (:inventory e) (tr-slots tr))))
    (slot-diff (or (:inventory e) {}) (tr-slots tr))))

(defn- frame ^Frame [e ^Track tr t due? mdata]
  (let [[bx by bz] (tr-pos tr)
        p (:pos e)
        x (vv/x p) y (vv/y p) z (vv/z p)
        dx (- (fixed x) (fixed bx))
        dy (- (fixed y) (fixed by))
        dz (- (fixed z) (fixed bz))
        ticks (- (long t) (long (tr-t0 tr)))
        yaw (angle (:yaw e)) pitch (angle (:pitch e))
        head (angle (or (:head-yaw e) (:yaw e)))
        ground (boolean (:on-ground e))
        since (if due?
                (inc (long (tr-since-tp tr)))
                (long (tr-since-tp tr)))
        vel (:vel e)
        equip (equipment-stacks e)
        equip-changed? (not= equip (tr-equip tr))
        self? (= :player (:type e))
        carried? (carried-differs? e tr self?)
        rel? (boolean
               (and (in-rel-range? dx dy dz)
                    (<= since forced-teleport)
                    (= ground (boolean (tr-on-ground tr)))))]
    (->Frame x y z dx dy dz yaw pitch head ground since (boolean due?)
             vel mdata (meta-diff mdata (tr-mdata tr)) equip
             (moved-now? e tr due? ticks)
             (turned-now? tr due? yaw pitch)
             rel?
             (boolean (and due? (not= head (long (tr-head tr)))))
             (not= mdata (tr-mdata tr))
             equip-changed?
             (vel-changed? tr vel)
             (when equip-changed? (equip-changes equip tr))
             (self-slot-diff e tr self?)
             carried?
             (zero? ticks))))

(defn- move-msg [eid e ^Frame f]
  (let [yaw (f-yaw f) pitch (f-pitch f) ground (f-ground f)]
    (cond
      (not (f-rel? f))
      (out/sync-pos eid (:pos e) (:yaw e) (:pitch e) ground)
      (and (f-moved? f) (f-turned? f))
      (out/move-look eid (f-dx f) (f-dy f) (f-dz f) yaw pitch ground)
      (f-moved? f)
      (out/move eid (f-dx f) (f-dy f) (f-dz f) ground)
      (f-turned? f)
      (out/look eid yaw pitch ground))))

(defn- slot-msg [[slot s]] (out/set-slot slot s))

(defn- self-msgs [eid e ^Frame f]
  (cond-> []
          (f-meta-changed? f)
          (conj (out/meta eid (:type e) (f-mdiff f)))
          (and (f-vel-changed? f) (not (:needs-sync? e)))
          (conj (out/velocity eid (f-vel f)))
          (seq (f-slot-diff f)) (into (map slot-msg) (f-slot-diff f))
          (f-carried-changed? f) (conj (out/carried (:carried e)))))

(defn- move-msgs [eid e ^Frame f]
  (let [md (if (f-first? f) (f-mdata f) (f-mdiff f))]
    (cond-> (if-let [m (when (f-due? f) (move-msg eid e f))] [m] [])
            (f-head-turned? f) (conj (out/head-look eid (f-head f)))
            (or (f-meta-changed? f) (f-first? f))
            (conj (out/meta eid (:type e) md))
            (and (f-due? f) (f-vel-changed? f))
            (conj (out/velocity eid (f-vel f)))
            (seq (f-equip-diff f))
            (into (map (fn [[slot s]] (out/equipment eid slot s))
                       (f-equip-diff f))))))

(def ^:private item-update-interval 20)

(def ^:private mob-update-interval 3)

(defn- track-idle? [^Frame f]
  (and (not (f-due? f))
       (not (f-head-turned? f)) (not (f-meta-changed? f))
       (not (f-equip-changed? f)) (not (f-vel-changed? f))
       (empty? (f-slot-diff f)) (not (f-carried-changed? f))))

(defn- advance-pos [^Track tr ^Frame f]
  (if (or (f-rel? f) (not (f-due? f)))
    (cond-> (assoc tr :since-tp (f-since f))
            (f-moved? f) (assoc :pos [(f-x f) (f-y f) (f-z f)])
            (f-turned? f) (assoc :yaw (f-yaw f) :pitch (f-pitch f)))
    (assoc tr :pos [(f-x f) (f-y f) (f-z f)] :yaw (f-yaw f)
           :pitch (f-pitch f) :on-ground (f-ground f) :since-tp 0)))

(defn- advance-track [^Track tr e ^Frame f]
  (if (track-idle? f)
    tr
    (cond-> (advance-pos tr f)
            (f-head-turned? f) (assoc :head (f-head f))
            (f-meta-changed? f) (assoc :mdata (f-mdata f))
            (f-equip-changed? f) (assoc :equip (f-equip f))
            (f-vel-changed? f) (assoc :vel-sent (f-vel f))
            (seq (f-slot-diff f))
            (assoc :slots (or (:inventory e) {}))
            (f-carried-changed? f) (assoc :carried (:carried e)))))

(def ^:private cloud-update-interval Integer/MAX_VALUE)

(def ^:private update-freqs
  (merge {:item              item-update-interval
          :experience-orb    item-update-interval
          :tnt               10
          :falling-block     20
          :area-effect-cloud cloud-update-interval
          :player            update-interval}
         (zipmap entity/thrown-types (repeat 10))))

(defn- update-freq ^long [e]
  (long (get update-freqs (:type e) mob-update-interval)))

(defn- due-now? [^long t tr e dirty?]
  (or (zero? (rem (- t (long (:t0 tr))) (update-freq e)))
      (boolean (:needs-sync? e))
      (boolean dirty?)))

(defn- quiet? [tr e self? item? dirty?]
  (and (not dirty?)
       (= (equipment-stacks e) (:equip tr))
       (or (not self?)
           (and (identical? (:inventory e) (:slots tr))
                (= (:carried e) (:carried tr))))
       (or item? (not (vel-changed? tr (:vel e))))))

(defn- collect-out [eid e vs self? track-delta msgs selfs]
  (let [out (transient [])
        out (if track-delta (conj! out track-delta) out)
        out (if (some? vs)
              (reduce (fn [out m] (conj! out (out/all m))) out msgs)
              out)
        out (if self?
              (reduce (fn [out m] (conj! out (out/to eid m)))
                      out selfs)
              out)]
    (persistent! out)))

(defn- changed-deltas [t eid e vs self? tr mdata due?]
  (let [f (frame e tr (long t) due? mdata)
        tr' (advance-track tr e f)
        msgs (move-msgs eid e f)]
    (collect-out eid e vs self?
                 (when-not (identical? tr tr') [:track eid tr'])
                 msgs (self-msgs eid e f))))

(defn- move-deltas [t viewers [eid e]]
  (let [vs (viewers eid)
        self? (= :player (:type e))]
    (when (or (some? vs) self?)
      (let [item? (contains? #{:item :experience-orb} (:type e))
            tr (track-of (long t) e)
            mdata (metadata e)
            dirty? (not= mdata (:mdata tr))
            due? (due-now? (long t) tr e dirty?)
            fresh? (and (not due?) (instance? Track tr))]
        (when-not (and fresh? (quiet? tr e self? item? dirty?))
          (changed-deltas (long t) eid e vs self? tr mdata due?))))))

(def ^:private tab-header-interval 20)

(defn- fmt ^String [^String pattern v]
  (String/format Locale/ROOT pattern
                 (to-array [(double (or v 0.0))])))

(defn- server-title [commit]
  (if commit
    (str "Collider Server (" commit ")")
    "Collider Server"))

(defn- tab-header-msg [commit {:keys [tps mspt p50-ms p99-ms]}]
  (out/tab-header (server-title commit)
                  (str "TPS " (fmt "%.1f" (or tps 20.0))
                       "\nMSPT " (fmt "%.2f" mspt) " ms"
                       "\np50 " (fmt "%.2f" p50-ms) " ms"
                       "\np99 " (fmt "%.2f" p99-ms) " ms")))

(defn- tab-header-deltas [world events]
  (when-let [perf (:perf world)]
    (let [commit (get-in world [:config :commit])
          msg (tab-header-msg commit perf)]
      (concat
        (when (zero? (rem (long (:tick world)) tab-header-interval))
          [(out/all msg)])
        (for [[tag eid] events :when (= :player-join tag)]
          (out/to eid msg))))))

(defn- resend-deltas [world]
  (for [{:keys [eid pos yaw pitch]} (:resends world)]
    (out/to eid (out/teleport pos yaw pitch))))

(defn swing-deltas
  "Returns the deltas of one swing a client plays itself.
  The server only passes it on, and only as often as an arm
  can swing."
  [world [tag eid hand]]
  (when (= :swing tag)
    (when-let [p (get-in world [:entities eid])]
      (let [t (:tick world)]
        (vec (state/swing-deltas eid p (or hand :main) t false))))))

(defn- entities-changed? [d]
  (some (fn [delta]
          (let [tag (nth delta 0)]
            (or (identical? :spawn-entity tag)
                (identical? :remove-entity tag)
                (identical? :change-dimension tag))))
        (:world d)))

(defn late-tracking
  "Returns the deltas that show players the entities that appeared
  during this tick."
  [world d]
  (when (entities-changed? d)
    (let [by-chunk (entities-by-chunk (tracked-entries world))
          job (fn [entry] (tracking-deltas world by-chunk entry))]
      (into [] (mapcat job) (state/player-entries world)))))

(defn- spawn-jobs [world ps ts]
  (let [by-chunk (entities-by-chunk ts)]
    (mapv (fn [entry] #(tracking-deltas world by-chunk entry)) ps)))

(def ^:private ^:const move-batch 32)

(defn- move-jobs [world ps ts]
  (let [viewers (viewer-index ps)
        t (long (:tick world))
        job (fn [entry] (move-deltas t viewers entry))
        batch-job (fn [batch] #(into [] (mapcat job) batch))]
    (mapv batch-job (partition-all move-batch ts))))

(defn- inline [jobs] (into [] (mapcat #(%)) jobs))

(defn player-list
  "Returns the tick steps of the player list of the server."
  [world d]
  (let [ps (state/player-entries world)
        joins (state/joins d)]
    (concat (joined-deltas joins)
            (duplicate-login-deltas world joins)
            (list-deltas world ps)
            (tab-header-deltas world joins))))

(defn players
  "Returns the tick steps of entity tracking in the level."
  [world _]
  (let [ps (state/player-entries world)
        ts (tracked-entries world)]
    (if (<= (count ts) move-batch)
      (concat (resend-deltas world)
              (inline (spawn-jobs world ps ts))
              (inline (move-jobs world ps ts)))
      [#(resend-deltas world)
       #(spawn-jobs world ps ts)
       #(move-jobs world ps ts)])))
