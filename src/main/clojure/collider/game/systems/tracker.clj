(ns collider.game.systems.tracker
  "What each player sees of the bodies near it."
  (:require [clojure.core.reducers :as r]
            [collider.data.long-map :as lm]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.entity.metadata :as metadata]
            [collider.game.entity.sections :as sections]
            [collider.game.hanging :as hanging]
            [collider.game.level :as level]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.systems.tracker.track :refer [->Track]]
            [collider.vec :as vv])
  (:import (collider.game.deltas.record Deltas)
           (collider.game.systems.tracker.track Frame Track)))

(set! *warn-on-reflection* true)

(def update-interval 2)

(def resync-interval 60)

(def forced-teleport 400)

(def ^:private tracked-types
  (into #{:player :item :tnt :falling-block :area-effect-cloud
          :experience-orb}
        cat [entity/thrown-types hanging/types (keys mobs/types)]))

(defn- tracked? [e]
  (contains? tracked-types (:type e)))

(def ^:private vel-zero (vv/v3 0.0 0.0 0.0))

(def ^:const ^:private pos-unit 4096.0)

(def ^:const ^:private rel-limit 32767)

(defn- fixed
  ^long [v] (Math/round (* (double v) pos-unit)))

(defn- angle
  ^long [v] (long (Math/floor (* (double v) (/ 256.0 360.0)))))

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
  "Defines fast readers of record type t, named with prefix."
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
             (or (:kept-mdata e) (metadata/of e))
             (equipment-stacks e)
             (:vel e)
             0
             (when (= :player (:type e)) (or (:inventory e) {}))
             (:carried e)
             t)))

(defn- as-seen [s]
  (when s
    [(:item s) (long (:count s 1)) (:components s) (:removed s)]))

(defn- slot-diff [inv known]
  (let [pick (fn [slot]
               (let [ours (get inv slot) theirs (get known slot)]
                 (when (not= (as-seen ours) (as-seen theirs))
                   [slot ours])))
        slots (into (sorted-set) (concat (keys inv) (keys known)))]
    (into [] (keep pick) slots)))

(defn- track-of [^long t e] (or (:track e) (baseline t e)))

(defn- viewed [ps]
  (transduce (keep #(:tracking (val %))) (completing lm/union)
             (lm/long-set) ps))

(defn- baseline-deltas [world t0 pid eid]
  (let [e (get-in world [:entities eid])]
    (when-not (:track e)
      (let [mdata (metadata/of e)
            ground (boolean (:on-ground e))
            base [[:track eid (baseline t0 e)]
                  (out/to pid (out/move eid 0 0 0 ground))]]
        (if (seq mdata)
          (conj base (out/to pid (out/meta eid (:type e) mdata)))
          base)))))

(defn- hidden-from [oid o bodies]
  (into (lm/long-set [oid])
        (keep (fn [[eid e]] (when-not (game-mode/shown-to? o e) eid)))
        bodies))

(defn- untracked [world]
  (reduce-kv (fn [acc t ids]
               (if (contains? tracked-types t)
                 acc
                 (lm/union acc ids)))
             (lm/long-set) (level/types-by world)))

(defn- by-chunk [world]
  (let [out (untracked world)
        keep? (if (lm/empty? out) any? #(not (lm/contains? out %)))]
    (sections/chunked (sections/index world) keep?)))

(defn- in-view [idx seen]
  (if (< (count idx) (count seen))
    (reduce-kv (fn [acc c es]
                 (if (lm/contains? seen c) (lm/union acc es) acc))
               (lm/long-set) idx)
    (reduce (fn [acc c]
              (if-let [es (lm/get idx c)] (lm/union acc es) acc))
            (lm/long-set) seen)))

(defn- tracking-deltas [world t0 idx ps [oid o]]
  (let [near (in-view idx (or (:sent-chunks o) (lm/long-set)))
        hidden (hidden-from oid o ps)
        want (lm/difference near hidden)
        have (or (:tracking o) (lm/long-set))
        add (into [] (lm/difference want have))
        gone (into [] (lm/difference have want))]
    (when (or (seq add) (seq gone))
      (into [[:tracking oid add gone]]
            (mapcat (fn [eid] (baseline-deltas world t0 oid eid)))
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

(def ^:private no-delta-types
  #{:player :llama-spit :wither :bat :item-frame :glow-item-frame
    :leash-knot :painting :end-crystal :evoker-fangs})

(defn- vel-due? [e self? due?]
  (or self?
      (and due?
           (or (:needs-sync? e)
               (not (contains? no-delta-types (:type e)))))))

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
        meta-changed? (not= mdata (tr-mdata tr))
        equip-changed? (not= equip (tr-equip tr))
        self? (= :player (:type e))
        carried? (carried-differs? e tr self?)
        rel? (boolean
               (and (in-rel-range? dx dy dz)
                    (<= since forced-teleport)
                    (= ground (boolean (tr-on-ground tr)))))
        mdiff (when meta-changed? (meta-diff mdata (tr-mdata tr)))]
    (new Frame x y z dx dy dz yaw pitch head ground since
         (boolean due?) vel mdata mdiff equip
         (moved-now? e tr due? ticks)
         (turned-now? tr due? yaw pitch)
         rel?
         (boolean (and due? (not= head (long (tr-head tr)))))
         meta-changed?
         equip-changed?
         (and (vel-due? e self? due?) (vel-changed? tr vel))
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
  (let [md (if (f-first? f) (f-mdata f) (f-mdiff f))
        m (when (f-due? f) (move-msg eid e f))]
    (cond-> []
            (and (f-due? f) (f-vel-changed? f))
            (conj (out/velocity eid (f-vel f)))
            m (conj m)
            (f-head-turned? f) (conj (out/head-look eid (f-head f)))
            (or (f-meta-changed? f) (f-first? f))
            (conj (out/meta eid (:type e) md))
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

(def ^:private still-update-interval Integer/MAX_VALUE)

(def ^:private update-freqs
  (merge {:item              item-update-interval
          :experience-orb    item-update-interval
          :tnt               10
          :falling-block     20
          :area-effect-cloud still-update-interval
          :player            update-interval}
         (zipmap entity/thrown-types (repeat 10))
         (zipmap hanging/types (repeat still-update-interval))))

(defn- update-freq ^long [e]
  (long (get update-freqs (:type e) mob-update-interval)))

(def ^:private ^:const frame-flush-interval 10)

(defn- flushed? [e ^long ticks]
  (and (contains? hanging/frames (:type e))
       (zero? (rem ticks frame-flush-interval))))

(defn- due-now? [^long t tr e dirty?]
  (let [ticks (- t (long (:t0 tr)))]
    (or (zero? (rem ticks (update-freq e)))
        (boolean (:needs-sync? e))
        (and (boolean dirty?) (not (flushed? e ticks))))))

(defn- quiet? [tr e self? dirty? equip-same?]
  (and (not dirty?)
       equip-same?
       (or (not self?)
           (and (identical? (:inventory e) (:slots tr))
                (= (:carried e) (:carried tr))
                (not (vel-changed? tr (:vel e)))))))

(defn- collect-out [eid vs self? track-delta msgs selfs]
  (let [out (transient [])
        out (if track-delta (conj! out track-delta) out)
        out (if vs
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
    (collect-out eid vs self?
                 (when-not (identical? tr tr') [:track eid tr'])
                 msgs (self-msgs eid e f))))

(defn- tracked-deltas [t eid e vs self? tr mdata dirty? due?]
  (let [fresh? (and (not due?) (instance? Track tr))
        same? (and fresh? (= (equipment-stacks e) (:equip tr)))]
    (when-not (and fresh? (quiet? tr e self? dirty? same?))
      (changed-deltas t eid e vs self? tr mdata due?))))

(defn- marked-deltas
  [eid e vs]
  (cond-> [[:merge-entity eid {:hurt-marked? nil}]]
    vs (conj (out/all (out/velocity eid (:vel e))))))

(defn- seen-deltas [t vs self? eid e]
  (let [tr (track-of (long t) e)
        mdata (metadata/of e)
        dirty? (not= mdata (:mdata tr))
        due? (due-now? (long t) tr e dirty?)]
    (tracked-deltas (long t) eid e vs self? tr mdata dirty? due?)))

(defn- move-deltas [t viewers eid e]
  (let [vs (contains? viewers eid)
        self? (= :player (:type e))
        ds (when (or vs self?) (seen-deltas t vs self? eid e))]
    (if (:hurt-marked? e)
      (into (vec ds) (marked-deltas eid e vs))
      ds)))

(defn- resend-deltas [world]
  (for [{:keys [eid pos yaw pitch]} (get-in world [:input :resends])]
    (out/to eid (out/teleport pos yaw pitch))))

(defn- entities-changed? [d]
  (some (fn [delta]
          (let [tag (nth delta 0)]
            (or (identical? :spawn-entity tag)
                (identical? :remove-entity tag)
                (identical? :change-dimension tag))))
        (:world d)))

(defn late-tracking
  "Returns the deltas that show players the entities new this tick."
  {:wake {:deltas #{:spawn-entity :remove-entity
                    :change-dimension}}}
  [world d]
  (deltas/of-vec
    (when (entities-changed? d)
      (let [idx (by-chunk world)
            ps (level/player-entries world)
            t0 (inc (long (:tick world)))
            track #(tracking-deltas world t0 idx ps %)]
        (into [] (mapcat track) ps)))))

(defn- spawn-deltas [world ps]
  (let [idx (by-chunk world)
        t0 (:tick world)]
    (deltas/fold #(tracking-deltas world t0 idx ps %) ps)))

(def ^:private ^:const move-batch 32)

(defn- collected [c]
  (if (instance? Deltas c) c (deltas/collected c)))

(defn- joined
  ([] (deltas/collecting))
  ([a b] (deltas/merge (collected a) (collected b))))

(defn- moves-deltas [world ps]
  (let [viewers (viewed ps)
        t (long (:tick world))
        step (fn [c eid e]
               (if (tracked? e)
                 (deltas/collect-all! c (move-deltas t viewers eid e))
                 c))]
    (collected (r/fold move-batch joined step (:entities world)))))

(defn tracker
  {:wake {:keys [:entities [:input :resends]]}}
  [world _]
  (let [ps (level/player-entries world)]
    (deltas/merge (deltas/of-vec (resend-deltas world))
                  (spawn-deltas world ps)
                  (moves-deltas world ps))))
