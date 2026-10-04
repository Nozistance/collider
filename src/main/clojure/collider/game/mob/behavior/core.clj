(ns collider.game.mob.behavior.core
  "The behaviours most brains share: swimming, panic, the look and
  walk sinks, cooldowns, idling, strolling and looking around. A
  position tracker is {:pos p} for a point or {:eid id :eye? b
  :target-eye? b} for an entity, a walk target {:to tracker :speed s
  :close n}."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.brain :as b]
            [collider.game.mob.nav :as nav]
            [collider.game.mob.randompos :as pos]
            [collider.game.mode :as game-mode]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const forever b/forever)

(defn other [w oid] (get (:entities w) oid))

(defn roll
  "Returns the draw for purpose k of behaviour i of mob eid at tick
  t, at least 0 and below 1."
  ^double [t eid i k]
  (random/rnd t eid k i))

(defn sample
  "Returns a whole number from lo to hi drawn for purpose k of
  behaviour i of mob eid at tick t."
  [t eid i k [lo hi]]
  (random/between (roll t eid i k) lo hi))

(defn at-block [cell] {:pos (v/centre cell)})

(defn at-entity
  ([oid eye?] (at-entity oid eye? false))
  ([oid eye? target-eye?]
   {:eid oid :eye? eye? :target-eye? target-eye?}))

(defn walk-target [to speed close]
  {:to to :speed (num/f32 speed) :close close})

(defn- eye-pos [o]
  (let [p (:pos o)]
    [(v/x p) (+ (v/y p) (entity/eye-height o)) (v/z p)]))

(defn tracked-pos
  "Returns the point tracker tr follows in world w."
  [w tr]
  (if-let [o (some->> (:eid tr) (other w))]
    (if (:eye? tr) (eye-pos o) (:pos o))
    (:pos tr)))

(defn tracked-block
  "Returns the cell tracker tr walks to in world w."
  [w tr]
  (let [o (some->> (:eid tr) (other w))]
    (v/cell (cond (nil? o) (:pos tr)
                  (:target-eye? tr) (eye-pos o)
                  :else (:pos o)))))

(defn visible?
  "Returns true when mob e sees entity oid among the living it saw
  last."
  [e oid t]
  (let [m (b/recall e :nearest-visible-living-entities t)]
    (boolean (and m (some #(= oid %) (:near m))
                  (get (:seen m) oid)))))

(defn closest
  "Returns the nearest entity mob e sees for which (pred oid o)."
  [w e t pred]
  (when-let [m (b/recall e :nearest-visible-living-entities t)]
    (some (fn [oid]
            (let [o (other w oid)]
              (when (and o (pred oid o) (get (:seen m) oid)) oid)))
          (:near m))))

(defn- sees? [w e tr t]
  (if-let [oid (:eid tr)]
    (and (entity/alive? (other w oid)) (visible? e oid t))
    true))

(defn cool
  "Returns mob e holding cooldown k of n ticks set at tick t, which
  count-down-cooldown-ticks takes from the next tick on."
  [e k n t]
  (b/remember e k n (+ (long t) (long n) 1)))

(defn look-at [e p t]
  (assoc e :look {:at p :until (+ (long t) 2)}))

(defn swim [chance]
  (let [c (num/f32 chance)
        wet? (fn [w _ e t & _] (animal/afloat? w e t nil))]
    {:id :swim :start? wet? :continue? wet?
     :tick (fn [_ eid e t i]
             (cond-> e
               (< (roll t eid i :jump) c) (assoc :jump true)))}))

(defn- panic-cause? [tag src]
  (contains? (set (data/tag-values "damage_type" tag)) (:type src)))

(defn- panic-cell [w eid e t i]
  (or (when (:burning? e) (animal/look-for-water w e))
      (some-> (pos/land-pos w e t eid [i :panic] 5 4) v/cell)))

(defn- panic-tick [speed]
  (fn [w eid e t i]
    (if-let [c (when (nav/done? e) (panic-cell w eid e t i))]
      (b/remember e :walk-target (walk-target (at-block c) speed 0)
                  forever)
      e)))

(defn animal-panic
  ([speed] (animal-panic speed "panic_causes"))
  ([speed tag]
   {:id :animal-panic :duration [100 120]
    :needs {:is-panicking :registered :hurt-by :registered}
    :start? (fn [_ _ e t]
              (or (some->> (b/recall e :hurt-by t) (panic-cause? tag))
                  (b/present? e :is-panicking t)))
    :continue? (constantly true)
    :start (fn [_ _ e _ _]
             (-> (b/remember e :is-panicking true forever)
                 (b/erase :walk-target)
                 nav/stop))
    :tick (panic-tick speed)
    :stop (fn [_ _ e _ _] (b/erase e :is-panicking))}))

(defn look-at-target-sink [lo hi]
  {:id :look-at-target-sink :duration [lo hi]
   :needs {:look-target :present}
   :continue? (fn [w _ e t _]
                (if-let [tr (b/recall e :look-target t)]
                  (sees? w e tr t)
                  false))
   :tick (fn [w _ e t _]
           (if-let [tr (b/recall e :look-target t)]
             (look-at e (tracked-pos w tr) t)
             e))
   :stop (fn [_ _ e _ _] (b/erase e :look-target))})

(defn count-down-cooldown-ticks
  "Returns the behaviour that keeps cooldown k of cool until it runs
  out and then erases it."
  [k]
  {:id [:count-down k] :duration :never :needs {k :present}
   :continue? (fn [_ _ e t _]
                (if-let [m (get-in e [:brain :memories k])]
                  (< (long t) (long (m 1)))
                  false))
   :stop (fn [_ _ e _ _] (b/erase e k))})

(defn do-nothing [lo hi]
  {:id :do-nothing :duration [lo hi] :continue? (constantly true)})

(defn random-stroll [speed]
  {:id :random-stroll :needs {:walk-target :absent}
   :one-shot
   (fn [w eid e t i]
     (if-let [p (pos/land-pos w e t eid [i :stroll] 10 7)]
       (let [to (at-block (v/cell p))]
         (b/remember e :walk-target (walk-target to speed 0) forever))
       (b/erase e :walk-target)))})

(defn set-walk-target-from-look-target [speed close]
  {:id :set-walk-target-from-look-target
   :needs {:walk-target :absent :look-target :present}
   :one-shot
   (fn [_ _ e t _]
     (let [tr (b/recall e :look-target t)]
       (b/remember e :walk-target (walk-target tr speed close)
                   forever)))})

(defn- look-vec [^double pitch ^double yaw]
  (let [r (num/f32 (/ Math/PI 180.0)) pi (num/f32 Math/PI)
        ya (num/fsub (num/fmul (- yaw) r) pi)
        xa (num/fmul (- pitch) r)
        xc (- (v/cos xa))]
    [(num/fmul (v/sin ya) xc) (v/sin xa) (num/fmul (v/cos ya) xc)]))

(defn- glanced [e ^double r1 ^double r2 [lo hi yaw]]
  (let [pitch (Math/max -90.0 (Math/min 90.0
                (num/f32 (+ (num/fmul r1 (num/fsub hi lo)) lo))))
        swing (num/fmul (num/fmul 2.0 r2) yaw)
        rot (num/wrap-degrees
              (num/fsub (num/f32 (+ (num/f32 (:yaw e)) swing)) yaw))
        [dx dy dz] (look-vec pitch rot)
        [x y z] (eye-pos e)]
    {:pos [(+ x dx) (+ y dy) (+ z dz)]}))

(defn random-look-around [interval max-yaw lo hi]
  (let [ps [(num/f32 lo) (num/f32 hi) (num/f32 max-yaw)]]
    {:id :random-look-around
     :needs {:look-target :absent :gaze-cooldown-ticks :absent}
     :start (fn [_ eid e t i]
              (let [r #(double (float (roll t eid i %)))
                    tr (glanced e (r :pitch) (r :yaw) ps)]
                (cool (b/remember e :look-target tr forever)
                      :gaze-cooldown-ticks
                      (sample t eid i :gaze interval) t)))}))

(defn- manhattan ^long [a b]
  (reduce + (map #(Math/abs (- (long %1) (long %2))) a b)))

(defn- reached? [w e wt]
  (<= (manhattan (tracked-block w (:to wt)) (v/cell (:pos e)))
      (long (:close wt))))

(defn- slotted [e i & kvs]
  (b/with-slot e i (apply assoc (b/slot e i) kvs)))

(def ^:private cant-reach :cant-reach-walk-target-since)

(defn- cant-reach-noted [e t p]
  (cond (:reached? p) (b/erase e cant-reach)
        (b/present? e cant-reach t) e
        :else (b/remember e cant-reach t forever)))

(defn- partial-path [w eid e t i cell]
  (let [c (v/bottom-centre cell) p (:pos e)
        dx (- (v/x c) (v/x p)) dz (- (v/z c) (v/z p))
        to (pos/pos-away w e t eid [i :toward] 10 7 dx dz)]
    (if to
      (let [[e q] (nav/create-path w e (v/cell to) 0)]
        [(some? q) (slotted e i :path q)])
      [false e])))

(defn- path-tried
  "Returns [ok e] with the path of mob e to walk target wt, ok when
  the mob may walk it."
  [w eid e t i wt]
  (let [cell (tracked-block w (:to wt))
        [e p] (nav/create-path w e cell 0)
        e (slotted e i :path p :speed (:speed wt))]
    (cond (reached? w e wt) [false (b/erase e cant-reach)]
          p [true (cant-reach-noted e t p)]
          :else (let [e (cant-reach-noted e t p)]
                  (partial-path w eid e t i cell)))))

(defn- sink-ready [w eid e t i]
  (let [wt (b/recall e :walk-target t)
        r? (reached? w e wt)
        [ok e] (if r? [false e] (path-tried w eid e t i wt))]
    (cond ok [true (slotted e i :last (tracked-block w (:to wt)))]
          r? [false (b/erase (b/erase e :walk-target) cant-reach)]
          :else [false (b/erase e :walk-target)])))

(defn- sink-check [may?]
  (fn [w eid e t i]
    (let [cd (long (:cooldown (b/slot e i) 0))]
      (cond (not (may? e)) [false e]
            (pos? cd) [false (slotted e i :cooldown (dec cd))]
            :else (sink-ready w eid e t i)))))

(defn- sink-start [w _ e _ i]
  (let [s (b/slot e i)
        e (nav/moved-to w e (:path s) (:speed s))
        p (:path (:nav e))]
    (slotted (b/remember e :path p forever) i :path p)))

(defn- spectated? [w wt]
  (boolean (some->> (:eid (:to wt)) (other w) game-mode/spectator?)))

(defn- sink-going? [w _ e t i]
  (let [s (b/slot e i) wt (b/recall e :walk-target t)]
    (boolean (and (:path s) (:last s) wt (not (nav/done? e))
                  (not (reached? w e wt)) (not (spectated? w wt))))))

(defn- sink-tick [w eid e t i]
  (let [p (:path (:nav e)) last (:last (b/slot e i))
        e (if (identical? p (:path (b/slot e i)))
            e
            (slotted (b/remember e :path p forever) i :path p))
        wt (b/recall e :walk-target t)
        to (tracked-block w (:to wt))]
    (if (and p last (> (v/dist-sq to last) 4.0))
      (let [[ok e] (path-tried w eid e t i wt)]
        (if ok (sink-start w eid (slotted e i :last to) t i) e))
      e)))

(defn- sink-stop [w eid e t i]
  (let [wt (b/recall e :walk-target t)
        stuck? (and wt (not (reached? w e wt)) (:stuck? (:nav e)))
        e (cond-> e stuck?
            (slotted i :cooldown
                     (random/below (roll t eid i :cooldown) 40)))]
    (-> (nav/stop e) (b/erase :walk-target) (b/erase :path)
        (slotted i :path nil))))

(defn move-to-target-sink
  "Returns the behaviour that walks the walk target. A mob for which
  may? is false does not start it."
  ([] (move-to-target-sink (constantly true)))
  ([may?] (move-to-target-sink may? 150 250))
  ([may? lo hi]
   {:id :move-to-target-sink :duration [lo hi]
    :needs {cant-reach :registered :path :absent
            :walk-target :present}
    :check (sink-check may?) :start sink-start
    :continue? sink-going? :tick sink-tick :stop sink-stop}))
