(ns collider.game.effect
  "Mob effects on living entities.
  An entity keeps its effects under :effects, by effect name. An
  instance holds :duration in ticks (-1 is infinite), :amplifier,
  :ambient?, :visible? and :icon?, and under :hidden the weaker one
  that comes back when it runs out."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(def ^:const infinite -1)

(def ^:private ^:const refresh-period 600)

(defn instance
  "Returns a new effect instance.
  The amplifier is clamped to 0..255."
  ([duration amplifier] (instance duration amplifier false true))
  ([duration amplifier ambient? visible?]
   (instance duration amplifier ambient? visible? visible?))
  ([duration amplifier ambient? visible? icon?]
   {:duration (long duration)
    :amplifier (max 0 (min 255 (long amplifier)))
    :ambient? (boolean ambient?) :visible? (boolean visible?)
    :icon? (boolean icon?) :hidden nil}))

(defn endless? [i] (= infinite (long (:duration i))))

(defn- shorter? [a b]
  (and (not (endless? a))
       (or (< (long (:duration a)) (long (:duration b)))
           (endless? b))))

(defn- details [i] (assoc i :hidden nil))

(defn- stronger [cur t]
  (cond-> (assoc cur :amplifier (:amplifier t)
                 :duration (:duration t))
    (shorter? t cur) (assoc :hidden cur)))

(declare merged)

(defn- longer [cur t]
  (cond
    (= (:amplifier t) (:amplifier cur))
    [(assoc cur :duration (:duration t)) true]
    (nil? (:hidden cur)) [(assoc cur :hidden (details t)) false]
    :else [(assoc cur :hidden (first (merged (:hidden cur) t)))
           false]))

(defn- flags [[cur changed?] t]
  (let [amb? (or changed? (and (not (:ambient? t)) (:ambient? cur)))
        cur (cond-> cur amb? (assoc :ambient? (:ambient? t)))
        vis? (not= (:visible? t) (:visible? cur))
        icon? (not= (:icon? t) (:icon? cur))]
    [(assoc cur :visible? (:visible? t) :icon? (:icon? t))
     (boolean (or changed? amb? vis? icon?))]))

(defn merged
  "Returns [instance changed?] after instance t lands on cur.
  This is MobEffectInstance.update."
  [cur t]
  (flags (cond
           (> (long (:amplifier t)) (long (:amplifier cur)))
           [(stronger cur t) true]
           (shorter? cur t) (longer cur t)
           :else [cur false])
         t))

(defn- map-duration [^long d f]
  (if (or (= infinite d) (zero? d)) d (f d)))

(defn- ticked-down [i]
  (cond-> (update i :duration map-duration dec)
    (:hidden i) (update :hidden ticked-down)))

(defn- downgraded [i]
  (if (and (zero? (long (:duration i))) (:hidden i))
    [(:hidden i) true]
    [i false]))

(defn remaining? [i]
  (or (endless? i) (pos? (long (:duration i)))))

(def ^:private kinds
  {:regeneration [:periodic 50] :poison [:periodic 25]
   :wither [:periodic 40] :instant-health [:instant]
   :instant-damage [:instant] :saturation [:instant]
   :hunger [:always] :absorption [:always]})

(defn- shift-int ^long [^long a ^long n]
  (unchecked-int (bit-shift-left a (bit-and n 31))))

(defn due?
  "Returns true when effect k with amplifier a acts at tick count c.
  An infinite effect counts the ticks of its entity, the others
  their remaining duration."
  [k ^long c ^long a]
  (let [[kind ^long base] (kinds k)]
    (case kind
      :periodic (let [n (bit-shift-right base (bit-and a 31))]
                  (or (<= n 0) (zero? (rem c n))))
      :instant (>= c 1)
      :always true
      false)))

(defn heal-amount
  "Returns what instant health heals at amplifier a."
  ^double [^long a]
  (double (max (shift-int 4 a) 0)))

(defn harm-amount
  "Returns what instant damage hurts at amplifier a."
  ^double [^long a]
  (double (float (shift-int 6 a))))

(defn stepped
  "Returns [instance events] after one tick that did not stop it.
  The events are :refresh when a hidden instance took its place,
  :gone when it ran out and :updated when the client needs its
  duration again. This is the rest of MobEffectInstance.tickServer
  after the effect acted, and the end of LivingEntity.tickEffects."
  [i]
  (let [[i down?] (downgraded (ticked-down i))
        d (long (:duration i))]
    [i (cond-> (if down? [:refresh] [])
         (not (remaining? i)) (conj :gone)
         (and (remaining? i) (zero? (rem d refresh-period)))
         (conj :updated))]))

(defn added
  "Returns [effects what] after instance t of effect k lands.
  What is :added, :updated or nil when nothing changed. This is
  LivingEntity.addEffect for an entity the effect can affect."
  [effects k t]
  (if-let [cur (get effects k)]
    (let [[i changed?] (merged cur t)]
      [(assoc effects k i) (when changed? :updated)])
    [(assoc (or effects {}) k t) :added]))

(defn- id-of ^long [k] (data/registry-id "mob_effect" k))

(defn in-order
  "Returns the [k instance] pairs of effects in registry order."
  [effects]
  (sort-by (comp id-of key) effects))

(defn- argb ^long [^long alpha ^long rgb]
  (bit-or (bit-shift-left alpha 24) (bit-and rgb 0xFFFFFF)))

(def ^:private ^:const ambient-alpha 38)

(def ^:private own-particles
  {:trial-omen [:trial-omen] :raid-omen [:raid-omen]})

(defn particle
  "Returns the particle effect k shows at instance i.
  It is [type argb] for the tinted entity effect particle."
  [k i]
  (or (own-particles k)
      (let [rgb (long (:color (get (data/mob-effects) k) 0))]
        [:entity-effect
         (argb (if (:ambient? i) ambient-alpha 255) rgb)])))

(defn particles
  "Returns the particles of the visible effects, in order."
  [effects]
  (into [] (keep (fn [[k i]] (when (:visible? i) (particle k i))))
        (in-order effects)))

(defn all-ambient?
  "Returns true when no visible effect is not ambient."
  [effects]
  (not-any? (fn [[_ i]] (and (:visible? i) (not (:ambient? i))))
            effects))

(defn instant?
  "Returns true when effect k acts once and is gone."
  [k]
  (boolean (:instant? (get (data/mob-effects) k))))
