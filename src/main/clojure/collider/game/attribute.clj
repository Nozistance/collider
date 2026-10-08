(ns collider.game.attribute
  "Attributes of living entities as their effects and equipment
  change them."
  (:require [collider.data :as data]
            [collider.game.effect :as effect]
            [collider.game.enchantment :as enchantment]
            [collider.game.stack :as stack]
            [collider.num :as num]))

(set! *warn-on-reflection* true)

(def ^:private ^:const add-value 0)

(def ^:private ^:const add-base-share 1)

(def ^:private ^:const multiply-total 2)

(def ^:private templates
  {:speed
   [[:movement-speed :effect.speed (num/f32 0.2) multiply-total]]
   :slowness
   [[:movement-speed :effect.slowness (num/f32 -0.15) multiply-total]]
   :haste
   [[:attack-speed :effect.haste (num/f32 0.1) multiply-total]]
   :mining-fatigue
   [[:attack-speed :effect.mining-fatigue (num/f32 -0.1)
     multiply-total]]
   :strength [[:attack-damage :effect.strength 3.0 add-value]]
   :jump-boost
   [[:safe-fall-distance :effect.jump-boost 1.0 add-value]]
   :invisibility
   [[:waypoint-transmit-range
     :effect.waypoint-transmit-range-hide -1.0 multiply-total]]
   :weakness [[:attack-damage :effect.weakness -4.0 add-value]]
   :health-boost [[:max-health :effect.health-boost 4.0 add-value]]
   :absorption [[:max-absorption :effect.absorption 4.0 add-value]]
   :luck [[:luck :effect.luck 1.0 add-value]]
   :unluck [[:luck :effect.unluck -1.0 add-value]]})

(def ^:private ranges
  {:air-drag-modifier [0.0 2048.0] :armor [0.0 30.0]
   :armor-toughness [0.0 20.0] :attack-damage [0.0 2048.0]
   :attack-knockback [0.0 5.0] :attack-speed [0.0 1024.0]
   :below-name-distance [0.0 512.0] :block-break-speed [0.0 1024.0]
   :block-interaction-range [0.0 64.0] :bounciness [0.0 1.0]
   :burning-time [0.0 1024.0] :camera-distance [0.0 32.0]
   :explosion-knockback-resistance [0.0 1.0]
   :entity-interaction-range [0.0 64.0]
   :fall-damage-multiplier [0.0 100.0] :flying-speed [0.0 1024.0]
   :follow-range [0.0 2048.0] :friction-modifier [0.0 2048.0]
   :gravity [-1.0 1.0] :jump-strength [0.0 32.0]
   :knockback-resistance [-2.0 1.0] :luck [-1024.0 1024.0]
   :max-absorption [0.0 2048.0] :max-health [1.0 1024.0]
   :mining-efficiency [0.0 1024.0] :movement-efficiency [0.0 1.0]
   :movement-speed [0.0 1024.0] :name-tag-distance [0.0 512.0]
   :oxygen-bonus [0.0 1024.0] :safe-fall-distance [-1024.0 1024.0]
   :scale [0.0625 16.0] :sneaking-speed [0.0 1.0]
   :spawn-reinforcements [0.0 1.0] :step-height [0.0 10.0]
   :submerged-mining-speed [0.0 20.0]
   :sweeping-damage-ratio [0.0 1.0]
   :tempt-range [0.0 2048.0] :water-movement-efficiency [0.0 1.0]
   :waypoint-transmit-range [0.0 6.0E7]
   :waypoint-receive-range [0.0 6.0E7]})

(def synced
  "The attributes a client is told about."
  (disj (set (keys ranges)) :attack-damage :attack-knockback
        :follow-range :knockback-resistance :spawn-reinforcements
        :tempt-range :waypoint-transmit-range
        :waypoint-receive-range))

(defn base-values
  [e]
  (get (data/attributes) (:type e)))

(defn effect-attributes
  [k]
  (into #{} (map first) (templates k)))

(def ^:private ^:const sprint-boost 0.3)

(defn- sprint-modifiers [e attr]
  (when (and (= :movement-speed attr) (= :player (:type e))
             (:sprinting? e))
    [[:sprinting (num/f32 sprint-boost) multiply-total]]))

(def ^:private creative-reach
  {:block-interaction-range [:creative-mode-block-range 0.5 add-value]
   :entity-interaction-range
   [:creative-mode-entity-range 2.0 add-value]})

(defn- frost-modifiers [e attr]
  (when (and (= :movement-speed attr) (:frost e))
    [[:powder-snow (double (:frost e)) add-value]]))

(defn- mode-modifiers [e attr]
  (when (= :creative (:game-mode e))
    (some-> (creative-reach attr) vector)))

(defn- taken-over?
  "Returns true when the modifier id that an effect puts on attribute
  a is gone. Equipment either put its own modifier under that id
  after the effect, or took the id off."
  [e a id]
  (or (contains? (get-in e [:equipment-modifiers a]) id)
      (contains? (get-in e [:lost-modifiers a]) id)))

(defn- effect-modifiers [e effects attr]
  (for [[k i] (effect/in-order effects)
        [a id amount op] (templates k)
        :when (and (= a attr) (not (taken-over? e a id)))]
    [id (* (double amount) (inc (long (:amplifier i)))) op]))

(defn modifiers
  "Returns the modifiers of attribute attr of entity e under effects.
  A sprinting player adds the sprint boost to its movement speed, a
  creative one the creative reach."
  [e effects attr]
  (-> (vec (sprint-modifiers e attr))
      (into (frost-modifiers e attr))
      (into (mode-modifiers e attr))
      (into (effect-modifiers e effects attr))
      (into (vals (get (:equipment-modifiers e) attr)))))

(defn- unheld [e k a id]
  (if (contains? (get-in e [k a]) id)
    (update-in e [k a] (if (= k :lost-modifiers) disj dissoc) id)
    e))

(defn reclaimed
  "Returns entity e once effect k puts or takes off its modifiers.
  Each id of k goes from equipment first, as removeModifier does."
  [e k]
  (reduce (fn [e [a id]]
            (-> (unheld e :equipment-modifiers a id)
                (unheld :lost-modifiers a id)))
          e (templates k)))

(def ^:private equipment-slots
  [:mainhand :offhand :feet :legs :chest :head :body :saddle])

(def ^:private slot-groups
  {:any (set equipment-slots) :hand #{:mainhand :offhand}
   :armor #{:feet :legs :chest :head :body}})

(def ^:private operations
  {:add-value add-value :add-multiplied-base add-base-share
   :add-multiplied-total multiply-total})

(defn- in-group? [group slot]
  (contains? (get slot-groups group #{group}) slot))

(defn- broken? [s]
  (and (stack/damageable? s)
       (>= (stack/damage s) (stack/max-damage s))))

(defn- enchantment-modifiers [slot s]
  (for [[k level] (stack/component s :enchantments)
        :let [en (enchantment/info k)]
        :when (some #(in-group? % slot) (:slots en))
        {:keys [attribute id amount operation]} (:attributes en)]
    [attribute [(str id "/" (name slot)) (double (amount level))
                (operations operation)]]))

(defn- stack-modifiers
  "Returns the [attr modifier] pairs that stack s gives in slot.
  Its own modifiers come before those of its enchantments."
  [slot s]
  (concat
    (for [{a :attribute m :modifier g :slot}
          (stack/component s :attribute-modifiers)
          :when (in-group? (or g :any) slot)]
      [a [(:id m) (double (:amount m)) (operations (:operation m))]])
    (enchantment-modifiers slot s)))

(defn- seen [s]
  (when s
    [(:item s) (long (:count s 1)) (not-empty (:components s))
     (not-empty (:removed s))]))

(defn- effect-holds? [e a id]
  (some (fn [k] (some (fn [[a' id']] (and (= a a') (= id id')))
                      (templates k)))
        (keys (:effects e))))

(defn- dropped
  "Returns [e ts] once the modifier id of attribute a goes, whoever
  put it. An effect that put it loses it until it puts it anew."
  [[e ts] [a [id]]]
  (let [own? (contains? (get-in e [:equipment-modifiers a]) id)
        fx? (effect-holds? e a id)
        shown? (or own? (and fx? (not (taken-over? e a id))))]
    [(cond-> e
       own? (update-in [:equipment-modifiers a] dissoc id)
       fx? (update-in [:lost-modifiers a] (fnil conj #{}) id))
     (cond-> ts shown? (conj a))]))

(defn- taken [has?]
  (fn [[e ts :as acc] [a [id :as m]]]
    (if (has? a)
      [(assoc-in e [:equipment-modifiers a id] m) (conj ts a)]
      acc)))

(defn- old-modifiers [e slot]
  (when-let [s (get (:last-equipment e) slot)]
    (stack-modifiers slot s)))

(defn- new-modifiers [equipment slot]
  (when-let [s (get equipment slot)]
    (when-not (broken? s) (stack-modifiers slot s))))

(defn- changed-slots [e equipment]
  (let [last (:last-equipment e)]
    (filterv #(not= (seen (get last %)) (seen (get equipment %)))
             equipment-slots)))

(defn- moved [e equipment slots]
  (let [acc [(update e :equipment-modifiers #(or % {})) #{}]
        acc (reduce dropped acc (mapcat #(old-modifiers e %) slots))]
    (reduce (taken (base-values e)) acc
            (mapcat #(new-modifiers equipment %) slots))))

(defn equipped
  "Returns [e attrs] with entity e wearing the modifiers of the
  stacks in equipment, by slot, and the attributes that changed.
  The old stacks of all changed slots lose their modifiers by id
  before the new stacks add their own, so the last changed slot
  wins. A broken stack adds none."
  [e equipment]
  (let [slots (changed-slots e equipment)
        [e' ts] (moved e equipment slots)]
    (if (empty? slots)
      [e #{}]
      [(assoc e' :last-equipment
              (select-keys equipment equipment-slots))
       ts])))

(defn- summed ^double [ms op]
  (reduce (fn [^double s [_ a o]] (if (= op o) (+ s (double a)) s))
          0.0 ms))

(defn- multiplied ^double [^double v [_ a o]]
  (if (= multiply-total o) (* v (+ 1.0 (double a))) v))

(defn- clamped ^double [attr ^double v]
  (let [[lo hi] (ranges attr)]
    (if (Double/isNaN v)
      (double lo)
      (max (double lo) (min (double hi) v)))))

(defn value
  "Returns the value of attribute attr of entity e under effects.
  It stays in the range of attr."
  ^double [e effects attr]
  (let [ms (modifiers e effects attr)
        base (+ (double (get (base-values e) attr 0.0))
                (summed ms add-value))
        v (+ base (* base (summed ms add-base-share)))]
    (clamped attr (reduce multiplied v ms))))

(defn entries
  "Returns the attribute entries [attr base modifiers] of the synced
  attributes in attrs that entity e has, for the client."
  [e effects attrs]
  (let [bs (base-values e)]
    (vec (for [a (sort attrs)
               :when (and (synced a) (contains? bs a))]
           [a (double (bs a)) (modifiers e effects a)]))))
