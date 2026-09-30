(ns collider.game.attribute
  "Attributes of living entities as their effects change them.
  A modifier is [id amount operation], the operation 0 to add, 1 to
  add a share of the base and 2 to multiply the total."
  (:require [collider.data :as data]
            [collider.game.effect :as effect]))

(set! *warn-on-reflection* true)

(defn- f ^double [^double v] (double (float v)))

(def ^:private templates
  {:speed [[:movement-speed :effect.speed (f 0.2) 2]]
   :slowness [[:movement-speed :effect.slowness (f -0.15) 2]]
   :haste [[:attack-speed :effect.haste (f 0.1) 2]]
   :mining-fatigue
   [[:attack-speed :effect.mining-fatigue (f -0.1) 2]]
   :strength [[:attack-damage :effect.strength 3.0 0]]
   :jump-boost [[:safe-fall-distance :effect.jump-boost 1.0 0]]
   :invisibility
   [[:waypoint-transmit-range
     :effect.waypoint-transmit-range-hide -1.0 2]]
   :weakness [[:attack-damage :effect.weakness -4.0 0]]
   :health-boost [[:max-health :effect.health-boost 4.0 0]]
   :absorption [[:max-absorption :effect.absorption 4.0 0]]
   :luck [[:luck :effect.luck 1.0 0]]
   :unluck [[:luck :effect.unluck -1.0 0]]})

(def ^:private ranges
  {:movement-speed [0.0 1024.0] :attack-speed [0.0 1024.0]
   :attack-damage [0.0 2048.0] :safe-fall-distance [-1024.0 1024.0]
   :waypoint-transmit-range [0.0 6.0E7] :max-health [1.0 1024.0]
   :max-absorption [0.0 2048.0] :luck [-1024.0 1024.0]})

(def synced
  "The attributes a client is told about."
  #{:movement-speed :attack-speed :safe-fall-distance :max-health
    :max-absorption :luck})

(defn base-values
  "Returns the base of every attribute that entity e has, as
  DefaultAttributes gives them for its kind."
  [e]
  (get (data/attributes) (:type e)))

(defn effect-attributes
  "Returns the attributes effect k changes."
  [k]
  (into #{} (map first) (templates k)))

(def ^:private ^:const sprint-boost 0.3)

(defn- sprint-modifiers [e attr]
  (when (and (= :movement-speed attr) (= :player (:type e))
             (:sprinting? e))
    [[:sprinting (f sprint-boost) 2]]))

(defn modifiers
  "Returns the modifiers of attribute attr of entity e."
  [e effects attr]
  (into (vec (sprint-modifiers e attr))
        (for [[k i] (effect/in-order effects)
              [a id amount op] (templates k)
              :when (= a attr)]
          [id (* (double amount) (inc (long (:amplifier i)))) op])))

(defn- summed ^double [ms op]
  (reduce (fn [^double s [_ a o]] (if (= op o) (+ s (double a)) s))
          0.0 ms))

(defn- clamped ^double [attr ^double v]
  (let [[lo hi] (ranges attr)]
    (if (Double/isNaN v)
      (double lo)
      (max (double lo) (min (double hi) v)))))

(defn value
  "Returns the value of attribute attr of entity e with effects.
  This is AttributeInstance.calculateValue."
  ^double [e effects attr]
  (let [ms (modifiers e effects attr)
        base (+ (double (get (base-values e) attr 0.0)) (summed ms 0))
        v (+ base (* base (summed ms 1)))
        v (reduce (fn [^double v [_ a o]]
                    (if (= 2 o) (* v (+ 1.0 (double a))) v))
                  v ms)]
    (clamped attr v)))

(defn entries
  "Returns the attribute entries [attr base modifiers] of the synced
  attributes in attrs that entity e has, for the client."
  [e effects attrs]
  (let [bs (base-values e)]
    (vec (for [a (sort attrs)
               :when (and (synced a) (contains? bs a))]
           [a (double (bs a)) (modifiers e effects a)]))))
