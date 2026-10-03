(ns collider.game.attribute
  "Attributes of living entities as their effects change them."
  (:require [collider.data :as data]
            [collider.game.effect :as effect]
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
  {:movement-speed [0.0 1024.0] :attack-speed [0.0 1024.0]
   :attack-damage [0.0 2048.0] :safe-fall-distance [-1024.0 1024.0]
   :waypoint-transmit-range [0.0 6.0E7] :max-health [1.0 1024.0]
   :max-absorption [0.0 2048.0] :luck [-1024.0 1024.0]})

(def synced
  "The attributes a client is told about."
  #{:movement-speed :attack-speed :safe-fall-distance :max-health
    :max-absorption :luck})

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

(defn modifiers
  "Returns the modifiers of attribute attr of entity e under effects.
  A sprinting player adds the sprint boost to its movement speed."
  [e effects attr]
  (into (vec (sprint-modifiers e attr))
        (for [[k i] (effect/in-order effects)
              [a id amount op] (templates k)
              :when (= a attr)]
          [id (* (double amount) (inc (long (:amplifier i)))) op])))

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
  "Returns the value of attribute attr of entity e under effects,
  with the modifiers more added. It stays in the range of attr."
  (^double [e effects attr] (value e effects attr nil))
  (^double [e effects attr more]
   (let [ms (into (modifiers e effects attr) more)
         base (+ (double (get (base-values e) attr 0.0))
                 (summed ms add-value))
         v (+ base (* base (summed ms add-base-share)))]
     (clamped attr (reduce multiplied v ms)))))

(defn entries
  "Returns the attribute entries [attr base modifiers] of the synced
  attributes in attrs that entity e has, for the client."
  [e effects attrs]
  (let [bs (base-values e)]
    (vec (for [a (sort attrs)
               :when (and (synced a) (contains? bs a))]
           [a (double (bs a)) (modifiers e effects a)]))))
