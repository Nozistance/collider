(ns collider.game.mob.goat
  "Goats, the screaming ones, their horns and the cooldowns they are
  born with."
  (:require [collider.game.mob.brain :as b]
            [collider.random :as random]))

(set! *warn-on-reflection* true)

(def ^:private ^:const scream-chance 0.02)

(def ^:private one-horn-chance (double (float 0.1)))

(defn voice [e] (if (:screaming? e) :screaming :classic))

(defn- roll ^double [ks k] (random/of-key (conj ks k)))

(defn- cooled [e k ks lo hi t]
  (let [n (random/between (roll ks k) lo hi)]
    (b/remember e k n (+ (long t) n))))

(defn- cooling
  "Returns goat e waiting to jump and to ram, for times the keys ks
  draw. Its first thought counts them down at tick t."
  [e ks t]
  (-> (cooled e :long-jump-cooldown-ticks ks 600 1200 t)
      (cooled :ram-cooldown-ticks ks 600 6000 t)))

(defn- screams [e ks]
  (cond-> e (< (roll ks :scream) scream-chance)
    (assoc :screaming? true)))

(defn- dehorned [e ks]
  (if (< (roll ks :one-horn) one-horn-chance)
    (assoc e (if (< (roll ks :horn) 0.5) :left-horn? :right-horn?)
           false)
    e))

(defn spawned
  "Returns goat e first put in the world at tick t. The keys ks
  decide its cooldowns, whether it screams and whether it lost a
  horn. A baby may lose one too, as the roll comes before its age."
  [e ks t]
  (-> (cooling e ks t) (screams ks) (dehorned ks)))

(defn born
  "Returns kid of goats a and b at tick t, by goat eid. It screams
  as one parent picked at random does, or by chance."
  [kid t eid a b]
  (let [ks [t eid :kid]
        p (if (< (roll ks :parent) 0.5) a b)]
    (cond-> (cooling kid ks (inc (long t)))
      (or (:screaming? p) (< (roll ks :scream) scream-chance))
      (assoc :screaming? true))))

(defn metadata
  "Returns what clients see of goat e besides its age and fire."
  [e]
  {:screaming? (boolean (:screaming? e))
   :left-horn? (not (false? (:left-horn? e)))
   :right-horn? (not (false? (:right-horn? e)))
   :pose (or (:pose e) :standing)})
