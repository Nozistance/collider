(ns collider.game.food
  "The food of a player, its hunger bar and the saturation and
  exhaustion under it, and the natural healing they pay for."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.mode :as game-mode]
            [collider.num :as num]))

(set! *warn-on-reflection* true)

(def ^:const full 20)

(def fresh
  "The food of a new player."
  {:food full :saturation 5.0 :exhaustion 0.0 :food-timer 0})

(defn level ^long [e] (long (:food e full)))

(defn saturation ^double [e] (double (:saturation e 5.0)))

(defn- exhaustion ^double [e] (double (:exhaustion e 0.0)))

(defn can-eat?
  "Returns true when player e may eat food, always? if it is edible
  even on a full bar."
  [e always?]
  (boolean
    (or (game-mode/invulnerable? e) always? (< (level e) full))))

(defn refuses?
  "Returns true when player e may not start eating item."
  [e item]
  (when-let [f (get-in (data/items) [item :food])]
    (not (can-eat? e (:always? f)))))

(defn eaten
  "Returns the food of player e after it ate nutrition points with
  saturation."
  [e ^long nutrition ^double saturation-gain]
  (let [f (min full (max 0 (+ (level e) nutrition)))
        s (num/f32 (+ saturation-gain (saturation e)))]
    {:food f :saturation (min (double f) (max 0.0 s))}))

(defn exhausted
  "Returns player e with amount more exhaustion, at most 40."
  [e ^double amount]
  (let [x (num/f32 (+ (exhaustion e) amount))]
    (assoc e :exhaustion (min 40.0 x))))

(defn- health ^double [e] (double (:health e 0.0)))

(defn- top ^double [e]
  (num/f32 (attribute/value e (:effects e) :max-health)))

(defn- hurt? [e]
  (< 0.0 (health e) (top e)))

(defn- healed [e ^double amount]
  (if (pos? (health e))
    (assoc e :health (min (top e) (num/f32 (+ (health e) amount))))
    e))

(defn- peaceful-step [e ^long lived]
  (cond-> e
    (and (zero? (rem lived 20)) (< (health e) (top e))) (healed 1.0)
    (and (zero? (rem lived 20)) (< (saturation e) 20.0))
    (assoc :saturation (num/f32 (+ (saturation e) 1.0)))
    (and (zero? (rem lived 10)) (< (level e) full))
    (assoc :food (inc (level e)))))

(defn regenerated
  "Returns player e after the natural regeneration of a peaceful
  world, lived ticks old. Regen? is the game rule."
  [e lived peaceful? regen?]
  (if (and peaceful? regen?) (peaceful-step e lived) e))

(defn- spent [e]
  (if (> (exhaustion e) 4.0)
    (cond-> (assoc e :exhaustion (num/fsub (exhaustion e) 4.0))
      (pos? (saturation e))
      (assoc :saturation (max 0.0 (num/fsub (saturation e) 1.0))))
    e))

(defn- timed [e ^long wait heal cost]
  (let [t (inc (long (:food-timer e 0)))]
    (if (>= t wait)
      (-> e (healed heal) (exhausted cost) (assoc :food-timer 0))
      (assoc e :food-timer t))))

(defn ticked
  "Returns player e after one tick of its food. Regen? is the game
  rule of natural regeneration."
  [e regen?]
  (let [e (spent e)
        s (saturation e)]
    (cond
      (and regen? (pos? s) (hurt? e) (>= (level e) full))
      (timed e 10 (num/fdiv (min s 6.0) 6.0) (min s 6.0))
      (and regen? (>= (level e) 18) (hurt? e)) (timed e 80 1.0 6.0)
      :else (assoc e :food-timer 0))))

(defn hud
  "Returns what the last health packet player e got must match."
  [e]
  [(health e) (level e) (zero? (saturation e))])
