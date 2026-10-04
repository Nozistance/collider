(ns collider.game.mob.shell
  "The shell of an armadillo, its state, rolling up and out, and
  the hurts it takes rolled up."
  (:require [collider.data :as data]
            [collider.game.mob.brain :as brain]
            [collider.game.out :as out]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.env.signal :as signal])
  (:import (collider.game.mob Steer)))

(set! *warn-on-reflection* true)

(def ^:private state-ids {:idle 0 :rolling 1 :scared 2 :unrolling 3})

(def ^:private ^:const rolling-ticks 10)

(def ^:private ^:const danger-ticks 80)

(def ^:private ^:const unscared-head-y-rot 32.0)

(def ^:private panic-tag "panic_environmental_causes")

(def ^:private ^:table panic-causes
  (delay (set (data/tag-values "damage_type" panic-tag))))

(defn state
  "Returns the state of armadillo e, which is idle, rolling, scared
  or unrolling."
  [e]
  (or (:armadillo-state e) :idle))

(defn state-id ^long [e] (state-ids (state e)))

(defn scared?
  "Returns true when armadillo e is in any state but idle."
  [e]
  (not= :idle (state e)))

(defn in-state-ticks ^long [e] (long (or (:in-state-ticks e) 0)))

(defn switched
  "Returns armadillo e in state s. Only a new state starts its ticks
  from zero."
  [e s]
  (if (= s (state e))
    e
    (assoc e :armadillo-state s :in-state-ticks 0)))

(defn should-switch-to-scared-state? [e]
  (and (= :rolling (state e)) (> (in-state-ticks e) rolling-ticks)))

(defn aged
  "Returns armadillo e one tick longer in its state. The count stops
  past the rolling time, the longest the server waits on it."
  [e]
  (let [n (in-state-ticks e)]
    (if (> n rolling-ticks) e (assoc e :in-state-ticks (inc n)))))

(defn max-head-y-rot
  "Returns how far armadillo e turns its head from its body."
  ^double [e]
  (if (scared? e) 0.0 unscared-head-y-rot))

(defn sound-key
  "Returns the sound k armadillo e makes as, or nil for none. A
  scared armadillo is silent and hurts muffled."
  [e k]
  (if (scared? e) (get {:say nil :hurt :hurt-reduced} k k) k))

(defn- stopped-in-place [e]
  (cond-> (assoc e :vel (v/v3 0.0 0.0 0.0) :love-until nil)
    (:path (:nav e)) (assoc-in [:nav :path] nil)
    (:move e) (update :move #(Steer/jumped % 0.0 false))))

(defn- said [e k]
  (update e :shell-sounds (fnil conj []) k))

(defn roll-up
  "Returns armadillo e rolled up, or e when it is scared already. It
  stops where it stands and falls out of love."
  [e]
  (if (scared? e)
    e
    (-> (stopped-in-place e) (said :armadillo/roll)
        (switched :rolling))))

(defn roll-out
  "Returns scared armadillo e back out of its shell, or e when it is
  not scared."
  [e]
  (if (scared? e)
    (-> (said e :armadillo/unroll-finish) (switched :idle))
    e))

(defn- pitch ^double [e t eid i]
  (let [r #(random/of-key t eid [:shell i %])
        base (if (some? (:baby-until e)) 1.5 1.0)]
    (+ base (* 0.2 (- (r 1) (r 2))))))

(defn shell-deltas
  "Returns the game event and the sound of each roll of armadillo eid
  not yet heard, at tick t."
  [eid e t]
  (let [at (:pos e)
        heard (fn [i k]
                (let [p (pitch e t eid i)]
                  (conj (signal/game-event :entity-action at eid)
                        (out/all (out/sound k at 1.0 p)))))]
    (into [] (comp (map-indexed heard) cat) (:shell-sounds e))))

(defn can-stay-rolled-up?
  "Returns true when armadillo e may stay rolled up at tick t, while
  it does not panic and stands in no liquid."
  [e t]
  (not (or (brain/present? e :is-panicking t) (:wet? e)
           (:in-lava? e))))

(defn taken
  "Returns damage amount as armadillo e takes it, halved past the
  first point when scared. It may go below zero."
  ^double [e ^double amount]
  (if (scared? e) (num/fdiv (num/fsub amount 1.0) 2.0) amount))

(defn- startled [e t]
  (let [k :danger-detected-recently
        e (brain/remember-for e k true t danger-ticks)]
    (if (can-stay-rolled-up? e t) (roll-up e) e)))

(defn hurt
  "Returns living armadillo e after a hurt from src at tick t. A
  living attacker scares it into its shell, and a hurt the place
  gave brings it out."
  [e src t living-attacker?]
  (cond (not (pos? (double (:health e)))) e
        living-attacker? (startled e t)
        (contains? @panic-causes (:type src)) (roll-out e)
        :else e))
