(ns collider.game.mob.chicken
  "Chicken coats, slow falls and laid eggs."
  (:require [collider.game.entity.gen :as gen]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.gift :as gift]
            [collider.game.mob.mobs :as mobs]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const egg-ticks 6000)

(def ^:private ^:const fall-drag 0.6)

(defn- chick-coat [_ t eid a b]
  (if (< (animal/rnd t eid :variant) 0.5) (:variant a) (:variant b)))

(def spec
  "The goals of a chicken.
  A chick takes the coat of either parent."
  (animal/spec animal/goals chick-coat))

(defn brain
  [world eid e t tempters]
  (animal/brain spec world eid e t tempters))

(defn- egg-time ^long [t eid]
  (gift/wait t eid :egg-time egg-ticks))

(defn- slowed
  "Returns chicken e, which falls slower off the ground."
  [e]
  (let [vel (:vel e)]
    (if (or (:on-ground e) (not (neg? (v/y vel))))
      e
      (let [vy (* (v/y vel) fall-drag)]
        (gen/with e {:vel (v/v3 (v/x vel) vy (v/z vel))})))))

(defn- laid [t eid e]
  (gift/gift-deltas t eid e :chicken-lay :chicken/egg :lay))

(defn- layer? [e]
  (and (pos? (double (:health e))) (not (mobs/baby? e))))

(defn- laying
  "Returns chicken e after its egg time at tick t. A grown chicken
  lays an egg when its egg time runs out and waits again. The wait
  starts on the first tick it is grown and counts this tick."
  [eid e t]
  (let [at (:egg-at e)
        wait (egg-time t eid)]
    (cond
      (not (layer? e)) [e nil]
      (nil? at) [(gen/with e {:egg-at (+ (long t) -1 wait)}) nil]
      (< (long t) (long at)) [e nil]
      :else [(gen/with e {:egg-at (+ (long t) wait)})
             (laid t eid e)])))

(defn ai-step
  "Returns chicken e after the part of its step that is its own, and
  its deltas. It falls slower and lays eggs."
  [eid e t]
  (laying eid (slowed e) t))
