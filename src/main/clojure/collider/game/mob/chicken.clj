(ns collider.game.mob.chicken
  "Chicken coats, slow falls and laid eggs."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.loot :as loot]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(def ^:private ^:const egg-ticks 6000)

(def ^:private ^:const fall-drag 0.6)

(def ^:private ^:table tables (delay (data/entity-drops)))

(defn- chick-coat [_ t eid a b]
  (if (< (animal/rnd t eid :variant) 0.5) (:color a) (:color b)))

(def spec
  "The goals of a chicken.
  A chick takes the coat of either parent."
  (animal/spec animal/goals chick-coat))

(defn brain
  [world eid e t tempters]
  (animal/brain spec world eid e t tempters))

(defn- egg-time ^long [t eid]
  (+ egg-ticks (long (* egg-ticks (random/of-key t eid :egg-time)))))

(defn- slowed
  "Returns chicken e, which falls slower off the ground."
  [e]
  (let [vel (:vel e)]
    (if (or (:on-ground e) (not (neg? (v/y vel))))
      e
      (let [vy (* (v/y vel) fall-drag)]
        (entity/with e {:vel (v/v3 (v/x vel) vy (v/z vel))})))))

(defn- eggs [t eid e]
  (loot/drops @tables :chicken-lay {:entity (mobs/loot-entity e)}
              #(random/of-key t eid [:lay %])))

(defn- pitch ^double [t eid]
  (let [r #(random/of-key t eid [:lay-pitch %])]
    (+ 1.0 (* 0.2 (- (double (r 1)) (double (r 2)))))))

(defn- laid [t eid e]
  (let [pos (:pos e)
        drop (fn [i s]
               (let [vel (entity/pop-velocity [t eid :lay i])]
                 [:spawn-entity (entity/item pos vel s)]))]
    (when-let [ds (seq (map-indexed drop (eggs t eid e)))]
      (let [snd (out/sound :chicken/egg pos 1.0 (pitch t eid))]
        (concat ds [(out/all snd)]
                (signal/game-event :entity-place pos eid))))))

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
      (nil? at) [(entity/with e {:egg-at (+ (long t) -1 wait)}) nil]
      (< (long t) (long at)) [e nil]
      :else [(entity/with e {:egg-at (+ (long t) wait)})
             (laid t eid e)])))

(defn ai-step
  "Returns chicken e after the part of its step that is its own, and
  its deltas. It falls slower and lays eggs."
  [eid e t]
  (laying eid (slowed e) t))
