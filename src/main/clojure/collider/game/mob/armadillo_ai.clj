(ns collider.game.mob.armadillo-ai
  "The brain of an armadillo, the danger it senses, balling up and
  peeking out, and the activities it picks."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.mob.behavior.core :as c]
            [collider.game.mob.behavior.social :as s]
            [collider.game.mob.brain :as b]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sensor :as sensor]
            [collider.game.mob.shell :as shell]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private danger :danger-detected-recently)

(def ^:private ^:const danger-ticks 80)

(def ^:private ^:const hurt-by-mob-ticks 100)

(def ^:private ^:const scared-ticks 50)

(def ^:private ^:const unrolling-ticks 30)

(def ^:private ^:const danger-threshold 75)

(def ^:private ^:table undead
  (delay (set (data/tag-values "entity_type" "undead"))))

(defn- overlaps? [p q w ow]
  (and (< (- (double p) (double w) 7.0) (+ (double q) (double ow)))
       (> (+ (double p) (double w) 7.0) (- (double q) (double ow)))))

(defn- near?
  "Returns true when the box of e grown by 7, 2 and 7 meets the box
  of o."
  [e o]
  (let [[w h] (entity/box e) [ow oh] (entity/box o)
        p (:pos e) q (:pos o) y (v/y p) oy (v/y q)]
    (and (overlaps? (v/x p) (v/x q) w ow)
         (< (- y 2.0) (+ oy (double oh)))
         (> (+ y (double h) 2.0) oy)
         (overlaps? (v/z p) (v/z q) w ow))))

(defn- last-hurt-by-mob? [e oid t]
  (let [[src at] (:last-hurt e)]
    (and (entity/living-attacker? src) (= oid (:cause src))
         (<= (- (long t) (long at)) hurt-by-mob-ticks))))

(defn- sprints? [o]
  (and (entity/player? o) (not (game-mode/spectator? o))
       (boolean (:sprinting? o))))

(defn scared-by?
  "Returns true when living entity oid of world scares armadillo e at
  tick t."
  [world e oid t]
  (let [o (get (:entities world) oid)]
    (boolean
      (and o (near? e o)
           (or (contains? @undead (:type o))
               (last-hurt-by-mob? e oid t) (sprints? o))))))

(defn- peek-timer ^long [t eid i k]
  (+ scared-ticks (c/sample t eid i k [100 400])))

(defn- heard [e k] (out/all (out/sound k (:pos e) 1.0 1.0)))

(defn- landed [e]
  [(shell/switched e :scared)
   (when (:on-ground e) [(heard e :armadillo/land)])])

(defn- with-peek [e i n]
  (b/with-slot e i (assoc (b/slot e i) :peek n)))

(defn- peeked [eid e t i around?]
  (if (and around? (:on-ground e)
           (zero? (long (:peek (b/slot e i)))))
    [(with-peek e i (peek-timer t eid i :peek))
     [(out/all (out/status eid :peek))]]
    [e []]))

(defn- scared-tick [eid e t i ttl around?]
  (let [[e ds] (peeked eid e t i around?)]
    (if (< (long ttl) unrolling-ticks)
      [(shell/switched e :unrolling)
       (conj ds (heard e :armadillo/unroll-start))]
      [e ds])))

(defn- watched [eid e t i]
  (let [ttl (b/ttl-of e danger t)
        around? (> ttl danger-threshold)
        {:keys [peek was]} (b/slot e i)
        n (if (= around? (boolean was))
            peek
            (peek-timer t eid i :toggle))
        e (b/with-slot e i {:peek n :was around?})]
    (case (shell/state e)
      :scared (scared-tick eid e t i ttl around?)
      :unrolling (cond-> e (> ttl unrolling-ticks)
                   (shell/switched :scared))
      e)))

(defn- ball-tick [_ eid e t i]
  (let [e (with-peek e i (max 0 (dec (long (:peek (b/slot e i) 0)))))]
    (if (shell/should-switch-to-scared-state? e)
      (landed e)
      (watched eid e t i))))

(def ^:private ball-up
  {:id :armadillo-ball-up :duration [6000 6000]
   :start? (fn [_ _ e _] (boolean (:on-ground e)))
   :continue? (fn [_ _ e _ _] (shell/scared? e))
   :start (fn [_ _ e _ _] (shell/roll-up e))
   :tick ball-tick
   :stop (fn [_ _ e t _]
           (cond-> e
             (not (shell/can-stay-rolled-up? e t)) shell/roll-out))})

(def ^:private panic
  (update (c/animal-panic 2.0 "panic_environmental_causes") :start
          (fn [f]
            (fn [w eid e t i] (f w eid (shell/roll-out e) t i)))))

(def ^:private rolling-out
  {:id :armadillo-rolling-out :needs {danger :absent}
   :one-shot (fn [_ _ e _ _]
               (when (shell/scared? e) (shell/roll-out e)))})

(defn- close-to-tempter ^double [e] (if (mobs/baby? e) 1.0 2.0))

(def ^:private core
  [(c/swim 0.8) panic (c/look-at-target-sink 45 90)
   (c/move-to-target-sink (complement shell/scared?))
   (c/count-down-cooldown-ticks :temptation-cooldown-ticks)
   (c/count-down-cooldown-ticks :gaze-cooldown-ticks) rolling-out])

(def ^:private tempted
  (s/follow-temptation (constantly 1.25) close-to-tempter))

(def ^:private follow
  {:id :follow :gate :run-one :order :shuffled
   :items [[tempted 1] [(s/baby-follow-adult [5 16] 1.25) 1]]})

(def ^:private wander
  {:id :wander :gate :run-one :order :shuffled
   :needs {:walk-target :absent}
   :items [[(c/random-stroll 1.0) 1]
           [(c/set-walk-target-from-look-target 1.0 3) 1]
           [(c/do-nothing 30 60) 1]]})

(def ^:private idle
  [(s/set-entity-look-target-sometimes :player 6.0 [30 60])
   (s/animal-make-love :armadillo 1.0 1) follow
   (c/random-look-around [150 250] 30.0 0.0 0.0) wander])

(def ^:private sensors
  [(sensor/nearest-living) (sensor/hurt-by)
   (sensor/tempting sensor/food-lure?) (sensor/adult)
   (sensor/mob-sensor 5 scared-by? shell/can-stay-rolled-up? danger
                      danger-ticks)])

(def breed
  (b/breed
    (sensor/with-sensors
      {:core core :idle idle :panic [ball-up]
       :requires {:panic {danger :present :is-panicking :absent}}
       :update [:panic :idle] :memories [danger]}
      sensors)))
