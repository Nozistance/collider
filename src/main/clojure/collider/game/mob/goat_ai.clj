(ns collider.game.mob.goat-ai
  "The brain of a goat at rest."
  (:require [collider.game.mob.behavior.core :as c]
            [collider.game.mob.behavior.social :as s]
            [collider.game.mob.brain :as b]
            [collider.game.mob.sensor :as sensor]))

(set! *warn-on-reflection* true)

(def ^:private core
  [(c/swim 0.8) (c/animal-panic 2.0) (c/look-at-target-sink 45 90)
   (c/move-to-target-sink)
   (c/count-down-cooldown-ticks :temptation-cooldown-ticks)
   (c/count-down-cooldown-ticks :long-jump-cooldown-ticks)
   (c/count-down-cooldown-ticks :ram-cooldown-ticks)])

(def ^:private wander
  {:id :wander :gate :run-one :order :shuffled
   :items [[(c/random-stroll 1.0) 2]
           [(c/set-walk-target-from-look-target 1.0 3) 2]
           [(c/do-nothing 30 60) 1]]})

(def ^:private idle
  [[0 (s/set-entity-look-target-sometimes :player 6.0 [30 60])]
   [0 (s/animal-make-love :goat)]
   [1 (s/follow-temptation (constantly 1.25))]
   [2 (s/baby-follow-adult [5 16] 1.25)]
   [3 wander]])

(def ^:private sensors
  [(sensor/nearest-living) (sensor/players) (sensor/adult)
   (sensor/hurt-by) (sensor/tempting sensor/food-lure?)])

(def ^:private at-rest
  {:ram-target :absent :long-jump-mid-jump :absent})

(def breed
  (b/breed
    (sensor/with-sensors
      {:core core :idle idle :requires {:idle at-rest} :update [:idle]
       :memories [:long-jump-cooldown-ticks :ram-cooldown-ticks
                  :ram-target :long-jump-mid-jump]}
      sensors)))
