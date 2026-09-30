(ns collider.game.mob.pig
  "Pig goals and piglet coats."
  (:require [collider.game.mob.animal :as animal]))

(set! *warn-on-reflection* true)

(def ^:private stick
  (animal/tempt :stick-tempt :stick-cooldown-until
                (constantly #{:carrot-on-a-stick})))

(defn- lured
  "Pig.registerGoals: a carrot on a stick lures a pig ahead of its
  food, at the priority of the food, so neither lure takes a pig
  from the other."
  [goals]
  (let [food? #(= :tempt (:kind %))
        [before [food & after]] (split-with (complement food?) goals)
        n (count before)]
    (concat before [(assoc stick :prio n) (assoc food :prio n)]
            after)))

(defn- piglet-coat [_ t eid a b]
  (if (< (animal/rnd t eid :variant) 0.5) (:color a) (:color b)))

(def spec
  "The goals of a pig.
  A piglet takes the coat of either parent."
  (animal/spec (lured animal/goals) piglet-coat))

(defn brain
  "Returns the pig's next state and deltas for one tick."
  [world eid e t tempters]
  (animal/brain spec world eid e t tempters))
