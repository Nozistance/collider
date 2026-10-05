(ns collider.game.mob.goat-ai
  "The brain of a goat and the horns its rams break off."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.entity :as entity]
            [collider.game.mob.behavior.core :as c]
            [collider.game.mob.behavior.long-jump :as jump]
            [collider.game.mob.behavior.ram :as ram]
            [collider.game.mob.behavior.social :as s]
            [collider.game.mob.brain :as b]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sensor :as sensor]
            [collider.game.stack :as stack]
            [collider.num :as num])
  (:import (java.util Random UUID)))

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

(def ^:private between-jumps [600 1200])

(defn- sound [k] #(mobs/sound-of % k))

(def ^:private long-jump
  [[0 (jump/long-jump-mid-jump between-jumps (sound :step))]
   [1 (jump/long-jump-to-random-pos
        between-jumps 5 5 3.5714288 (sound :long-jump))]])

(def ^:private jump-ready
  {:tempting-player :absent :breed-target :absent :walk-target :absent
   :long-jump-cooldown-ticks :absent})

(defn- horn
  "Returns the horn goat eid, e, drops. Its instrument is drawn from
  the uuid of the goat."
  [eid e]
  (let [tag (if (:screaming? e)
              "screaming_goat_horns" "regular_goat_horns")
        vs (data/tag-values "instrument" tag)
        uuid ^UUID (entity/uuid-of eid e)
        s {:item :goat-horn :count 1}]
    (if (seq vs)
      (let [r (Random. (long (.hashCode uuid)))]
        (stack/put s :instrument (nth vs (.nextInt r (count vs)))))
      s)))

(defn- horn-side [e t eid i]
  (cond (false? (:left-horn? e)) :right-horn?
        (false? (:right-horn? e)) :left-horn?
        (< (c/roll t eid i :horn) 0.5) :left-horn?
        :else :right-horn?))

(defn- spread [t eid i k lo hi]
  (let [r (num/f32 (c/roll t eid i k))
        lo (num/f32 lo) w (num/f32 (- (num/f32 hi) lo))]
    (double (num/f32 (+ (num/f32 (* r w)) lo)))))

(defn- hornless? [e]
  (and (false? (:left-horn? e)) (false? (:right-horn? e))))

(defn- dropped-horn
  "Returns [e deltas] of goat eid, e, losing a horn at tick t, or nil
  for a kid or a goat with no horn left."
  [_ eid e t i]
  (when-not (or (mobs/baby? e) (hornless? e))
    (let [k (horn-side e t eid i)
          r (partial spread t eid i)
          vel [(r :hx -0.2 0.2) (r :hy 0.3 0.7) (r :hz -0.2 0.2)]
          item (entity/item (:pos e) vel (horn eid e) 0)]
      [(assoc e k false)
       [[:merge-entity eid {k false}] [:spawn-entity item]]])))

(defn- attack-damage ^double [e]
  (if (mobs/baby? e)
    1.0
    (attribute/value e (:effects e) :attack-damage)))

(defn- between-rams [e] (if (:screaming? e) [100 300] [600 6000]))

(def ^:private ram
  [[0 (ram/ram-target
        {:between between-rams :foe? ram/foe? :speed 3.0
         :force #(if (mobs/baby? %) 1.0 2.5) :damage attack-damage
         :impact (sound :ram-impact) :horn-break (sound :horn-break)
         :drop-horn dropped-horn})]
   [1 (ram/prepare-ram-nearest-target
        (comp first between-rams) [4 7] 1.25 ram/foe? 20
        (sound :prepare-ram))]])

(def ^:private ram-ready
  {:tempting-player :absent :breed-target :absent
   :ram-cooldown-ticks :absent})

(def ^:private sensors
  [(sensor/nearest-living) (sensor/players) (sensor/adult)
   (sensor/hurt-by) (sensor/tempting sensor/food-lure?)])

(def ^:private at-rest
  {:ram-target :absent :long-jump-mid-jump :absent})

(def breed
  (b/breed
    (sensor/with-sensors
      {:core core :idle idle :long-jump long-jump :ram ram
       :activities [:core :idle :long-jump :ram]
       :requires {:idle at-rest :long-jump jump-ready :ram ram-ready}
       :update [:ram :long-jump :idle]
       :memories [:long-jump-cooldown-ticks :ram-cooldown-ticks
                  :ram-target :long-jump-mid-jump]}
      sensors)))
