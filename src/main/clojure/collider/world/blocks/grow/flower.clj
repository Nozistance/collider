(ns collider.world.blocks.grow.flower
  "Flowers that open, close or grow on a random tick."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.blocks.eyeblossom :as eyeblossom]))

(set! *warn-on-reflection* true)

(defn eyeblossom-tick
  "Returns the change that opens or closes an eyeblossom."
  [chunks p st _roll time world]
  (when-let [new (eyeblossom/switched (long st) (long time))]
    (let [t (long (:tick world 0))]
      [[p new (eyeblossom/switch-fx chunks p st new t true)]])))

(def ^:private potted-eyeblossom
  {:potted-open-eyeblossom   :potted-closed-eyeblossom
   :potted-closed-eyeblossom :potted-open-eyeblossom})

(defn- night? [^long time] (<= 12600 (mod time 24000) 23400))

(defn potted-tick
  "Returns the change that opens or closes a potted eyeblossom."
  [_chunks p st _roll time _world]
  (let [self (block/block-of (long st))]
    (when (contains? potted-eyeblossom self)
      (let [open? (= :potted-open-eyeblossom self)]
        (when (not= open? (night? (long time)))
          [[p (block/state (potted-eyeblossom self))]])))))

(defn chorus-tick
  "Returns the growth of a random tick of a chorus flower."
  [chunks p st roll _time _world]
  (let [pick (fn [salt ^long n] (random/below (roll salt) n))]
    (chorus/flower-tick chunks p st pick)))
