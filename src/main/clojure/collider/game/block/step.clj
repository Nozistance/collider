(ns collider.game.block.step
  "Blocks that act on the bodies standing on them."
  (:require [collider.game.changes :as changes]
            [collider.game.entity :as entity]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.motion :as motion]))

(set! *warn-on-reflection* true)

(def ^:private ^:const offset (double (float 0.2)))

(def ^:private ^:table steppables
  (delay (block/state-table
           :boolean
           #(#{:redstone-ore :turtle-egg} (block/type-of %)))))

(defn stepped-state
  "Returns the state of the block body e stands on, air when it is
  off the ground or a spectator."
  ^long [chunks e]
  (if (and (:on-ground e) (not (game-mode/spectator? e)))
    (motion/below-state chunks (:pos e) (:support e) offset)
    0))

(defn- lit-deltas [world p st]
  (when (= :false (:lit (block/props-of st)))
    (changes/change-deltas world [[p (block/with st :lit :true)]]
                           (:tick world))))

(defn- breaks-eggs? [world e]
  (and (entity/living? e) (not (#{:turtle :bat} (:type e)))
       (or (entity/player? e)
           (get-in world [:rules :mob-griefing] true))))

(defn- eggs-deltas [world p st]
  (let [n (block/prop-long st :eggs)
        t (:tick world)
        pitch (+ 0.9 (* 0.2 (random/of-key t p :egg)))
        sound (out/all (out/block-sound :entity.turtle.egg-break p 0.7
                                        pitch))
        broken (out/all (out/break-effect p st))]
    (if (<= n 1)
      (into [sound broken] (changes/change-deltas world [[p 0]] t))
      (-> [sound]
          (into (changes/change-deltas
                  world
                  [[p (block/with-long st :eggs (dec n)) nil 2]] t))
          (conj broken)))))

(defn- egg-deltas [world eid e p st]
  (when (and (breaks-eggs? world e)
             (< (random/of-key (:tick world) eid p) 0.01))
    (eggs-deltas world p st)))

(defn deltas
  "Returns the deltas of the block body e of eid stands on. Sneaking
  steps carefully."
  [world eid e]
  (let [chunks (:chunks world)
        st (stepped-state chunks e)]
    (when (and (aget ^booleans @steppables st) (not (:sneaking? e)))
      (let [p (motion/below-at chunks (:pos e) (:support e) offset)]
        (case (block/type-of st)
          :redstone-ore (lit-deltas world p st)
          :turtle-egg (egg-deltas world eid e p st))))))
