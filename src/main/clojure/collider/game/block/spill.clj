(ns collider.game.block.spill
  "What the block entity of a removed block leaves in the world."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.furnace :as furnace]
            [collider.game.entity :as entity]
            [collider.game.item :as item]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.lectern :as lectern]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private listed
  #{:chest :trapped-chest :barrel :furnace :blast-furnace :smoker
    :brewing-stand :campfire :shelf :chiseled-bookshelf})

(defn- contents [e]
  (case (:kind e)
    :decorated-pot [(:item e)]
    (when (contains? listed (:kind e)) (:items e))))

(defn- spilled [world pos e]
  (let [t (:tick world)
        roll (fn [i] #(random/of-key t pos [:spill i] %))
        pour (fn [i s] (when s (item/scattered pos s (roll i))))]
    (into []
          (comp (map-indexed pour) cat
                (map (fn [it] [:spawn-entity it])))
          (contents e))))

(defn- record-popped [world pos e]
  (when-let [r (:record e)]
    (let [above (mapv + pos [0 1 0])
          stop (out/level-event :sound-stop-jukebox-song pos 0)]
      [[:spawn-entity (item/popped world above r :jukebox)]
       (out/all stop)])))

(defn- book-dropped [world pos old e]
  (when (and (lectern/has-book? old) (:book e))
    (let [[x y z] pos
          [dx _ dz] (dir/offset (block/facing-of old))
          p [(+ (double x) 0.5 (* 0.25 (double dx)))
             (double (inc (long y)))
             (+ (double z) 0.5 (* 0.25 (double dz)))]
          v (entity/pop-velocity [(:tick world) pos :lectern])]
      [[:spawn-entity (entity/item p v (:book e))]])))

(def ^:private furnaces #{:furnace :blast-furnace :smoker})

(defn- paid [world pos e]
  (when (contains? furnaces (:kind e))
    (let [t (:tick world)
          centre (mapv #(+ (double %) 0.5) pos)
          roll #(random/of-key t pos :spill-xp %)]
      (furnace/award-deltas e centre roll :spill))))

(defn removed-deltas
  "Returns the deltas of the block entity at pos going with its
  block old."
  [world pos old]
  (when-let [e (be/at world pos)]
    (case (:kind e)
      :jukebox (record-popped world pos e)
      :lectern (book-dropped world pos old e)
      (concat (spilled world pos e) (paid world pos e)))))
