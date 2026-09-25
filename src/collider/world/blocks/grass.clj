(ns collider.world.blocks.grass
  "Grass blocks: their states and where they stay alive."
  (:require [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private ^:table grass (delay (block/state :grass-block)))

(def ^:private ^:table dirt (delay (block/state :dirt)))

(defn grass-state ^long [] @grass)

(defn dirt-state ^long [] @dirt)

(defn short-grass? [st] (= :short-grass (block/block-of (long st))))

(defn can-stay-alive?
  "Whether grass or mycelium st lives under the block above.
  One snow layer lets it live and a full fluid kills it."
  [^long st ^long above]
  (cond
    (and (= :snow-layer (block/type-of above))
         (= :1 (:layers (block/props-of above)))) true
    (block/full-fluid? above) false
    :else (let [d (block/dampening above)]
            (< (block/light-dampening-into st above :up d) 15))))
