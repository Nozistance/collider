(ns collider.world.blocks.grass
  "Grass blocks, their states and where they stay alive."
  (:require [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private ^:table grass (delay (block/state :grass-block)))

(def ^:private ^:table dirt (delay (block/state :dirt)))

(defn grass-state
  ^long []
  @grass)

(defn dirt-state
  ^long []
  @dirt)

(defn can-stay-alive?
  "Returns true when grass or mycelium st lives under the block
  above. One snow layer lets it live and a full fluid kills it."
  [^long st ^long above]
  (cond
    (and (= :snow-layer (block/type-of above))
         (= :1 (:layers (block/props-of above)))) true
    (block/full-fluid? above) false
    :else (let [d (block/dampening above)]
            (< (block/light-dampening-into st above :up d) 15))))

(defn tall-of
  "Returns the lower and the upper half of the tall plant that the
  short grass or fern st grows into."
  [st]
  (let [fern? (= :fern (block/block-of (long st)))
        tall (if fern? :large-fern :tall-grass)]
    [(block/state tall) (block/state tall {:half :upper})]))
