(ns collider.game.systems.presses
  "Bodies pressing the plates they are in once they have moved."
  (:require [collider.game.apply :as apply]
            [collider.game.areas :as areas]
            [collider.game.changes :as changes]
            [collider.game.deltas :as deltas]
            [collider.game.entity.bodies :as bodies]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.world.block :as block]
            [collider.world.blocks.inside :as inside]
            [collider.world.blocks.plate :as plate]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:table plates
  (delay (block/state-table :boolean plate/plate?)))

(def ^:private unmoved
  #{:player :painting :item-frame :glow-item-frame :leash-knot
    :lightning-bolt :area-effect-cloud})

(defn- up? [^long st]
  (and (plate/plate? st) (zero? (plate/signal-of st))))

(defn- touched
  "Returns the cells of the plates at rest that the box of body e is
  in, in the order it meets them."
  [chunks e]
  (let [[half height] (size/box e) p (:pos e)]
    (for [[x y z st] (inside/met chunks [[p p nil]] half height @plates)
          :when (up? st)]
      [x y z])))

(defn- pressed [[w :as acc] p]
  (let [st (chunk/at (:chunks w) p)]
    (if (up? st)
      (let [t (:tick w)]
        (apply/then acc (changes/change-deltas
                          w (plate/checked p st (bodies/counter w) t) t)))
      acc)))

(defn- presses-of [world cells]
  (second (reduce pressed [world deltas/empty-deltas] (distinct cells))))

(defn presses
  "Returns the deltas of the plates the bodies but the players press
  after their turns, body by body."
  {:wake {:keys [:entities]}}
  [world _d]
  (let [chunks (:chunks world) active (areas/active-chunks world)]
    (presses-of world
                (for [[_ e] (:entities world)
                      :when (and (not (unmoved (:type e)))
                                 (areas/active-at? active (:pos e)))
                      c (touched chunks e)]
                  c))))

(defn player-deltas
  "Returns the deltas of the plates player e presses in its tick."
  [world [_ e]]
  (when-not (game-mode/spectator? e)
    (deltas/as-vec (presses-of world (touched (:chunks world) e)))))
