(ns collider.game.block.press
  "Plates and string that bodies press as they meet them."
  (:require [collider.cell :as cell]
            [collider.game.apply :as apply]
            [collider.game.changes :as changes]
            [collider.game.deltas :as deltas]
            [collider.game.entity.bodies :as bodies]
            [collider.game.entity.size :as size]
            [collider.game.schedule :as schedule]
            [collider.world.block :as block]
            [collider.world.blocks.inside :as inside]
            [collider.world.blocks.plate :as plate]
            [collider.world.blocks.tripwire :as tripwire]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:table pressables
  "The states bodies press, plates and string."
  (delay (block/state-table :boolean
                            #(or (plate/plate? %) (tripwire/wire? %)))))

(def ^:private ^:table wires
  (delay (block/state-table :boolean tripwire/wire?)))

(defn up?
  "Returns true when st at p waits for a body to press it. String
  waits only with no tick of its own pending in world w."
  [w p ^long st]
  (cond
    (plate/plate? st) (zero? (plate/signal-of st))
    (tripwire/wire? st)
    (and (not (tripwire/powered? st))
         (not (schedule/scheduled? (:block-ticks w) (cell/pack p)
                                   :tripwire)))
    :else false))

(defn- ops [w p ^long st]
  (let [t (:tick w)]
    (if (plate/plate? st)
      (plate/checked p st (bodies/counter w) t)
      (tripwire/checked p st true t))))

(defn- pressed [[w :as acc] p]
  (let [st (chunk/at (:chunks w) p)]
    (if (up? w p st)
      (let [t (:tick w)]
        (apply/then acc (changes/change-deltas w (ops w p st) t)))
      acc)))

(defn pressed-at
  "Returns the deltas of the plates and string at cells pressed one by
  one in world."
  [world cells]
  (second (reduce pressed [world deltas/empty-deltas] (distinct cells))))

(defn tripped
  "Returns the deltas of the string body e trips on its move from point
  from to point to."
  [world e from to]
  (let [[half height] (size/box e)]
    (deltas/as-vec
      (pressed-at world
                  (for [[x y z st] (inside/met (:chunks world)
                                               [[from to nil]] half height
                                               @wires)
                        :when (up? world [x y z] st)]
                    [x y z])))))
