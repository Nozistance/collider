(ns collider.game.systems.brewing
  "Brewing stands brewing."
  (:require [collider.game.systems.blocks.edit :as edit]
            [collider.game.block.brewing :as brewing]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private bottle-props
  [:has-bottle-0 :has-bottle-1 :has-bottle-2])

(defn- bottle-prop [items i p]
  [p (if (nth items (long i)) :true :false)])

(defn- with-bottles ^long [^long st items]
  (block/state (block/block-of st)
               (into (block/props-of st)
                     (map-indexed (partial bottle-prop items))
                     bottle-props)))

(defn- brew-deltas [world pos spill]
  (cons (out/all (out/level-event out/sound-brewing-stand-brew pos))
        (when spill
          [[:spawn-entity (items/popped world pos spill :brew)]])))

(defn tick-deltas
  "Returns the deltas of one tick of the brewing stand e at pos."
  [world [pos e]]
  (let [st (chunk/chunks-get-block (:chunks world) pos)
        [e' brewed? spill] (brewing/tick e)
        st' (with-bottles st (:items e'))]
    (concat (when (not= e e') [[:set-block-entity pos e']])
            (when (not= st st') (edit/set-deltas world [[pos st']]))
            (when brewed? (brew-deltas world pos spill)))))
