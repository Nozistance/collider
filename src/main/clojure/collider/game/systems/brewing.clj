(ns collider.game.systems.brewing
  "Brewing stands brewing."
  (:require [collider.game.block.brewing :as brewing]
            [collider.game.changes :as changes]
            [collider.game.item :as item]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private bottle-props
  [:has-bottle-0 :has-bottle-1 :has-bottle-2])

(defn- with-bottles ^long [^long st items]
  (let [flag #(block/flag (nth items (long %)))
        flags (map flag (range (count bottle-props)))]
    (apply block/with st (interleave bottle-props flags))))

(defn- brew-deltas [world pos spill]
  (cons (out/all (out/level-event :sound-brewing-stand-brew pos))
        (when spill
          [[:spawn-entity (item/popped world pos spill :brew)]])))

(defn tick-deltas
  [world [pos e]]
  (let [st (chunk/at (:chunks world) pos)
        [e' brewed? spill] (brewing/tick e)
        st' (with-bottles st (:items e'))]
    (concat (when (not= e e') [[:set-block-entity pos e']])
            (when (not= st st')
              (changes/set-deltas world [[pos st']]))
            (when brewed? (brew-deltas world pos spill)))))
