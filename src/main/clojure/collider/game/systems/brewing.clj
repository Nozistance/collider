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

(defn- wakes
  "Returns the ops of the stand at pos that wake its comparators as e
  turns to e', once as it takes fuel and once as it brews or starts."
  [pos e e']
  (let [fuel #(long (:fuel % 0))
        brew #(long (:brew % 0))
        wake [:output-signal pos]]
    (cond-> []
      (> (fuel e') (fuel e)) (conj wake)
      (or (pos? (brew e)) (pos? (brew e'))) (conj wake))))

(defn tick-deltas
  "Returns the deltas of one tick of the brewing stand e at pos."
  [world [pos e]]
  (let [st (chunk/at (:chunks world) pos)
        [e' brewed? spill] (brewing/tick e)
        st' (with-bottles st (:items e'))
        ops (cond-> (wakes pos e e')
              (not= st st') (conj [:set [pos st'] 2]))]
    (concat (changes/entities-changed-deltas
              world (when (not= e e') [[:set-block-entity pos e']]) ops
              (:tick world))
            (when brewed? (brew-deltas world pos spill)))))
