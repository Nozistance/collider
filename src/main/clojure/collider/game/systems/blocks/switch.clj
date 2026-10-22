(ns collider.game.systems.blocks.switch
  "Pulling a lever and pressing a button."
  (:require [collider.cell :as cell]
            [collider.data :as data]
            [collider.game.changes :as changes]
            [collider.game.out :as out]
            [collider.math :as math]
            [collider.world.block :as block]
            [collider.world.blocks.button :as button]
            [collider.world.env.signal :as signal]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(defn switches?
  "Returns true when a hand switches the block at pos."
  [world pos]
  (button/switch? (changes/block-at world pos)))

(defn- switched [world pos st]
  (let [base (dec (long (:tick world)))
        tell [pos (button/holder pos st)]]
    (changes/flagged-deltas world [[pos st]] update/all tell base)))

(defn- event [on?] (if on? :block-activate :block-deactivate))

(defn- pulled [world pos st]
  (let [on? (not (button/powered? st))
        pitch (math/f32 (if on? 0.6 0.5))]
    (concat (switched world pos (button/powered st on?))
            [(out/all (out/block-sound :block.lever.click pos
                                       (math/f32 0.3) pitch))]
            (signal/game-event (event on?) pos nil))))

(defn- pressed [world eid pos st]
  (if (button/powered? st)
    ^:consumed []
    (let [block (block/block-of st)
          at (+ (long (:tick world)) (data/pressed-ticks block) -1)]
      (concat (switched world pos (button/powered st true))
              [[:schedule-ticks {at [(cell/pack pos)]}]
               (out/except eid (out/block-sound
                                 (data/open-sound block true)
                                 pos 1.0 1.0))]
              (signal/game-event (event true) pos eid)))))

(defn switch-deltas
  "Returns the deltas of player eid using the lever or button
  state at pos."
  [world eid pos state]
  (if (= :lever (block/type-of state))
    (pulled world pos state)
    (pressed world eid pos state)))
