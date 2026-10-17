(ns collider.world.blocks.button
  "Buttons and levers, what they tell when they switch, and the
  button that springs back."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn switch?
  "Returns true when st is a lever or a button."
  [^long st]
  (contains? #{:lever :button} (block/type-of st)))

(defn powered?
  [^long st]
  (= :true (:powered (block/props-of st))))

(defn powered
  ^long [^long st on?]
  (block/with st :powered (block/flag on?)))

(defn holder
  "Returns the cell of the block that holds the switch st at p."
  [p ^long st]
  (dir/toward p (dir/opposite (support/connected-direction st))))

(defn told
  "Returns the ops that tell the cells around the switch st at p and
  around the block that holds it."
  [p ^long st]
  [[:notify p nil st] [:notify (holder p st) nil st]])

(defn- released [chunks p _ctx]
  (let [st (chunk/at chunks p)]
    (when (and (= :button (block/type-of st)) (powered? st))
      (let [st' (powered st false)
            sound (data/open-sound (block/block-of st) false)]
        (into [[p st' [[:sound sound 1.0 1.0]
                       [:game-event :block-deactivate]]]]
              (told p st'))))))

(def rule
  "The block rule of buttons. They are held as attached blocks are
  and spring back on their tick."
  (assoc support/rule
         :name :button
         :match? (fn [_chunks st _p] (= :button (block/type-of st)))
         :due released))
