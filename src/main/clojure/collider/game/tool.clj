(ns collider.game.tool
  "The tool component of an item against the block it digs."
  (:require [collider.data :as data]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(defn- tool-of [stack] (get-in (data/items) [(:item stack) :tool]))

(defn- first-rule [stack st k]
  (let [b (block/block-of st)]
    (some #(when (and (contains? % k) (contains? (:blocks %) b))
             [(get % k)])
          (:rules (tool-of stack)))))

(defn mining-speed
  "Returns how fast stack digs st, by the first rule of its tool
  with a speed for the block."
  ^double [stack st]
  (double (or (first (first-rule stack st :speed))
              (:default-speed (tool-of stack))
              1.0)))

(defn correct-for-drops?
  "Returns true when the first rule of the tool of stack with a
  verdict for st lets it drop."
  [stack st]
  (boolean (first (first-rule stack st :correct?))))
