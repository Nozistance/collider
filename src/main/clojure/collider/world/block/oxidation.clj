(ns collider.world.block.oxidation
  "Copper blocks that age through stages and keep a stage under wax."
  (:require [collider.data :as data]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(defn- facts [^long st] (get (data/blocks) (block/block-of st)))

(defn ages?
  "Returns true when st has a stage before or after it."
  [^long st]
  (let [b (facts st)]
    (boolean (and b (or (:next b) (:previous b))))))

(defn stage
  "Returns how many stages st is past the fresh block."
  ^long [^long st]
  (loop [b (facts st) n 0]
    (if-let [p (:previous b)]
      (recur (get (data/blocks) p) (inc n))
      n)))

(defn aged
  "Returns the state of the next stage of st, or nil."
  [^long st]
  (block/related st :next))

(defn scraped
  "Returns the state of the stage before st, or nil."
  [^long st]
  (block/related st :previous))

(defn waxed
  [^long st]
  (block/related st :waxed))

(defn unwaxed
  [^long st]
  (block/related st :unwaxed))
