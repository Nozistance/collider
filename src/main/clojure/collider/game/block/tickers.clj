(ns collider.game.block.tickers
  "The block entities that tick, in the order they joined the level,
  as Level.blockEntityTickers kept by LevelChunk.tickersInLevel. A
  ticker that stays keeps its turn, a new one goes last."
  (:require [collider.data.long-map :as lm]
            [collider.world.block :as block])
  (:import (collider.game.block HashOrder)))

(set! *warn-on-reflection* true)

(def none
  "No ticker yet."
  {:turns (lm/long-map) :turn-of {} :next 0})

(defn ticks?
  "Returns true when block entity e ticks under block state st."
  [e st]
  (case (:kind e)
    (:furnace :blast-furnace :smoker :campfire :brewing-stand
     :sign :hanging-sign) true
    :jukebox (= :true (:has-record (block/props-of st)))
    :potent-sulfur
    (not= :dry (:potent-sulfur-state (block/props-of st)))
    false))

(defn- added [tk pos]
  (let [tk (or tk none)
        n (long (:next tk))]
    (if (contains? (:turn-of tk) pos)
      tk
      {:turns (assoc (:turns tk) n pos)
       :turn-of (assoc (:turn-of tk) pos n)
       :next (inc n)})))

(defn without
  "Returns tickers tk without the one at pos."
  [tk pos]
  (if-let [n (get (:turn-of tk) pos)]
    (assoc tk :turns (dissoc (:turns tk) n)
              :turn-of (dissoc (:turn-of tk) pos))
    tk))

(defn bound
  "Returns tickers tk with the one at pos when on?, else without it."
  [tk pos on?]
  (if on? (added tk pos) (without tk pos)))

(defn- hash-order [ps]
  (let [ps (vec ps)
        xyz (int-array (into [] cat ps))]
    (mapv #(nth ps %) (HashOrder/of xyz))))

(defn loaded
  "Returns tickers tk with the block entities bes of a loaded chunk,
  a map of pos to entity. state-of gives the block state at a pos."
  [tk bes state-of]
  (let [on? #(ticks? (get bes %) (state-of %))]
    (reduce added tk (filter on? (hash-order (keys bes))))))
