(ns collider.game.detector
  "Detector: looks at one tick when it is done and adds what it saw. It gets
   the world before the events, the events, the deltas and the world after,
   and returns deltas of its own. It does not take part in the tick.
   Channels: statistics."
  (:require [collider.game.detector.stats :as stats]))

(set! *warn-on-reflection* true)

(def channels [stats/observe])

(defn observe [world events deltas world']
  (into [] (mapcat #(% world events deltas world')) channels))
