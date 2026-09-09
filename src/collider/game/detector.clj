(ns collider.game.detector
  (:require [collider.game.detector.stats :as stats]))

(set! *warn-on-reflection* true)

(def channels [stats/observe])
(defn observe [world events deltas world']
  (into [] (mapcat #(% world events deltas world')) channels))
