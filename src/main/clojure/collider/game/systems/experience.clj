(ns collider.game.systems.experience
  "Showing players their experience bar when it changed."
  (:require [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(defn player-deltas
  "Returns the deltas that show player eid its experience bar when
  it changed."
  [_world [eid e]]
  (let [total (long (:xp-total e 0))]
    (when-not (= total (:xp-sent e))
      (let [p (:xp-progress e 0.0) lvl (:xp-level e 0)]
        [(out/to eid (out/experience p lvl total))
         [:merge-entity eid {:xp-sent total}]]))))
