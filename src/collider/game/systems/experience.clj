(ns collider.game.systems.experience
  "Showing players their experience bar when it changed, as the
  lastSentExp check of ServerPlayer.doTick."
  (:require [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.game.level :as level]))

(set! *warn-on-reflection* true)

(defn- shown-deltas [[eid e]]
  (let [total (long (:xp-total e 0))]
    (when-not (= total (:xp-sent e))
      (let [p (:xp-progress e 0.0) lvl (:xp-level e 0)]
        [(out/to eid (out/experience p lvl total))
         [:merge-entity eid {:xp-sent total}]]))))

(defn experience
  "Returns the deltas that show each player whose experience changed
  its bar."
  {:wake {:types #{:player}}}
  [world _d]
  (deltas/of-vec
    (into [] (mapcat shown-deltas) (level/player-entries world))))
