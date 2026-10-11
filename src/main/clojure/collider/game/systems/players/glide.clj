(ns collider.game.systems.players.glide
  "A gliding player stops once it may no longer glide."
  (:require [collider.game.entity.metadata :as metadata]
            [collider.game.glide :as glide]
            [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(defn- flags [e]
  (select-keys (metadata/of e)
               [:burning? :sneaking? :sprinting? :swimming? :invisible?
                :glowing? :fall-flying?]))

(defn- shown [eid e]
  (let [m (out/meta eid :player (flags e))]
    [(out/all m) (out/to eid m)]))

(defn player-deltas
  "Returns the deltas of player eid, e, gliding this tick."
  [_ [eid e]]
  (let [gliding? (and (:fall-flying e) (glide/can-glide? e))]
    (cond-> []
      (:glide-shown? e) (into (shown eid e))
      (:glide-shown? e) (conj [:merge-entity eid {:glide-shown? nil}])
      (and (:fall-flying e) (not gliding?))
      (conj [:merge-entity eid {:fall-flying false :fall-fly-ticks 0}])
      gliding?
      (conj [:merge-entity eid {:fall-fly-ticks (inc (long (:fall-fly-ticks e 0)))}])
      (and (not (:fall-flying e)) (pos? (long (:fall-fly-ticks e 0))))
      (conj [:merge-entity eid {:fall-fly-ticks 0}]))))
