(ns collider.game.systems.players.glide
  "A gliding player stops once it may no longer glide."
  (:require [collider.game.entity.metadata :as metadata]
            [collider.game.glide :as glide]
            [collider.game.inventory :as inventory]
            [collider.random :as random]
            [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(defn- flags [e]
  (select-keys (metadata/of e)
               [:burning? :sneaking? :sprinting? :swimming? :invisible?
                :glowing? :fall-flying?]))

(defn- shown [eid e]
  (let [m (out/meta eid :player (flags e))]
    [(out/all m) (out/to eid m)]))

(def ^:private parts {:mainhand :main :offhand :off})

(defn- wear-deltas
  "Returns the deltas of wearing one glider of player eid, e, picked
  at random, every 20 ticks of gliding."
  [t eid e]
  (when (zero? (mod (inc (long (:fall-fly-ticks e 0))) 20))
    (let [gs (glide/glider-slots e)
          roll (random/of-key t eid :glide-wear)
          [i part] (nth gs (long (* (count gs) roll)))]
      (inventory/hurt-slot-deltas t eid e i (parts part part) 1))))

(defn player-deltas
  "Returns the deltas of player eid, e, gliding this tick."
  [world [eid e]]
  (let [gliding? (and (:fall-flying e) (glide/can-glide? e))]
    (cond-> (if gliding? (vec (wear-deltas (:tick world) eid e)) [])
      (:glide-shown? e) (into (shown eid e))
      (:glide-shown? e) (conj [:merge-entity eid {:glide-shown? nil}])
      (and (:fall-flying e) (not gliding?))
      (conj [:merge-entity eid {:fall-flying false :fall-fly-ticks 0}])
      gliding?
      (conj [:merge-entity eid {:fall-fly-ticks (inc (long (:fall-fly-ticks e 0)))}])
      (and (not (:fall-flying e)) (pos? (long (:fall-fly-ticks e 0))))
      (conj [:merge-entity eid {:fall-fly-ticks 0}]))))
