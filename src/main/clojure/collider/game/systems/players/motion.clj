(ns collider.game.systems.players.motion
  "The speed the server keeps for a player, which the client does not
  send. Every tick it moves on as the speed of a body with no input,
  while the body stays where the client says it is."
  (:require [collider.game.entity :as entity]
            [collider.game.mob.travel :as travel]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const still-sq 9.0E-6)

(def ^:private ^:const least-y 0.003)

(def ^:private ^:const flying-drag 0.6)

(defn- dead-banded [vel]
  (let [x (double (v/x vel)) y (double (v/y vel)) z (double (v/z vel))
        still? (< (+ (* x x) (* z z)) still-sq)]
    [(if still? 0.0 x) (if (< (Math/abs y) least-y) 0.0 y) (if still? 0.0 z)]))

(defn player-deltas
  "Returns the delta that moves the kept speed of player eid, e, on by
  a tick."
  [world [eid e]]
  (let [vel (dead-banded (or (:vel e) [0.0 0.0 0.0]))
        [half height] (entity/pose-box (:pose e :standing))
        [_ moved _ stuck] (travel/body-moved world (assoc e :vel vel) half height)
        vel' (if (:flying e)
               [(v/x moved) (* (double (v/y vel)) flying-drag) (v/z moved)]
               moved)]
    [[:merge-entity eid {:vel (mapv double vel') :stuck stuck}]]))
