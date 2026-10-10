(ns collider.game.systems.players.motion
  "The speed the server keeps for a player, which the client does
  not send. A knock sets it and every tick wears it down as a body
  with no input moving through air or along the ground."
  (:require [collider.num :as num]
            [collider.vec :as v]
            [collider.world.blocks.motion :as motion]))

(set! *warn-on-reflection* true)

(def ^:private ^:const still-sq 9.0E-6)

(def ^:private ^:const least-y 0.003)

(def ^:private ^:const gravity 0.08)

(def ^:private ^:const air-inertia (double (float 0.91)))

(def ^:private ^:const vertical-drag (double (float 0.98)))

(def ^:private ^:const flying-drag 0.6)

(defn- inertia ^double [world e]
  (if (:on-ground e)
    (let [st (motion/below-state (:chunks world) (:pos e) (:support e))]
      (num/f32 (* (motion/friction st) air-inertia)))
    air-inertia))

(defn worn
  "Returns velocity vel of player e after one tick of wear in world."
  [world e vel]
  (let [x (double (v/x vel)) y (double (v/y vel)) z (double (v/z vel))
        still? (< (+ (* x x) (* z z)) still-sq)
        x (if still? 0.0 x) z (if still? 0.0 z)
        y (if (< (Math/abs y) least-y) 0.0 y)
        y (if (and (:on-ground e) (neg? y)) 0.0 y)
        k (inertia world e)]
    [(* x k)
     (if (:flying e) (* y flying-drag) (* (- y gravity) vertical-drag))
     (* z k)]))

(defn player-deltas
  "Returns the delta that wears the kept speed of player eid, e."
  [world [eid e]]
  (when-let [vel (:vel e)]
    (let [w (worn world e vel)]
      (when-not (= w vel)
        [[:merge-entity eid {:vel w}]]))))
