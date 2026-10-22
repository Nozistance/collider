(ns collider.game.jump
  "A player jumping off the ground, as the server sees it from the
  moves the client sends."
  (:require [collider.math :as math]
            [collider.vec :as v]
            [collider.world.blocks.motion :as motion]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const strength (double (float 0.42)))

(def ^:private ^:const boost (double (float 0.1)))

(def ^:private ^:const least (double (float 1.0E-5)))

(def ^:private ^:const sprint-push 0.2)

(defn- block-factor ^double [chunks e]
  (let [p (:pos e)
        feet (chunk/at chunks [(long (Math/floor (v/x p))) (long (Math/floor (v/y p)))
                               (long (Math/floor (v/z p)))])
        here (motion/jump-factor feet)]
    (if (== here 1.0)
      (motion/jump-factor (motion/below-state chunks p (:support e)))
      here)))

(defn- boost-power ^double [e]
  (if-let [b (get (:effects e) :jump-boost)]
    (math/fmul boost (double (float (inc (long (:amplifier b))))))
    0.0))

(defn- power ^double [world e]
  (double (float (+ (math/fmul strength (block-factor (:chunks world) e))
                    (boost-power e)))))

(defn jumped
  "Returns speed vel of player e as it jumps off the ground, a
  sprinter pushed forward too."
  [world e vel]
  (let [p (power world e)]
    (if (<= p least)
      vel
      (let [v [(v/x vel) (Math/max p (double (v/y vel))) (v/z vel)]]
        (if (:sprinting? e)
          (let [a (math/f32 (* (math/f32 (double (:yaw e 0.0))) math/deg->rad))]
            [(+ (double (v/x v)) (* (- (math/sin a)) sprint-push))
             (v/y v)
             (+ (double (v/z v)) (* (math/cos a) sprint-push))])
          v)))))
