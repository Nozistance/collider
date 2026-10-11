(ns collider.game.systems.players.motion
  "The speed the server keeps for a player, which the client does not
  send. Every tick it moves on as the speed of a body with no input,
  while the body stays where the client says it is."
  (:require [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.mob.travel :as travel]
            [collider.game.out :as out]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const still-sq 9.0E-6)

(def ^:private ^:const least-y 0.003)

(def ^:private ^:const flying-drag 0.6)

(defn- dead-banded [vel]
  (let [x (double (v/x vel)) y (double (v/y vel)) z (double (v/z vel))
        still? (< (+ (* x x) (* z z)) still-sq)]
    [(if still? 0.0 x) (if (< (Math/abs y) least-y) 0.0 y) (if still? 0.0 z)]))

(defn- flat-speed ^double [vel]
  (let [x (double (v/x vel)) z (double (v/z vel))]
    (Math/sqrt (+ (* x x) (* z z)))))

(def ^:private fly-into-wall {:type :fly-into-wall})

(defn- wall-deltas
  "Returns the deltas of player eid, e, flying into a wall at pos,
  slowed from speed vel to vel'."
  [world eid e pos vel vel']
  (let [lost (- (flat-speed vel) (flat-speed vel'))
        n (float (- (* lost 10.0) 3.0))]
    (when (pos? n)
      (let [kind (if (> (long n) 4) :entity.player.big-fall
                     :entity.player.small-fall)
            fx (out/sound kind pos 1.0 1.0 :players)]
        (cons (out/except eid fx)
              (hurt/damage-deltas world eid e (double n)
                                  fly-into-wall))))))

(defn player-deltas
  "Returns the deltas that move the kept speed of player eid, e, on
  by a tick. A glider hurts itself on the walls it flies into, and a
  ladder takes it off its wings."
  [world [eid e]]
  (let [vel (dead-banded (or (:vel e) [0.0 0.0 0.0]))
        [half height] (entity/pose-box (:pose e :standing))
        [pos moved _ stuck flight hit?]
        (travel/body-moved world (assoc e :vel vel) half height)
        vel' (if (:flying e)
               [(v/x moved) (* (double (v/y vel)) flying-drag) (v/z moved)]
               moved)]
    (cond-> [[:merge-entity eid
              {:vel (mapv double vel') :stuck stuck}]]
      (= :ladder flight)
      (conj [:merge-entity eid {:fall-flying false}])
      (and (= :glide flight) hit?)
      (into (wall-deltas world eid e pos vel vel')))))
