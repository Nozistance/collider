(ns collider.game.systems.players.motion
  "The speed the server keeps for a player, which the client does not
  send. Every tick it moves on as the speed of a body with no input,
  while the body stays where the client says it is."
  (:require [collider.game.changes :as changes]
            [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.input :as input]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.travel :as travel]
            [collider.game.out :as out]
            [collider.vec :as v]
            [collider.world.phys :as phys]))

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

(defn- stuck-move
  "Returns move [from to d] of player e cut short by the block that
  held it."
  [world e half height [f _ d]]
  (let [m (mapv * d (:stuck e))
        mv (phys/move (:chunks world) f m half height
                      (mobs/step-height :player)
                      (boolean (:on-ground e)) (phys/context e))]
    [f (phys/pos mv) m]))

(defn- packet-moves
  "Returns the moves [from to d] the packets of player eid, e, made
  this tick. A block that held it holds the first of them only."
  [world eid e half height]
  (let [ms (input/moves-of world eid)]
    (if (and (:stuck e) (seq ms))
      (cons (stuck-move world e half height (first ms)) (rest ms))
      ms)))

(defn player-deltas
  "Returns the deltas that move the kept speed of player eid, e, on
  by a tick. A glider hurts itself on the walls it flies into, and a
  ladder takes it off its wings. A body that holds on to a ladder
  falls no further."
  [world [eid e]]
  (let [[half height] (entity/pose-box (:pose e :standing))
        ms (packet-moves world eid e half height)
        e (if (and (:stuck e) (seq ms))
            (assoc e :stuck nil :vel [0.0 0.0 0.0])
            e)
        vel (dead-banded (travel/current-pushed
                           world e (or (:vel e) [0.0 0.0 0.0])
                           half height))
        {:keys [pos stuck flight hit? holds-on? snowed?] moved :vel}
        (travel/body-moved world (assoc e :vel vel) half height ms)
        vel' (if (:flying e)
               [(v/x moved) (* (double (v/y vel)) flying-drag) (v/z moved)]
               moved)]
    (cons [:merge-entity eid
           (cond-> {:vel (mapv double vel') :stuck stuck}
             (= :ladder flight) (assoc :fall-flying false)
             (and holds-on? (pos? (double (:fall e 0.0))))
             (assoc :fall 0.0)
             (not= snowed? (boolean (:in-powder-snow e)))
             (assoc :in-powder-snow (when snowed? true)))]
          (concat (when (and (= :glide flight) hit?)
                    (wall-deltas world eid e pos vel vel'))
                  (when (and (not (:flying e)) (entity/alive? e))
                    (changes/bubble-fx world eid (:pos e)))))))
