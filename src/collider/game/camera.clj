(ns collider.game.camera
  "The entity a spectator looks through, its camera."
  (:require [collider.game.entity :as entity]
            [collider.game.game-mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private still (v/v3 [0.0 0.0 0.0]))

(defn- xyz [p] [(v/x p) (v/y p) (v/z p)])

(defn- placed
  "ServerPlayer.teleportTo in its own level, turned as it stands:
  player e of id eid put at pos."
  [eid e pos]
  (let [yaw (double (:yaw e 0.0)) pitch (double (:pitch e 0.0))]
    [[:teleport eid pos]
     [:merge-entity eid {:head-yaw yaw :vel still}]
     (out/to eid (out/teleport pos yaw pitch))]))

(defn set-deltas
  "ServerPlayer.setCamera: player e of id eid looks through the
  entity of id cam in world, through its own eyes for nil or eid.
  It is put where the camera is and told of it; nil when nothing
  changes."
  [world eid e cam]
  (let [cam (when-not (= cam eid) cam)]
    (when (not= cam (:camera e))
      (let [c (if cam (get-in world [:entities cam]) e)]
        (concat [[:merge-entity eid {:camera cam}]]
                (placed eid e (xyz (:pos c)))
                [(out/to eid (out/camera (or cam eid)))])))))

(defn- alive?
  "Entity.isAlive: not removed, and with health left when it has
  health."
  [e]
  (and e (or (nil? (:health e)) (pos? (double (:health e))))))

(defn follow-deltas
  "ServerPlayer.tick: player e of id eid snaps to its camera, and
  looks through its own eyes again when the camera is gone or the
  player sneaks."
  [world eid e]
  (when-let [cam (:camera e)]
    (let [c (get-in world [:entities cam])]
      (if (alive? c)
        (let [m {:pos (:pos c) :yaw (double (:yaw c 0.0))
                 :pitch (double (:pitch c 0.0))}]
          (cons [:merge-entity eid m]
                (when (:sneaking? e)
                  (set-deltas world eid (merge e m) nil))))
        (set-deltas world eid e nil)))))

(defn- box
  "The half width and height of the pickable entity e, nil for one
  a spectator cannot pick."
  [e]
  (let [t (:type e)]
    (cond
      (= :player t)
      (when-not (game-mode/spectator? e)
        (entity/pose-box (:pose e :standing)))
      (mobs/mob-type? t) (mobs/box-of e)
      (#{:tnt :falling-block} t) [0.49 0.98])))

(def ^:private ^:const border 29999984.0)

(defn- in-border?
  "WorldBorder.isWithinBounds of the default border for the block
  position of e."
  [e]
  (let [x (Math/floor (v/x (:pos e))) z (Math/floor (v/z (:pos e)))]
    (and (>= x (- border)) (< x border)
         (>= z (- border)) (< z border))))

(defn- gap ^double [^double lo ^double hi ^double p]
  (max 0.0 (- lo p) (- p hi)))

(defn- in-range?
  "Player.isWithinEntityInteractionRange with a buffer of three:
  the eye of player p near enough the box of entity t."
  [p t [half h]]
  (let [[ex ey ez] (xyz (:pos p))
        ey (+ (double ey) (entity/eye-height p))
        [x y z] (xyz (:pos t))
        half (double half)
        dx (gap (- x half) (+ x half) ex)
        dy (gap y (+ y (double h)) ey)
        dz (gap (- z half) (+ z half) ez)
        r (+ 3.0 game-mode/entity-range)]
    (< (+ (* dx dx) (* dy dy) (* dz dz)) (* r r))))

(defn spectate-deltas
  "ServerGamePacketListenerImpl.handleSpectatorAction: spectator p
  of id eid picks entity tid as its camera."
  [world eid tid]
  (let [p (get-in world [:entities eid])
        t (when tid (get-in world [:entities tid]))]
    (when (and (game-mode/spectator? p) t (in-border? t))
      (when-let [b (box t)]
        (when (in-range? p t b)
          (set-deltas world eid p tid))))))
