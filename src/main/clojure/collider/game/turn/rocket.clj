(ns collider.game.turn.rocket
  "The turn of a firework rocket. It speeds up as it climbs, slides
  along what stops it and goes off when its life runs out or it meets
  a body."
  (:require [collider.game.entity.size :as size]
            [collider.game.out :as out]
            [collider.game.projectile :as projectile]
            [collider.vec :as v]
            [collider.world.blocks.motion :as motion]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(defn- half ^double [] (size/half :firework-rocket))

(defn- height ^double [] (size/height :firework-rocket))

(defn- sped-up [e]
  (let [k (if (:horizontal-collision e) 1.0 1.15)]
    [(* (double (v/x (:vel e))) k) (+ (double (v/y (:vel e))) 0.04)
     (* (double (v/z (:vel e))) k)]))

(defn- moved [world e d]
  (let [stuck (:stuck e)
        m (if stuck (mapv * d stuck) d)
        mv (phys/move (:chunks world) (:pos e) m (half) (height))
        at (phys/pos mv)]
    {:pos at :on-ground (phys/on-ground? mv)
     :horizontal-collision (or (not= (v/x m) (v/x (phys/vel mv)))
                (not= (v/z m) (v/z (phys/vel mv))))
     :stuck (motion/stuck-speed (:chunks world) at (half) (height))}))

(defn- explosion [eid]
  [[:remove-entity eid] (out/all (out/status eid :fireworks))])

(defn- launch [e at]
  (when (zero? (long (:life e 0)))
    [(out/all (out/sound :firework-rocket/launch at 3.0 1.0))]))

(defn step-deltas
  "Returns the deltas of the turn of firework rocket e of eid."
  [world eid e]
  (if (< (v/y (:pos e)) (chunk/void-y world))
    [[:remove-entity eid]]
    (let [left? (projectile/left-owner? world e (:vel e))
          d (sped-up e)
          hit (projectile/clip world eid (assoc e :left-owner? left?) d)
          m (moved world e d)
          [yaw pitch] (projectile/rotated e d)
          life (inc (long (:life e 0)))
          met? (= :entity (:kind hit))]
      (concat
        (when met? (explosion eid))
        (launch e (:pos m))
        (cond met? nil
              (> life (long (:lifetime e 0))) (explosion eid)
              :else [[:merge-entity eid
                      (assoc m :vel d :left-owner? left? :yaw yaw
                             :pitch pitch :life life
                             :age (inc (long (:age e 0))))]])))))
