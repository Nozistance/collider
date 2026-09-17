(ns collider.game.entity
  "Entity records and their constructors."
  (:require [collider.random :as random]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defrecord Mob [pos vel on-ground yaw pitch head-yaw walked wet? jump-cd
                task follow look no-action say-tick
                health hurt-resist last-damage death-time health-sent panic-until
                baby-until love-until breed-ready-at tempt-cooldown-until
                type color sheared? track])

(defrecord Player [type name uuid pos yaw pitch on-ground client-vel tp-target
                   chunk-pos sent-chunks needs-spawn? tracking track
                   health hurt-resist last-damage death-time health-sent
                   inventory held-slot using-item? sneaking? sprinting? skin-parts
                   ping keepalive-at keepalive-pending?])

(defrecord Item [type pos vel yaw pitch on-ground stack age pickup-delay needs-sync? track])

(defrecord Tnt [type pos vel yaw pitch on-ground origin fuse kb track])

(defrecord FallingBlock [type pos vel yaw pitch on-ground block start time track])

(defn item
  "Returns a dropped item entity of stack at pos with velocity vel and a pickup
   delay in ticks."
  ([pos vel stack] (item pos vel stack 10))
  ([pos vel stack delay]
   {:type  :item :pos pos :vel vel :yaw 0.0 :pitch 0.0 :on-ground false
    :stack stack :age 0 :pickup-delay delay :health 5.0}))

(defn pop-velocity [ks]
  (let [r (fn [k] (random/of-key (conj ks k)))]
    [(- (* 0.2 (r :vx)) 0.1) 0.2 (- (* 0.2 (r :vz)) 0.1)]))

(defn of [m]
  (if (record? m)
    m
    (let [m (cond-> m
                    (:pos m) (assoc :pos (v/v3 (:pos m)))
                    (:vel m) (assoc :vel (v/v3 (:vel m))))]
      (case (:type m)
        :player (map->Player m)
        :item (map->Item m)
        :tnt (map->Tnt m)
        :falling-block (map->FallingBlock m)
        (map->Mob m)))))

(defn eye-height ^double [e]
  (case (:type e)
    :player 1.62
    :tnt 0.0
    :falling-block 0.0
    :item 0.21
    1.19))

(defn mob-moved [e pos vel on-ground yaw wet? jump-cd]
  (assoc e :pos pos :vel vel :on-ground on-ground :yaw yaw :wet? wet? :jump-cd jump-cd))

(defn mob-looked [e head-yaw pitch look]
  (assoc e :head-yaw head-yaw :pitch pitch :look look))
