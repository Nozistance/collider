(ns collider.game.entity

  (:require [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defrecord Mob [pos vel on-ground yaw pitch head-yaw walked wet? jump-cd
   task pending look wake-tick say-tick
   health hurt-resist last-damage death-time health-sent panic-until
   baby-until love-until breed-ready-at tempt-cooldown-until
   type color track])

(defrecord Player [type name uuid pos yaw pitch on-ground client-vel tp-target
   chunk-pos sent-chunks needs-spawn? tracking track
   health hurt-resist last-damage death-time health-sent
   inventory held-slot using-item? sneaking? sprinting? skin-parts
   ping last-echo-tick])

(defrecord Item [type pos vel yaw pitch on-ground stack age pickup-delay track])

(defrecord Tnt [type pos vel yaw pitch on-ground origin fuse kb track])

(defn of [m]
  (if (record? m)
    m
    (let [m (cond-> m
              (:pos m) (assoc :pos (v/v3 (:pos m)))
              (:vel m) (assoc :vel (v/v3 (:vel m))))]
     (case (:type m)
      :player (map->Player m)
      :item   (map->Item m)
      :tnt    (map->Tnt m)
      (map->Mob m)))))

(defn eye-height ^double [e]
  (case (:type e)
    :player 1.62
    :tnt    0.0
    :item   0.21
    1.19))

(defn mob-moved
  ^Mob [^Mob e pos vel on-ground yaw wet? jump-cd]
  (Mob. pos vel on-ground yaw (.-pitch e) (.-head-yaw e) (.-walked e) wet? jump-cd
        (.-task e) (.-pending e) (.-look e) (.-wake-tick e) (.-say-tick e)
        (.-health e) (.-hurt-resist e) (.-last-damage e) (.-death-time e) (.-health-sent e) (.-panic-until e)
        (.-baby-until e) (.-love-until e) (.-breed-ready-at e) (.-tempt-cooldown-until e)
        (.-type e) (.-color e) (.-track e)
        (.-__meta e) (.-__extmap e)))

(defn mob-looked
  ^Mob [^Mob e head-yaw pitch look]
  (Mob. (.-pos e) (.-vel e) (.-on-ground e) (.-yaw e) pitch head-yaw (.-walked e) (.-wet? e) (.-jump-cd e)
        (.-task e) (.-pending e) look (.-wake-tick e) (.-say-tick e)
        (.-health e) (.-hurt-resist e) (.-last-damage e) (.-death-time e) (.-health-sent e) (.-panic-until e)
        (.-baby-until e) (.-love-until e) (.-breed-ready-at e) (.-tempt-cooldown-until e)
        (.-type e) (.-color e) (.-track e)
        (.-__meta e) (.-__extmap e)))
