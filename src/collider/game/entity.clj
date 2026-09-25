(ns collider.game.entity
  "Entity records and their constructors."
  (:require [collider.game.mob.mobs :as mobs]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(defrecord Mob [pos vel on-ground yaw pitch head-yaw walked wet?
                jump-cd task follow look no-action say-tick health
                hurt-resist last-damage death-time health-sent
                panic-until baby-until love-until breed-ready-at
                tempt-cooldown-until type color sheared? track])

(defrecord Player [type name uuid pos yaw pitch on-ground client-vel
                   tp-target chunk-pos sent-chunks needs-spawn?
                   tracking track health hurt-resist last-damage
                   death-time health-sent inventory held-slot
                   using-item? sneaking? sprinting? skin-parts
                   view-distance chunk-view ping
                   keepalive-at keepalive-pending?])

(defrecord Item [type pos vel yaw pitch on-ground stack age
                 pickup-delay needs-sync? track])

(defrecord Tnt [type pos vel yaw pitch on-ground origin fuse kb
                track])

(defrecord FallingBlock
  [type pos vel yaw pitch on-ground block start time track])

(defrecord Projectile [type pos vel yaw pitch on-ground stack owner
                       age left-owner? track])

(defrecord Cloud [type pos vel yaw pitch on-ground radius color
                  waiting? age duration wait-time radius-per-tick
                  radius-on-use victims track])

(def thrown-types
  #{:snowball :egg :ender-pearl :splash-potion :lingering-potion})

(defn item
  "Returns a dropped item entity of stack at pos. It moves with
  velocity vel and cannot be picked up for delay ticks."
  ([pos vel stack] (item pos vel stack 10))
  ([pos vel stack delay]
   {:type :item :pos pos :vel vel :yaw 0.0 :pitch 0.0
    :on-ground false :stack stack :age 0 :pickup-delay delay
    :health 5.0}))

(defn pop-velocity
  "Returns the velocity a stack leaves its holder with, drawn from
  the random keys ks."
  [ks]
  (let [r (fn [k] (random/of-key (conj ks k)))]
    [(- (* 0.2 (r :vx)) 0.1) 0.2 (- (* 0.2 (r :vz)) 0.1)]))

(defn- record-of [m]
  (case (:type m)
    :player (map->Player m)
    :item (map->Item m)
    :tnt (map->Tnt m)
    :falling-block (map->FallingBlock m)
    :area-effect-cloud (map->Cloud m)
    (:snowball :egg :ender-pearl :splash-potion :lingering-potion)
    (map->Projectile m)
    (map->Mob m)))

(defn of
  "Returns the entity m as the record its type calls for."
  [m]
  (if (record? m)
    m
    (record-of (cond-> m
                 (:pos m) (assoc :pos (v/v3 (:pos m)))
                 (:vel m) (assoc :vel (v/v3 (:vel m)))))))

(defn uuid-of
  "Returns the uuid of entity e, whose id is eid."
  [eid e]
  (or (:uuid e) (UUID. (long eid) (long eid))))

(defn eye-height
  "Returns how far above its position the entity e looks out."
  ^double [e]
  (case (:type e)
    :player 1.62
    :tnt 0.0
    :falling-block 0.0
    :item 0.21
    :area-effect-cloud 0.425
    (:snowball :egg :ender-pearl :splash-potion
     :lingering-potion) 0.2125
    1.19))

(defn mob-moved
  "Returns the mob e after a step of its own movement."
  [e pos vel on-ground yaw wet? jump-cd]
  (assoc e :pos pos :vel vel :on-ground on-ground :yaw yaw
         :wet? wet? :jump-cd jump-cd))

(defn mob-looked
  "Returns the mob e turned towards what it looks at."
  [e head-yaw pitch look]
  (assoc e :head-yaw head-yaw :pitch pitch :look look))

(defn- plain [v] (if (v/v3? v) (vec v) v))

(def ^:private thrown
  #{:snowball :egg :ender-pearl :splash-potion :lingering-potion})

(defn- kind [type]
  (cond (contains? thrown type) :thrown
        (#{:item :tnt :falling-block :area-effect-cloud} type) type
        :else :mob))

(def ^:private kept
  {:mob [:health :death-time :color :sheared? :sound-variant]
   :item [:stack :age :pickup-delay :health]
   :tnt [:fuse :origin]
   :falling-block [:block :time]
   :thrown [:owner :left-owner? :stack]})

(def ^:private defaults
  {:mob {:death-time 0 :color 0 :sheared? false}
   :item {:age 0 :pickup-delay 0 :health 5.0}
   :tnt {:fuse 80}
   :falling-block {:time 0}
   :thrown {:left-owner? false}})

(def ^:private fresh
  {:mob {:task nil :no-action 0}
   :thrown {:age 0}})

(def ^:private timers [:love-until :baby-until :breed-ready-at])

(def ^:private expired {:love-until 0 :breed-ready-at 0})

(defn- base [e]
  {:type (:type e) :pos (plain (:pos e)) :vel (plain (:vel e))
   :yaw (:yaw e) :pitch (:pitch e) :on-ground (:on-ground e)})

(defn- left [e k ^long tick]
  (when-let [t (get e k)]
    (let [dt (- (long t) tick)] (when (pos? dt) dt))))

(defn- saved-timers [m e tick]
  (reduce (fn [m k]
            (if-let [dt (left e k tick)] (assoc m k dt) m))
          m timers))

(defn- with-knockback [e]
  (if-let [kb (:kb e)] (update e :vel v/+ kb) e))

(defn saved
  "Returns entity e as the plain data vanilla keeps across a save
  at game tick tick. The rest restarts fresh when loaded."
  [e tick]
  (let [k (kind (:type e))]
    (if (= :area-effect-cloud k)
      (-> (into {} e) (dissoc :track :victims)
          (update :pos plain) (update :vel plain))
      (cond-> (into (base (with-knockback e))
                    (filter (comp some? val))
                    (select-keys e (kept k)))
        (= :mob k) (saved-timers e (long tick))))))

(defn- still-axis ^double [^double a]
  (if (> (Math/abs a) 10.0) 0.0 a))

(defn- loaded-base [m]
  {:pos (or (:pos m) [0.0 0.0 0.0])
   :vel (mapv still-axis (or (:vel m) [0.0 0.0 0.0]))
   :yaw (double (or (:yaw m) 0.0))
   :pitch (double (or (:pitch m) 0.0))
   :on-ground (boolean (:on-ground m))})

(defn- loaded-timers [e m ^long tick]
  (reduce (fn [e k]
            (if-let [dt (get m k)]
              (assoc e k (+ tick (long dt)))
              (cond-> e (contains? expired k) (assoc k (expired k)))))
          e timers))

(defn- mob-extras [e m tick]
  (let [top (mobs/max-health (:type m))]
    (-> e
        (assoc :head-yaw (:yaw e) :health-sent top
               :health (or (:health m) top))
        (loaded-timers m (long tick)))))

(defn- kind-extras [e k m tick]
  (case k
    :mob (mob-extras e m tick)
    :falling-block (update e :block #(or % (block/state :sand)))
    e))

(defn loaded
  "Returns the entity saved as m back at game tick tick, or nil
  when vanilla discards it on load."
  [m tick]
  (let [k (kind (:type m))]
    (cond
      (= :area-effect-cloud k)
      (of (merge (dissoc m :victims) (loaded-base m)))
      (and (= :item k) (not (pos? (long (:count (:stack m) 0)))))
      nil
      :else
      (of (-> (merge (defaults k) (select-keys m (kept k)))
              (merge (loaded-base m) (fresh k) {:type (:type m)})
              (kind-extras k m tick))))))
