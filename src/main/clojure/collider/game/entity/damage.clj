(ns collider.game.entity.damage
  "The hurt and the knockback a living entity takes, and the resistance
  that holds off the next hurt."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.combat :as combat]
            [collider.game.enchantment :as enchantment]
            [collider.game.entity :as entity]
            [collider.game.mob.mobs :as mobs]
            [collider.game.slots :as slots]
            [collider.game.stack :as stack]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v])
  (:import (collider.game.entity.records Mob)))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-resist 20)

(def ^:private ^:table unknocked
  (delay (set (data/tag-values "damage_type" "no_knockback"))))

(def ^:private ^:table unmarked
  (delay (set (data/tag-values "damage_type" "no_impact"))))

(def ^:private ^:const knock-power (double (float 0.4)))

(def ^:private ^:const least-turn (double (float 1.0E-5)))

(def ^:private ^:const knock-key 0x6b6e6f63)

(defn- side-draw ^double [^long tick ^long eid ^long k]
  (* 0.01 (- (random/of-longs tick eid (+ knock-key k))
             (random/of-longs tick eid (+ knock-key k 1)))))

(defn- knock-side
  "Returns xd zd, or a tiny random side push while they are too
  short."
  [tick eid ^double xd ^double zd]
  (loop [i 0 xd xd zd zd]
    (if (< (+ (* xd xd) (* zd zd)) least-turn)
      (recur (inc i) (side-draw tick eid (* 4 i))
             (side-draw tick eid (+ (* 4 i) 2)))
      [xd zd])))

(defn- resisted ^double [e ^double power]
  (let [r (attribute/value e (:effects e) :knockback-resistance)]
    (* power (- 1.0 (double r)))))

(defn- knock-vel
  "Returns velocity v halved and pushed away along unit xd zd with
  power p, lifted only on ground."
  [v ground? p xd zd]
  (let [vy (v/y v) p (double p)]
    [(- (/ (v/x v) 2.0) (* (double xd) p))
     (if ground? (min 0.4 (+ (/ vy 2.0) p)) vy)
     (- (/ (v/z v) 2.0) (* (double zd) p))]))

(defn knocked
  "Returns living entity e knocked back with power, away from
  direction xd zd."
  [e power xd zd tick eid]
  (let [p (resisted e (double power))]
    (if (<= p 0.0)
      e
      (let [[xd zd] (knock-side tick eid (double xd) (double zd))
            xd (double xd) zd (double zd)
            f (Math/sqrt (+ (* xd xd) (* zd zd)))
            vel (or (:vel e) [0.0 0.0 0.0])
            ground? (:on-ground e)]
        (assoc e :vel (knock-vel vel ground? p (/ xd f) (/ zd f)))))))

(defn rested
  "Returns entity e with its hurt resistance one tick lower."
  [e]
  (let [r (long (or (:hurt-resist e) 0))]
    (if (pos? r) (assoc e :hurt-resist (dec r)) e)))

(def ^:private generic {:type :generic})

(def ^:private ^:const player-memory 100)

(defn- taken
  [e src tick]
  (cond-> e
    (:player? src)
    (assoc :hurt-by-player-until (+ (long tick) player-memory)
           :last-hurt-by-player (:uuid (:attacker src)))
    (instance? Mob e) (assoc :last-hurt [src tick])
    (and (instance? Mob e) (not (identical? generic src)))
    (assoc :hurt-cause (:type src))
    (and (instance? Mob e) (not (pos? (double (:health e)))))
    (assoc :killed-by src)))

(defn- lost
  "Returns health after damage, in float precision."
  ^double [health damage]
  (max 0.0 (num/f32 (- (num/f32 health) (double damage)))))

(defn- counted
  "Returns entity e that counts one more hurt to show, when damage
  took health."
  [e ^double damage]
  (if (zero? damage)
    e
    (assoc e :hurts (inc (long (or (:hurts e) 0))))))

(defn- reacted [e src tick]
  (if-let [f (:hurt-reaction (mobs/types (:type e)))]
    (f e src tick (entity/living-attacker? src))
    e))

(defn- breed-taken ^double [e ^double amount]
  (if-let [f (:hurt-taken (mobs/types (:type e)))]
    (f e amount)
    amount))

(def ^:private armor-order [:feet :legs :chest :head])

(defn- wear-piece [e src n tick eid k]
  (let [slot (slots/armor k)
        s (get-in e [:inventory slot])
        fire? (and (combat/tagged? (:type src) "is_fire")
                   (= "is_fire" (data/resists (:item s))))]
    (if (and s (stack/damageable? s) (not fire?))
      (let [roll #(random/of-key tick eid [:armor slot %])
            worn (+ (stack/damage s) (enchantment/item-damage s n roll))]
        (if (>= worn (stack/max-damage s))
          (-> (update e :inventory dissoc slot)
              (update :broken (fnil conj []) [k (:item s)]))
          (assoc-in e [:inventory slot] (stack/with-damage s worn))))
      e)))

(defn- armor-worn
  "Returns player e with the armor it wears worn by a hit of amount
  from src, a quarter of it, at least 1."
  [e src amount tick eid]
  (if (and (entity/player? e) (not (combat/tagged? (:type src) "bypasses_armor")))
    (let [n (max 1 (long (/ (double amount) 4.0)))]
      (reduce #(wear-piece %1 src n tick eid %2) e armor-order))
    e))

(defn- suffered
  "Returns e after amount from src reaches it past its armor,
  resistance, protection and absorption, with the damage its health
  took."
  [e health amount src tick eid]
  (let [e (armor-worn e src amount tick eid)
        d (combat/damage-after e src (double amount))
        abs (double (or (:absorption e) 0.0))
        left (max 0.0 (num/f32 (- d abs)))
        e (cond-> e
            (pos? abs) (assoc :absorption (num/f32 (- abs (- d left)))))]
    [(assoc e :health (lost health left)) left]))

(defn- hurt-again [e health amount src tick]
  (let [last-d (num/f32 (or (:last-damage e) 0.0))
        amount (double amount)
        more (num/f32 (- amount last-d))]
    (if (> amount last-d)
      (-> (let [[e lost-hp] (suffered e health more src tick nil)]
            (counted (assoc e :last-damage amount) lost-hp))
          (taken src tick)
          (reacted src tick))
      e)))

(defn- knock-dir
  "Returns the knockback direction, against the motion of a
  projectile or toward where src came from."
  [e src]
  (let [m (:along src) p (:from src) pos (:pos e)]
    (cond m [(- (v/x m)) (- (v/z m))]
          p [(- (v/x p) (v/x pos)) (- (v/z p) (v/z pos))]
          :else [0.0 0.0])))

(defn- knocked-by [e src tick eid]
  (if (contains? @unknocked (:type src))
    e
    (let [[xd zd] (knock-dir e src)]
      (knocked e knock-power xd zd tick eid))))

(defn- marked [e src]
  (if-not (contains? @unmarked (:type src))
    (assoc e :hurt-marked? true)
    e))

(defn- hurt-fully [e health amount src tick eid]
  (let [[e lost-hp] (suffered e health amount src tick eid)]
    (-> (assoc e :last-damage amount
               :hurt-resist max-resist :struck-by src)
        (counted lost-hp)
        (taken src tick)
        (reacted src tick)
        (marked src)
        (knocked-by src tick eid))))

(defn- hurt-item [e ^double health ^double amount]
  (assoc e :health (double (long (- health amount)))))

(defn- quieted [e]
  (if (instance? Mob e) (assoc e :no-action 0) e))

(defn- hurt-living [e health amount src tick eid]
  (if (> (long (or (:hurt-resist e) 0)) (/ max-resist 2.0))
    (hurt-again (quieted e) health amount src tick)
    (hurt-fully (quieted e) health amount src tick eid)))

(defn hurt
  "Returns entity e after amount of damage from source src.
  A full hit keeps src in :struck-by until it is shown."
  ([e amount] (hurt e amount nil 0 0))
  ([e amount src tick eid]
   (let [health (double (or (:health e) 0.0))
         amount (max 0.0 (breed-taken e (num/f32 amount)))]
     (cond
       (not (pos? health)) e
       (contains? #{:item :experience-orb} (:type e))
       (hurt-item e health amount)
       :else
       (hurt-living e health amount (or src generic) tick eid)))))

(defn taken?
  "Returns true when h, entity e after a hurt, shows a hit."
  [e h]
  (or (< (double (:health h 0.0)) (double (:health e 0.0)))
      (not (identical? (:struck-by h) (:struck-by e)))))
