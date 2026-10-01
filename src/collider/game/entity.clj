(ns collider.game.entity
  "Entity constructors, saving and loading."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.entity.records :as types]
            [collider.game.entity.size :as size]
            [collider.game.hanging :as hanging]
            [collider.game.mob.mobs :as mobs]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block])
  (:import (clojure.lang PersistentHashMap)
           (collider.game.entity.records Item Mob Orb)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(def thrown-types
  #{:snowball :egg :ender-pearl :splash-potion :lingering-potion
    :experience-bottle})

(defn item
  "Returns a dropped item entity of stack at pos.
  It moves with velocity vel and cannot be picked up for delay ticks."
  ([pos vel stack] (item pos vel stack 10))
  ([pos vel stack delay]
   {:type :item :pos pos :vel vel :yaw 0.0 :pitch 0.0
    :on-ground false :stack stack :age 0 :pickup-delay delay
    :health 5.0}))

(defn pop-velocity
  "Returns the velocity a stack leaves its holder with, drawn from the
  random keys ks."
  [ks]
  (let [r (fn [k] (random/of-key (conj ks k)))]
    [(- (* 0.2 (r :vx)) 0.1) 0.2 (- (* 0.2 (r :vz)) 0.1)]))

(def ^:private mob-fields (Mob/getBasis))

(defn- field [e f] (list '. e (symbol (str "-" f))))

(defmacro ^:private with-extmap [e x]
  (let [o (with-meta (gensym "m") {:tag `Mob})]
    `(let [~o ~e]
       (new Mob ~@(map #(field o %) mob-fields) (meta ~o) ~x))))

(defmacro ^:private extmap [e]
  (field (with-meta e {:tag `Mob}) "__extmap"))

(defn- compact
  "Returns mob e with its few keys beyond its fields in an array map,
  which finds a key without hashing it."
  [e]
  (let [x (extmap e)]
    (if (and (instance? PersistentHashMap x) (<= (count x) 8))
      (with-extmap e (into {} x))
      e)))

(defn- record-of [m]
  (case (:type m)
    :player (types/map->Player m)
    :item (types/map->Item m)
    :experience-orb (types/map->Orb m)
    :tnt (types/map->Tnt m)
    :falling-block (types/map->FallingBlock m)
    :area-effect-cloud (types/map->Cloud m)
    (:painting :item-frame :glow-item-frame) (types/map->Hanging m)
    (:snowball :egg :ender-pearl :splash-potion :lingering-potion
     :experience-bottle)
    (types/map->Projectile m)
    (compact (types/map->Mob m))))

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

(defn pose-box
  "Returns the half width and height of a player in pose."
  [pose]
  (size/type-box :player pose))

(defn pose-eye
  "Returns the eye height of a player in pose."
  ^double [pose]
  (size/pose-eye :player pose))

(defn eye-height
  "Returns how far above its position the entity e looks out."
  ^double [e]
  (size/eye e))

(defn box
  "Returns the half width and the height of entity e."
  [e]
  (size/box e))

(defn- same? [o vs]
  `(and ~@(map (fn [[k s]] `(identical? ~s ~(field o (name k)))) vs)))

(defn- rebuilt [o at]
  `(new Mob ~@(map at mob-fields) (meta ~o) ~(field o "__extmap")))

(defmacro with
  "Returns entity e with the keys of the map kvs set to their values.
  A mob takes them all in one copy and stays itself when it already
  holds them. The keys are fields of Mob."
  [e kvs]
  (assert (every? (set (map keyword mob-fields)) (keys kvs)))
  (let [x (gensym "e")
        o (with-meta (gensym "m") {:tag `Mob})
        vs (into {} (map (fn [[k _]] [k (gensym (name k))])) kvs)
        at (fn [f] (get vs (keyword f) (field o f)))]
    `(let [~x ~e ~@(mapcat (fn [[k v]] [(vs k) v]) kvs)]
       (if (instance? Mob ~x)
         (let [~o ~x] (if ~(same? o vs) ~o ~(rebuilt o at)))
         (assoc ~x ~@(mapcat identity vs))))))

(defn- fields-of [cls]
  (eval (list (symbol (str cls) "getBasis"))))

(defn- filled [e fields]
  (let [put (fn [i f] `(aset ~i ~(field e f)))]
    `(doto (object-array ~(count fields))
       ~@(map-indexed put fields))))

(defn- put-field [a x k v fields]
  (let [set (fn [i f] [(keyword f) `(do (aset ~a ~i ~v) ~x)])]
    `(case ~k
       ~@(apply concat (map-indexed set fields))
       (assoc ~x ~k ~v))))

(defmacro ^:private merger [cls]
  (let [fields (fields-of cls)
        e (with-meta (gensym "e") {:tag cls})
        a (with-meta (gensym "a") {:tag 'objects})
        [m x k v] (map gensym ["m" "x" "k" "v"])
        got (fn [i] `(aget ~a ~i))
        f `(fn [~x ~k ~v] ~(put-field a x k v fields))]
    `(fn [~e ~m]
       (let [~a ~(filled e fields)
             ext# (reduce-kv ~f ~(field e "__extmap") ~m)]
         (new ~cls ~@(map got (range (count fields))) (meta ~e)
              ext#)))))

(def ^:private mob-merged (merger Mob))

(def ^:private item-merged (merger Item))

(def ^:private orb-merged (merger Orb))

(defn merged
  "Returns entity e with the map m merged into it."
  [e m]
  (cond (< (count m) 2) (merge e m)
        (instance? Mob e) (mob-merged e m)
        (instance? Item e) (item-merged e m)
        (instance? Orb e) (orb-merged e m)
        :else (merge e m)))

(defn mob-looked
  "Returns the mob e turned towards what it looks at."
  [e head-yaw pitch look]
  (with e {:head-yaw head-yaw :pitch pitch :look look}))

(defn- plain [v] (if (v/v3? v) (vec v) v))

(defn- kind [type]
  (cond (contains? thrown-types type) :thrown
        (contains? hanging/types type) :hanging
        (#{:item :tnt :falling-block :area-effect-cloud
           :experience-orb} type) type
        :else :mob))

(def ^:private kept
  {:mob [:health :death-time :color :sheared? :sound-variant
         :effects :absorption :fall]
   :item [:stack :age :pickup-delay :health]
   :experience-orb [:value :count :age :health]
   :tnt [:fuse :origin :owner]
   :falling-block [:block :time :fall :hurt :hurt-max]
   :thrown [:owner :left-owner? :stack]
   :hanging [:block-pos :facing :variant :stack :rotation]})

(def ^:private defaults
  {:mob {:death-time 0 :color 0 :sheared? false}
   :item {:age 0 :pickup-delay 0 :health 5.0}
   :experience-orb {:value 0 :count 1 :age 0 :health 5.0}
   :tnt {:fuse 80}
   :falling-block {:time 0}
   :thrown {:left-owner? false}})

(def ^:private fresh
  {:mob {:task nil :no-action 0}
   :thrown {:age 0}})

(def ^:private timers
  [:love-until :baby-until :breed-ready-at :egg-at])

(def ^:private expired {:love-until 0 :breed-ready-at 0})

(defn- base [e]
  (cond-> {:type (:type e) :pos (plain (:pos e)) :vel (plain (:vel e))
           :yaw (:yaw e) :pitch (:pitch e) :on-ground (:on-ground e)}
    (seq (:tags e)) (assoc :tags (:tags e))))

(defn- left [e k ^long tick]
  (when-let [t (get e k)]
    (let [dt (- (long t) tick)] (when (pos? dt) dt))))

(defn- saved-timers [m e tick]
  (reduce (fn [m k]
            (if-let [dt (left e k tick)] (assoc m k dt) m))
          m timers))

(defn timed?
  "Returns true when what a save keeps of e depends on the tick."
  [e]
  (boolean (some #(get e %) timers)))

(defn saved
  "Returns entity e as the data a save keeps at game tick tick.
  The rest starts fresh when loaded."
  [e tick]
  (let [k (kind (:type e))]
    (if (= :area-effect-cloud k)
      (-> (into {} e) (dissoc :track :victims)
          (update :pos plain) (update :vel plain))
      (cond-> (into (base e)
                    (filter (comp some? val))
                    (select-keys e (kept k)))
        (= :mob k) (saved-timers e (long tick))
        (:carrots (:hop e)) (assoc :carrots (:carrots (:hop e)))))))

(defn- still-axis ^double [^double a]
  (if (> (Math/abs a) 10.0) 0.0 a))

(defn- loaded-base [m]
  (cond-> {:pos (or (:pos m) [0.0 0.0 0.0])
           :vel (mapv still-axis (or (:vel m) [0.0 0.0 0.0]))
           :yaw (double (or (:yaw m) 0.0))
           :pitch (double (or (:pitch m) 0.0))
           :on-ground (boolean (:on-ground m))}
    (seq (:tags m)) (assoc :tags (set (:tags m)))))

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
        (cond-> (:carrots m) (assoc :hop {:carrots (:carrots m)}))
        (loaded-timers m (long tick)))))

(defn- kind-extras [e k m tick]
  (case k
    :mob (mob-extras e m tick)
    :falling-block (update e :block #(or % (block/state :sand)))
    :hanging (assoc e :check-at (hanging/first-check-at tick))
    e))

(defn loaded
  "Returns the entity saved as m back at game tick tick, or nil when
  the entity does not survive a load."
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
              (kind-extras k m tick)
              (assoc :born (long tick)))))))

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
  "Returns xd zd, or a tiny side drawn while they are too short,
  as LivingEntity.knockback:1657."
  [tick eid ^double xd ^double zd]
  (loop [i 0 xd xd zd zd]
    (if (< (+ (* xd xd) (* zd zd)) least-turn)
      (recur (inc i) (side-draw tick eid (* 4 i))
             (side-draw tick eid (+ (* 4 i) 2)))
      [xd zd])))

(defn- resisted ^double [e ^double power]
  (let [r (get (attribute/base-values e) :knockback-resistance 0.0)]
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
  "Returns living entity e knocked back with power away from
  direction xd zd, as LivingEntity.knockback:1651."
  [e power xd zd tick eid]
  (let [p (resisted e (double power))]
    (if (<= p 0.0)
      e
      (let [[xd zd] (knock-side tick eid (double xd) (double zd))
            xd (double xd) zd (double zd)
            f (Math/sqrt (+ (* xd xd) (* zd zd)))]
        (assoc e :vel (knock-vel (or (:vel e) [0.0 0.0 0.0])
                                 (:on-ground e) p (/ xd f)
                                 (/ zd f)))))))

(defn rested
  "Returns entity e with its hurt resistance one tick lower, as
  LivingEntity.baseTick counts it down."
  [e]
  (let [r (long (or (:hurt-resist e) 0))]
    (if (pos? r) (assoc e :hurt-resist (dec r)) e)))

(def ^:private generic {:type :generic})

(defn- taken
  "Returns entity e after a hurt from src it takes, as
  LivingEntity.hurtServer:1240-1272 marks the one who caused it and
  the last damage source."
  [e src tick]
  (cond-> e
    (:player? src) (assoc :hurt-by-player tick)
    (and (instance? Mob e) (not (identical? generic src)))
    (assoc :hurt-cause (:type src))))

(defn- f32 ^double [x] (double (float x)))

(defn- lost
  "Returns health after damage, as LivingEntity.actuallyHurt:1979
  sets it in float."
  ^double [health damage]
  (max 0.0 (f32 (- (f32 health) (double damage)))))

(defn- hurt-again [e health amount src tick]
  (let [last-d (f32 (or (:last-damage e) 0.0))
        amount (double amount)]
    (if (> amount last-d)
      (taken (assoc e :health (lost health (f32 (- amount last-d)))
                      :last-damage amount)
             src tick)
      e)))

(defn- knock-dir
  "Returns xd zd of LivingEntity.dealDefaultKnockback:1298: against
  the motion of a projectile, else toward where src came from."
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
  (if (and (instance? Mob e) (not (contains? @unmarked (:type src))))
    (assoc e :hurt-marked? true)
    e))

(defn- hurt-fully [e health amount src tick eid]
  (let [left (lost health amount)]
    (-> (assoc e :health left :last-damage amount
               :hurt-resist max-resist :struck-by src)
        (taken src tick)
        (marked src)
        (knocked-by src tick eid))))

(defn- hurt-item [e ^double health ^double amount]
  (assoc e :health (double (long (- health amount)))))

(defn- quieted [e]
  (if (instance? Mob e) (assoc e :no-action 0) e))

(defn hurt
  "Returns entity e after amount of damage from source src, as
  LivingEntity.hurtServer:1189. A source is a map: :type the damage
  type, :cause and :direct the eids, :from where it knocks from,
  :along the motion of the projectile that knocks, :pos where it
  came from, :player? when a player caused it. A full
  hit leaves the source in :struck-by until it is shown."
  ([e amount] (hurt e amount nil 0 0))
  ([e amount src tick eid]
   (let [health (double (or (:health e) 0.0))
         resist (long (or (:hurt-resist e) 0))
         amount (max 0.0 (f32 amount))
         src (or src generic)]
     (cond
       (not (pos? health)) e
       (contains? #{:item :experience-orb} (:type e))
       (hurt-item e health amount)
       (> resist (/ max-resist 2.0))
       (hurt-again (quieted e) health amount src tick)
       :else (hurt-fully (quieted e) health amount src tick eid)))))

(defn taken?
  "Returns true when entity e took the hurt that made h of it."
  [e h]
  (or (< (double (:health h 0.0)) (double (:health e 0.0)))
      (not (identical? (:struck-by h) (:struck-by e)))))
