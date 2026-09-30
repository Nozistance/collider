(ns collider.game.entity
  "Entity constructors, saving and loading."
  (:require [collider.game.entity.records :as types]
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

(def pose-box
  "The half width and height of a player in each pose."
  {:standing [0.3 1.8] :crouching [0.3 1.5]
   :swimming [0.3 0.6] :sleeping [0.1 0.2]})

(def pose-eyes
  {:standing 1.62 :crouching 1.27 :swimming 0.4 :sleeping 0.2})

(defn eye-height
  "Returns how far above its position the entity e looks out."
  ^double [e]
  (if (= :player (:type e))
    (double (pose-eyes (:pose e :standing)))
    (size/eye e)))

(defn box
  "Returns the half width and the height of entity e."
  [e]
  (if (= :player (:type e))
    (pose-box (:pose e :standing))
    (size/box e)))

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
         :effects :absorption]
   :item [:stack :age :pickup-delay :health]
   :experience-orb [:value :count :age :health]
   :tnt [:fuse :origin]
   :falling-block [:block :time]
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

(defn- with-knockback [e]
  (if-let [kb (:kb e)] (update e :vel v/+ kb) e))

(defn saved
  "Returns entity e as the data a save keeps at game tick tick.
  The rest starts fresh when loaded."
  [e tick]
  (let [k (kind (:type e))]
    (if (= :area-effect-cloud k)
      (-> (into {} e) (dissoc :track :victims)
          (update :pos plain) (update :vel plain))
      (cond-> (into (base (with-knockback e))
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

(defn- knock-back [e ^double dx ^double dz]
  (let [f (Math/sqrt (+ (* dx dx) (* dz dz)))
        v (or (:vel e) [0.0 0.0 0.0])]
    (if (zero? f)
      e
      (assoc e :vel [(- (/ (v/x v) 2.0) (* (/ dx f) 0.4))
                     (min 0.4 (+ (/ (v/y v) 2.0) 0.4))
                     (- (/ (v/z v) 2.0) (* (/ dz f) 0.4))]))))

(defn rested
  "Returns entity e with its hurt resistance one tick lower, as
  LivingEntity.baseTick counts it down."
  [e]
  (let [r (long (or (:hurt-resist e) 0))]
    (if (pos? r) (assoc e :hurt-resist (dec r)) e)))

(defn- hurt-again [e ^double health ^double amount]
  (let [last-d (double (or (:last-damage e) 0.0))]
    (if (> amount last-d)
      (assoc e :health (- health (- amount last-d))
               :last-damage amount)
      e)))

(defn- hurt-fully [e health amount dx dz]
  (let [left (max 0.0 (- (double health) (double amount)))]
    (cond-> (assoc e :health left
                     :last-damage amount
                     :hurt-resist max-resist)
            dx (knock-back (double dx) (double dz)))))

(defn- hurt-item [e ^double health ^double amount]
  (assoc e :health (double (long (- health amount)))))

(defn hurt
  "Returns entity e after amount of damage.
  It is knocked back from direction dx dz when given."
  ([e ^double amount] (hurt e amount nil nil))
  ([e ^double amount dx dz]
   (let [health (double (or (:health e) 0.0))
         resist (long (or (:hurt-resist e) 0))]
     (cond
       (not (pos? health)) e
       (contains? #{:item :experience-orb} (:type e))
       (hurt-item e health amount)
       (> resist (/ max-resist 2.0)) (hurt-again e health amount)
       :else (hurt-fully e health amount dx dz)))))
