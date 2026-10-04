(ns collider.game.mob.sensor
  "Sensors of mobs with a brain."
  (:require [collider.game.entity :as entity]
            [collider.game.level :as level]
            [collider.game.mode :as game-mode]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.brain :as b]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.env.difficulty :as difficulty]))

(set! *warn-on-reflection* true)

(def ^:const default-rate 20)

(def ^:private ^:const hurt-memory 40)

(def ^:private ^:const reach-margin 2.0)

(defn first-sense
  "Returns the tick a sensor first senses on when its brain first
  thinks at tick t and the sensor waits p more thinks."
  ^long [t ^long p]
  (+ (long t) (max p 1) -1))

(defn phases
  "Returns the first tick of each of sensors for mob eid whose brain
  first thinks at tick t. Each sensor waits less than its rate."
  [sensors t eid]
  (vec (map-indexed
         (fn [i s]
           (let [r (random/rnd t eid :sensor-phase i)]
             (first-sense t (random/below r (:rate s)))))
         sensors)))

(defn- due? [^long start ^long rate ^long t]
  (and (>= t start) (zero? (rem (- t start) rate))))

(defn sensing
  "Returns the sense part of a brain that runs sensors in order, each
  on the ticks of its phase in the brain of the mob."
  [sensors]
  (let [ss (vec sensors)]
    (fn [w eid e t]
      (let [ph (:phase (:brain e))]
        (reduce-kv (fn [e i s]
                     (if (due? (ph i) (:rate s) t)
                       ((:tick s) w eid e t)
                       e))
                   e ss)))))

(defn with-sensors
  "Returns brain spec with the sense part of sensors and the memories
  they require known."
  [spec sensors]
  (-> (assoc spec :sense (sensing sensors) :sensors (vec sensors))
      (update :memories (fnil into []) (mapcat :requires sensors))))

(defn fresh
  "Returns the brain of a new mob eid of breed b with sensors, whose
  brain first thinks at tick t, with the memories mems."
  ([b sensors t eid] (fresh b sensors t eid nil))
  ([b sensors t eid mems]
   (assoc (b/fresh b mems) :phase (phases sensors t eid))))

(defn follow-range
  "Returns the follow range of mob e with the bonus it drew at spawn."
  ^double [e]
  (let [r (mobs/attribute (:type e) :follow-range)]
    (+ r (* r (double (or (:follow-bonus e) 0.0))))))

(defn- remembered [e k v] (b/remember e k v b/forever))

(defn- by-distance [e entries]
  (let [p (:pos e)]
    (sort-by (fn [[oid o]] [(v/dist-sq p (:pos o)) oid]) entries)))

(defn- within [e [ow oh] o]
  (let [[w h] (entity/box e) r (follow-range e)
        p (:pos e) q (:pos o) x (v/x p) y (v/y p) z (v/z p)
        ox (v/x q) oy (v/y q) oz (v/z q)]
    (and (< (- (- x w) r) (+ ox ow)) (> (+ (+ x w) r) (- ox ow))
         (< (- y r) (+ oy oh)) (> (+ (+ y h) r) oy)
         (< (- (- z w) r) (+ oz ow)) (> (+ (+ z w) r) (- oz ow)))))

(defn- living-near [world eid e]
  (let [reach (+ (double (first (entity/box e))) (follow-range e)
                 reach-margin)
        near? (fn [[oid o]]
                (and (not= oid eid) (entity/living? o)
                     (entity/alive? o) (within e (entity/box o) o)))]
    (->> (sense/around world (:pos e) reach)
         (filter near?)
         (by-distance e)
         (mapv first))))

(defn- in-range? [e t oid o]
  (let [r (follow-range e)]
    (if (= oid (b/recall e :attack-target t))
      (<= (v/dist-sq (:pos e) (:pos o)) (let [d (max r 2.0)] (* d d)))
      (sense/in-range? (:pos e) o r))))

(defn targetable?
  "Returns true when mob eid, e, may look at entity oid, o, at tick t:
  it is seen, in follow range and in sight."
  [world eid e t oid o]
  (boolean
    (and o (not= eid oid) (game-mode/seen? o) (in-range? e t oid o)
         (animal/in-sight? world e o))))

(defn- attackable? [world eid e t oid o]
  (and (not (zero? (difficulty/id world)))
       (not (game-mode/invulnerable? o))
       (targetable? world eid e t oid o)))

(def ^:private visible-key :nearest-visible-living-entities)

(defn- sighted
  "Returns [seen? e] for entity oid, o, by the visible entities of e,
  which keep each answer from the first time it is asked."
  [world eid e t oid o]
  (let [[m until] (get-in e [:brain :memories visible-key])]
    (if-some [s (get (:seen m) oid)]
      [s e]
      (let [s (targetable? world eid e t oid o)]
        [s (b/remember e visible-key (assoc-in m [:seen oid] s)
                       until)]))))

(defn seen
  "Returns [ids e] of the visible living entities of mob eid, e, at
  tick t that pred accepts, nearest first, at most n of them, and e
  with what it learned of their sight."
  ([world eid e t pred] (seen world eid e t pred Long/MAX_VALUE))
  ([world eid e t pred n]
   (let [es (:entities world)]
     (loop [ids (:near (b/recall e visible-key t)) e e acc []]
       (let [oid (first ids) o (get es oid)]
         (cond (or (nil? ids) (>= (count acc) (long n))) [acc e]
               (not (and o (pred o))) (recur (next ids) e acc)
               :else
               (let [[s e] (sighted world eid e t oid o)]
                 (recur (next ids) e (if s (conj acc oid) acc)))))))))

(defn nearest-living
  "Returns the sensor of the living entities in follow range of the
  box, nearest first, and of those in sight."
  []
  {:rate default-rate
   :requires [:nearest-living-entities visible-key]
   :tick (fn [world eid e _]
           (let [ids (living-near world eid e)]
             (-> (remembered e :nearest-living-entities ids)
                 (remembered visible-key {:near ids :seen {}}))))})

(defn- player-ids [world e pred]
  (let [ps (level/player-entries world)]
    (mapv first (by-distance e (filter #(pred (val %)) ps)))))

(defn- closer-than [e ^double r]
  (let [r2 (* r r)]
    (fn [p]
      (and (not (game-mode/spectator? p))
           (< (v/dist-sq (:pos e) (:pos p)) r2)))))

(defn- sense-players [world eid e t]
  (let [es (:entities world)
        ps (player-ids world e (closer-than e (follow-range e)))
        seen (filterv #(targetable? world eid e t % (es %)) ps)
        foes (filterv #(attackable? world eid e t % (es %)) seen)
        foe (first foes)]
    (-> (remembered e :nearest-players ps)
        (remembered :nearest-visible-player (first seen))
        (remembered :nearest-visible-attackable-players foes)
        (remembered :nearest-visible-attackable-player foe))))

(defn players
  "Returns the sensor of the players in follow range, nearest first,
  of the nearest in sight and of those a mob may attack."
  []
  {:rate default-rate
   :requires [:nearest-players :nearest-visible-player
              :nearest-visible-attackable-player
              :nearest-visible-attackable-players]
   :tick sense-players})

(defn- last-source [e t]
  (when-let [[src at] (:last-hurt e)]
    (when (<= (- (long t) (long at)) hurt-memory) src)))

(defn- hurt-cause [e src]
  (if (entity/living-attacker? src)
    (remembered e :hurt-by-entity (:cause src))
    e))

(defn- sense-hurt [world _ e t]
  (let [src (last-source e t)
        e (if src
            (hurt-cause (remembered e :hurt-by src) src)
            (b/erase e :hurt-by))
        cause (b/recall e :hurt-by-entity t)]
    (if (and (some? cause)
             (not (entity/alive? (get (:entities world) cause))))
      (b/erase e :hurt-by-entity)
      e)))

(defn hurt-by
  "Returns the sensor of the damage source of the mob in the last 40
  ticks, from its :last-hurt [source tick], and of its living cause."
  []
  {:rate default-rate :requires [:hurt-by :hurt-by-entity]
   :tick sense-hurt})

(defn food-lure?
  "Returns true when mob e eats item."
  [e item]
  (contains? (mobs/food (:type e)) item))

(defn- lured? [lure? e p r]
  (and (game-mode/seen? p)
       (sense/in-range? (:pos e) p r)
       (some #(lure? e %) (sense/hands-of p))))

(defn tempting
  "Returns the sensor of the nearest player in tempt range that holds
  an item lure? accepts for the mob in either hand."
  [lure?]
  {:rate default-rate :requires [:tempting-player]
   :tick (fn [world _ e _]
           (let [r (mobs/attribute (:type e) :tempt-range)
                 pred #(lured? lure? e % (double (float r)))
                 pid (first (player-ids world e pred))]
             (remembered e :tempting-player pid)))})

(defn- adult-of? [e o]
  (and (= (:type o) (:type e)) (not (mobs/baby? o))))

(defn adult
  "Returns the sensor of the nearest visible adult of the same type."
  []
  {:rate default-rate
   :requires [:nearest-visible-adult visible-key]
   :tick (fn [world eid e t]
           (if (b/present? e visible-key t)
             (let [[ids e] (seen world eid e t #(adult-of? e %) 1)]
               (remembered e :nearest-visible-adult (first ids)))
             e))})

(defn mob-sensor
  "Returns the sensor that every rate ticks sets memory for ttl ticks
  when scares? accepts world, mob, the id of one of its nearest
  living entities and the tick. It erases memory while ready?
  rejects the mob at the tick."
  [rate scares? ready? memory ttl]
  {:rate rate :requires [:nearest-living-entities]
   :tick (fn [world _ e t]
           (cond
             (not (ready? e t)) (b/erase e memory)
             (some #(scares? world e % t)
                   (b/recall e :nearest-living-entities t))
             (b/remember-for e memory true t ttl)
             :else e))})

(defn in-water
  "Returns the sensor of whether the mob touches water."
  []
  {:rate default-rate :requires [:is-in-water]
   :tick (fn [_ _ e _]
           (if (:wet? e)
             (remembered e :is-in-water true)
             (b/erase e :is-in-water)))})
