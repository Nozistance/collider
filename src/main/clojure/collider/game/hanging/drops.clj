(ns collider.game.hanging.drops
  "The sounds and drops of paintings and item frames that break."
  (:require [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn sound
  "Returns the sound effect what of hanging entity e, such as break."
  [e what]
  (let [k (keyword (str "entity." (name (:type e)) "." what))]
    (out/all (out/sound k (:pos e) 1.0 1.0 :neutral))))

(def ^:private drop-offset (double (float 0.15)))

(defn- drop-pos [e]
  (let [[x y z] (:pos e)
        [dx _ dz] (dir/offset (:facing e))]
    [(+ (double x) (* (long dx) drop-offset)) (double y)
     (+ (double z) (* (long dz) drop-offset))]))

(defn- spawned [world eid e stack n]
  (let [ks [(:tick world) eid :hanging-drop n]]
    [:spawn-entity
     (entity/item (drop-pos e) (entity/pop-velocity ks) stack)]))

(defn- drops? [world]
  (get-in world [:rules :entity-drops] true))

(defn- endless? [causer]
  (boolean (and causer (player/infinite-materials? causer))))

(defn frame-drops
  "Returns the deltas of the items frame eid drops when causer
  breaks or empties it. The frame itself drops when with-frame? is
  true."
  [world eid e causer with-frame?]
  (when (and (drops? world) (not (endless? causer)))
    (let [s (:stack e)]
      (cond-> []
        with-frame?
        (conj (spawned world eid e {:item (:type e) :count 1} 0))
        s (conj (spawned world eid e s 1))))))

(defn- frame-broken [world eid e causer by]
  (concat [(sound e "break")]
          (frame-drops world eid e causer true)
          (signal/game-event :block-change (:pos e) by)))

(defn- painting-broken [world eid e causer]
  (when (drops? world)
    (cons (sound e "break")
          (when-not (endless? causer)
            [(spawned world eid e {:item :painting :count 1} 0)]))))

(defn dropped
  "Returns the deltas of hanging entity eid breaking by causer, whose
  eid is by. They hold its break sound and drops, and a frame adds a
  block change game event."
  [world eid e causer by]
  (if (= :painting (:type e))
    (painting-broken world eid e causer)
    (frame-broken world eid e causer by)))

(defn kill-deltas
  "Returns the deltas of hanging entity eid, e, killed by causer,
  whose eid is by. Both causer and by are nil when nothing caused it."
  [world eid e causer by]
  (concat [[:remove-entity eid]]
          (signal/game-event :entity-die (:pos e) eid)
          (dropped world eid e causer by)))
