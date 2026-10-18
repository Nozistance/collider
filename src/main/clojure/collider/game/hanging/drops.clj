(ns collider.game.hanging.drops
  "The sounds and drops of paintings and item frames that break."
  (:require [collider.game.changes :as changes]
            [collider.game.entity :as entity]
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

(defn signal-deltas
  "Returns the deltas of the comparators that read the cell of frame e
  hearing of its change. Level w holds the frame as it is after, and
  base is the tick the change comes after."
  [w e base]
  (remove changes/idle? (changes/change-deltas
                  w [[:output-signal (:block-pos e)]] base)))

(defn gone-deltas
  "Returns the deltas of the comparators that read the cell of hanging
  entity eid, e, hearing that it went from level w after tick base.
  A painting has none."
  [w eid e base]
  (when-not (= :painting (:type e))
    (signal-deltas (update w :entities dissoc eid) e base)))

(defn- frame-broken [world eid e causer by heard]
  (concat [(sound e "break")]
          heard
          (frame-drops world eid e causer true)
          (signal/game-event :block-change (:pos e) by)))

(defn- painting-broken [world eid e causer]
  (when (drops? world)
    (cons (sound e "break")
          (when-not (endless? causer)
            [(spawned world eid e {:item :painting :count 1} 0)]))))

(defn dropped
  "Returns the deltas of hanging entity eid breaking by causer, whose
  eid is by. They hold its break sound and drops, and a frame adds
  heard, the deltas of gone-deltas, and a block change game event."
  [world eid e causer by heard]
  (if (= :painting (:type e))
    (painting-broken world eid e causer)
    (frame-broken world eid e causer by heard)))

(defn kill-deltas
  "Returns the deltas of hanging entity eid, e, killed by causer,
  whose eid is by, with heard as dropped takes it. Both causer and by
  are nil when nothing caused it."
  [world eid e causer by heard]
  (concat [[:remove-entity eid]]
          (signal/game-event :entity-die (:pos e) eid)
          (dropped world eid e causer by heard)))
