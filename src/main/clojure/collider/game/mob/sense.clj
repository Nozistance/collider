(ns collider.game.mob.sense
  "A mob's senses of the blocks under it and the entities around it."
  (:require [collider.game.mode :as game-mode]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn block-at
  "Returns the block state at a position."
  (^long [world p] (chunk/at (:chunks world) p))
  (^long [world x y z] (chunk/block-state (:chunks world) x y z)))

(defn feet-cell
  "Returns the block cell a position stands in."
  [p]
  [(long (Math/floor (v/x p)))
   (long (Math/floor (v/y p)))
   (long (Math/floor (v/z p)))])

(defn- closer? [best ^double d2 oid]
  (or (nil? best)
      (< d2 (double (best 0)))
      (and (= d2 (double (best 0))) (< (long oid) (long (best 1))))))

(defn- near? [pos ^double r2 pred o]
  (and o (< (v/dist-sq pos (:pos o)) r2) (pred o)))

(defn player-within?
  "Returns true when a player that pred accepts stands closer than
  the root of r2 to pos."
  [world pos r2 pred]
  (let [es (:entities world)
        f (fn [_ _ oid]
            (when (near? pos (double r2) pred (get es oid))
              (reduced true)))]
    (boolean (reduce-kv f nil (:players world)))))

(defn watchers
  "Returns the x, y and z of every player that pred accepts, one
  player after another."
  ^doubles [world pred]
  (let [es (:entities world)
        at (fn [o] (let [p (:pos o)] [(v/x p) (v/y p) (v/z p)]))]
    (double-array
      (into [] (comp (keep (fn [[_ oid]] (get es oid)))
                     (filter pred) (mapcat at))
            (:players world)))))

(defn watched?
  "Returns true when a player of ws, as watchers gives them, stands
  closer than the root of r2 to pos."
  [^doubles ws pos ^double r2]
  (let [x (v/x pos) y (v/y pos) z (v/z pos) n (alength ws)]
    (loop [i 0]
      (and (< i n)
           (let [dx (- (aget ws i) x) dy (- (aget ws (inc i)) y)
                 dz (- (aget ws (+ i 2)) z)]
             (or (< (+ (* dx dx) (* dy dy) (* dz dz)) r2)
                 (recur (+ i 3))))))))

(def ^:private ^:const cell-shift 2)

(defn- cell-key ^long [^long cx ^long cz]
  (bit-or (bit-shift-left (bit-and cx 0xFFFFFFFF) 32)
          (bit-and cz 0xFFFFFFFF)))

(defn- cell-at ^long [^double c]
  (bit-shift-right (long (Math/floor c)) cell-shift))

(defn- cell-of ^long [pos]
  (cell-key (cell-at (v/x pos)) (cell-at (v/z pos))))

(defn- build-index [entities]
  (persistent!
    (reduce-kv (fn [m eid e]
                 (if-let [p (:pos e)]
                   (let [k (cell-of p)]
                     (assoc! m k (conj (get m k []) [eid e])))
                   m))
               (transient {})
               entities)))

(def ^:private index-cache (atom nil))

(defn- entity-index [entities]
  (let [c @index-cache]
    (if (and c (identical? (nth c 0) entities))
      (nth c 1)
      (let [idx (build-index entities)]
        (reset! index-cache [entities idx])
        idx))))

(defn- scan-cell [best pos r2 pred entries]
  (reduce (fn [best [oid o]]
            (if-not (pred oid o)
              best
              (let [d2 (v/dist-xz-sq pos (:pos o))]
                (if (and (< d2 (double r2)) (closer? best d2 oid))
                  [d2 oid o]
                  best))))
          best
          entries))

(defn- cell-span [^double c ^double r]
  [(cell-at (- c r)) (cell-at (+ c r))])

(defn- scan-row [index best pos r2 pred cx z0 z1]
  (loop [cz (long z0) best best]
    (if (> cz (long z1))
      best
      (recur (inc cz)
             (scan-cell best pos r2 pred
                        (get index (cell-key (long cx) cz)))))))

(defn nearest
  "Returns [distance-squared id entity] of the nearest entity within
  r2 that pred accepts, or nil."
  [world pos r2 pred]
  (let [r2 (double r2)
        index (entity-index (:entities world))
        r (Math/sqrt r2)
        [x0 x1] (cell-span (v/x pos) r)
        [z0 z1] (cell-span (v/z pos) r)]
    (loop [cx (long x0) best nil]
      (if (> cx (long x1))
        best
        (recur (inc cx)
               (scan-row index best pos r2 pred cx z0 z1))))))

(defn held-of
  "Returns the item in the player's selected hotbar slot."
  [p]
  (get-in p [:inventory (+ 36 (long (or (:held-slot p) 0))) :item]))

(defn in-hand
  "Returns the item the player holds in hand, nil for an empty one."
  [p hand]
  (:item (player/hand-stack p hand)))

(defn hands-of
  "Returns the set of items the player holds in either hand."
  [p]
  (set (keep (fn [slot] (get-in p [:inventory slot :item]))
             [(+ 36 (long (or (:held-slot p) 0))) 45])))

(defn visibility
  "Returns how much of its range a mob sees player p at, as
  LivingEntity.getVisibilityPercent: a sneaking player less."
  ^double [p]
  (if (:sneaking? p) 0.8 1.0))

(defn in-range?
  "Tells whether a mob at pos notices player p within range r, as
  TargetingConditions.test: r shrinks with the player's visibility,
  never below 2."
  [pos p ^double r]
  (let [d (max (* r (visibility p)) 2.0)]
    (<= (v/dist-sq pos (:pos p)) (* d d))))

(defn holders
  "Returns [id items player] for every player that holds something
  and that mobs can see."
  [world]
  (into []
        (keep (fn [[pid p]]
                (let [items (hands-of p)]
                  (when (and (seq items) (game-mode/seen? p))
                    [pid items p]))))
        (level/player-entries world)))
