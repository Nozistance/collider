(ns collider.game.mob.sense
  "A mob's senses of the blocks under it and the entities around it."
  (:require [collider.game.entity.sections :as sections]
            [collider.game.mode :as game-mode]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.vec :as v]
            [collider.world.chunk :as chunk])
  (:import (clojure.lang MapEntry)))

(set! *warn-on-reflection* true)

(defn block-at
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

(defn indexed
  "Returns world with its entities by section for the searches of its
  mobs this tick."
  [world]
  (assoc world ::index
         (delay [(sections/index world) (:entities world)])))

(defn- entity-index [world]
  (if-let [d (::index world)]
    @d
    [(sections/index world) (:entities world)]))

(defn- near-ids [idx pos r]
  (let [x (v/x pos) z (v/z pos) r (double r)]
    (sections/columns idx (- x r) (- z r) (+ x r) (+ z r))))

(defn- nearest-by [world pos r dist r2 pred]
  (let [[idx es] (entity-index world) r2 (double r2)
        f (fn [best oid]
            (let [o (get es oid)]
              (if-not (pred oid o)
                best
                (let [d2 (double (dist pos (:pos o)))]
                  (if (and (< d2 r2) (closer? best d2 oid))
                    [d2 oid o]
                    best)))))]
    (reduce f nil (near-ids idx pos r))))

(defn nearest
  "Returns [distance-squared id entity] of the nearest entity within
  r2 that pred accepts, or nil. Distance counts on x and z."
  [world pos r2 pred]
  (let [r2 (double r2)]
    (nearest-by world pos (Math/sqrt r2) v/dist-xz-sq r2 pred)))

(defn nearest-around
  "Returns [distance-squared id entity] of the entity nearest pos in
  space among those that pred accepts, or nil. Pred must refuse
  every entity farther than r from pos on x or on z."
  [world pos r pred]
  (nearest-by world pos (double r) v/dist-sq
              Double/POSITIVE_INFINITY pred))

(defn around
  "Returns [id entity] of every entity whose section column meets the
  square of half side r around pos, column by column."
  [world pos r]
  (let [[idx es] (entity-index world)]
    (map #(MapEntry/create % (get es %)) (near-ids idx pos r))))

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

(def ^:private armor-slots [5 6 7 8])

(def ^:private ^:const least-cover (double (float 0.1)))

(defn- armor-cover
  "Returns the share of the armor slots of player p that hold
  something, never below a tenth."
  ^double [p]
  (let [inv (:inventory p)
        worn (count (keep #(:item (get inv %)) armor-slots))]
    (max least-cover (/ (double worn) 4.0))))

(defn visibility
  "Returns how much of its range a mob sees player p at. A sneaking
  player is seen less, and an invisible one less still, the less
  armor it wears."
  ^double [p]
  (cond-> 1.0
    (:sneaking? p) (* 0.8)
    (contains? (:effects p) :invisibility)
    (* (* 0.7 (armor-cover p)))))

(defn in-range?
  "Returns true when a mob at pos notices player p within range r.
  The range shrinks with the visibility of the player, never below 2."
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
