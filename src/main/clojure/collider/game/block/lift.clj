(ns collider.game.block.lift
  "Bodies that rise out of a block whose collision shape grows up, as
  snow does when snow falls on it or farmland when it turns to dirt."
  (:require [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.vec :as v]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private ^:const eps 1.0E-7)

(def ^:private ^:const all-relative 511)

(def ^:private lifting
  #{[:snow :snow] [:farmland :dirt] [:dirt-path :dirt] [:mud :clay]})

(defn rises?
  "Returns true when block old turning into new lifts the bodies in
  what it grows."
  [old new]
  (contains? lifting [(block/block-of old) (block/block-of new)]))

(defn- top-of ^double [^long st]
  (let [tops (map #(nth % 4) (block/collision-boxes st))]
    (/ (double (reduce max 0 tops)) 16.0)))

(defn- across? [^double lo ^double hi ^double c]
  (and (< lo (inc c)) (> hi c)))

(defn- caught? [o [cx cy cz] ^double lo ^double hi]
  (let [[half h] (entity/box o) [x y z] (:pos o)
        half (double half) x (double x) y (double y) z (double z)
        cy (double cy)]
    (and (across? (- x half) (+ x half) (double cx))
         (across? (- z half) (+ z half) (double cz))
         (< y (+ cy hi)) (> (+ y (double h)) (+ cy lo)))))

(defn- held? [o [cx _ cz]]
  (let [[half] (entity/box o) [x _ z] (:pos o)
        in (- (double half) eps) x (double x) z (double z)]
    (and (across? (- x in) (+ x in) (double cx))
         (across? (- z in) (+ z in) (double cz)))))

(defn- rise
  "Returns how far body o rises, one block less what it falls back
  from there onto the grown shape topped at hi of cell."
  ^double [o [_ cy _ :as cell] ^double hi]
  (let [b (+ (double (nth (:pos o) 1)) 1.0)
        top (+ (double cy) hi)]
    (if (and (held? o cell) (>= (+ b eps) top))
      (+ 1.0 (max -1.0 (- top b)))
      0.0)))

(defn- lifted-body [o cell lo hi]
  (when (and (entity/box o) (caught? o cell lo hi))
    (let [dy (rise o cell hi)]
      [dy (v/add (:pos o) [0.0 dy 0.0])])))

(defn rising
  "Returns how far body o rises as block old at cell turns into new
  and where to, as [dy pos], or nil when it stays out of the way."
  [o cell old new]
  (lifted-body o cell (top-of old) (top-of new)))

(defn player-deltas
  "Returns the deltas that move player eid up by dy to pos and tell
  its client so."
  [eid [dy p]]
  [[:teleport eid p]
   (out/to eid (out/teleport [0.0 dy 0.0] 0.0 0.0 all-relative))])

(defn- body-deltas [eid o r]
  (if (= :player (:type o))
    (player-deltas eid r)
    [[:merge-entity eid {:pos (nth r 1)}]]))

(defn- lift-one [skip [es ds] [cell old new]]
  (let [lo (top-of old) hi (top-of new)
        hits (for [[id o] es :when (not= id skip)
                   :let [r (lifted-body o cell lo hi)] :when r]
               [id o r])]
    [(reduce (fn [es [id o [_ p]]] (assoc es id (assoc o :pos p)))
             es hits)
     (into ds (mapcat (fn [[id o r]] (body-deltas id o r))) hits)]))

(defn lifted
  "Returns the deltas that lift the bodies of level world, all but
  skip, out of what each change [cell old new] of changes in turn
  grows. A body lifted twice rises from where the first lift left it."
  ([world changes] (lifted world changes nil))
  ([world changes skip]
   (second (reduce (partial lift-one skip) [(:entities world) []]
                   changes))))
