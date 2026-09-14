(ns collider.game.systems.blocks.edit
  "Block edits: checks and the deltas of a change."
  (:require [collider.data :as data]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn block-at
  "Returns the block at pos."
  ^long [world pos]
  (chunk/chunks-get-block (:chunks world) (gen/flat-chunk) pos))

(def ^:private ^:const player-half 0.3)
(def ^:private ^:const player-height 1.8)
(def ^:private ^:const crouching-height 1.5)
(def ^:private ^:const tnt-half 0.49)
(def ^:private ^:const tnt-height 0.98)

(defn builder-box
  "Returns the width and height an entity takes up while building, or nil
   when it never stands in the way."
  [e]
  (case (:type e)
    :player [player-half (if (and (:sneaking? e) (not (:flying e))) crouching-height player-height)]
    (:tnt :falling-block) [tnt-half tnt-height]
    :item nil
    (when-let [m (get mobs/types (:type e))]
      (let [s (if (mobs/baby? e) 0.5 1.0)]
        [(* s (double (:half m))) (* s (double (:height m)))]))))

(defn box-hits?
  "Returns true when an entity of the given size at pos overlaps the box."
  [[x1 y1 z1 x2 y2 z2] [px py pz] [half h]]
  (let [px (double px) py (double py) pz (double pz) half (double half) h (double h)]
    (and (> (+ px half) (double x1)) (< (- px half) (double x2))
         (> (+ py h) (double y1)) (< py (double y2))
         (> (+ pz half) (double z1)) (< (- pz half) (double z2)))))

(defn- box-hits-at? [^doubles a ^long n [px py pz] [half h]]
  (let [px (double px) py (double py) pz (double pz) half (double half) h (double h)]
    (loop [i 0]
      (if (= i n)
        false
        (let [o (* i 6)]
          (if (and (> (+ px half) (aget a o)) (< (- px half) (aget a (+ o 3)))
                   (> (+ py h) (aget a (+ o 1))) (< py (aget a (+ o 4)))
                   (> (+ pz half) (aget a (+ o 2))) (< (- pz half) (aget a (+ o 5))))
            true
            (recur (inc i))))))))

(defn- abs-boxes ^doubles [^long x ^long y ^long z state]
  (let [bs (block/collision-boxes state)
        a (double-array (* 6 (count bs)))]
    (reduce (fn [^long i b]
              (let [o (* i 6)]
                (aset a o (+ x (/ (double (nth b 0)) 16.0)))
                (aset a (+ o 1) (+ y (/ (double (nth b 1)) 16.0)))
                (aset a (+ o 2) (+ z (/ (double (nth b 2)) 16.0)))
                (aset a (+ o 3) (+ x (/ (double (nth b 3)) 16.0)))
                (aset a (+ o 4) (+ y (/ (double (nth b 4)) 16.0)))
                (aset a (+ o 5) (+ z (/ (double (nth b 5)) 16.0)))
                (inc i)))
            0 bs)
    a))

(defn obstructed?
  "Returns true when a block placed at pos would stand inside an entity."
  [world [x y z] state]
  (let [^doubles a (abs-boxes (long x) (long y) (long z) state)
        n (quot (alength a) 6)]
    (and (pos? n)
         (boolean
           (reduce-kv (fn [_ _ e]
                        (if-let [dims (builder-box e)]
                          (if (box-hits-at? a n (:pos e) dims) (reduced true) false)
                          false))
                      false (:entities world))))))

(defn own-change
  "Returns the effect telling one player what the block at pos really is."
  [world eid pos]
  (out/to eid (out/blocks-changed (chunk/block-chunk pos) [[pos (block-at world pos)]])))

(defn reject-deltas
  "Returns the deltas that take back an edit the world refused."
  [world eid pos pos']
  (cond-> [(own-change world eid pos)]
          pos' (conj (own-change world eid pos'))))

(defn change-deltas
  "Returns the deltas for changing blocks, together with what the change
   does to the blocks around them."
  [world changes]
  (let [chunks' (chunk/chunks-set-blocks (:chunks world) (gen/flat-chunk) changes)
        all (into (vec changes) (connect/derived-changes chunks' (map first changes) (:tick world)))
        chunks'' (chunk/chunks-set-blocks chunks' (gen/flat-chunk) all)
        mixed (liquid/mix-changes chunks'' (gen/flat-chunk) (map first all))]
    (into [[:set-blocks (into all mixed) (dec (long (:tick world)))]]
          (map (fn [[p _]] (out/all (out/fizz p))))
          mixed)))

(defn placed-deltas
  "Returns the deltas for a player placing blocks, with the sound the
   block makes."
  ([world eid pos state] (placed-deltas world eid [[pos state]]))
  ([world eid changes]
   (let [[pos state] (first changes)]
     (conj (change-deltas world changes)
           (out/except eid (out/sound (data/place-sound (block/block-of state)) pos 1.0 0.8))))))

(defn be-changed
  "Returns the deltas for what the block at pos holds."
  [pos e]
  [[:set-block-entity pos e] (out/all (out/block-entity pos))])

(defn held-slot
  "Returns the inventory slot a player holds."
  ^long [world eid]
  (+ 36 (long (or (get-in world [:entities eid :held-slot]) 0))))

(defn held-stack
  "Returns the stack a player holds."
  [world eid]
  (get-in world [:entities eid :inventory (held-slot world eid)]))

(defn hit-uv
  "Returns where on a face a click landed, across and up, from zero to one."
  [face [cx cy cz]]
  (let [x (/ (double cx) 16.0) y (/ (double cy) 16.0) z (/ (double cz) 16.0)]
    (case (long face)
      2 [(- 1.0 x) y]
      3 [x y]
      4 [z y]
      5 [(- 1.0 z) y]
      nil)))

(defn section
  "Returns which of n equal parts a fraction falls in."
  ^long [^double rel ^long n]
  (min (dec n) (max 0 (long (Math/floor (* rel n))))))

(defn hit-slot
  "Returns which slot of a grid drawn on a block's face was clicked."
  [st face cursor rows cols]
  (when (= (block/facing-of st) (dir/from-index (long face)))
    (when-let [[u v] (hit-uv face cursor)]
      (+ (section (double u) (long cols)) (* (long cols) (section (- 1.0 (double v)) (long rows)))))))

(defn waterloggable?
  "Returns true when the block can still take water."
  [st]
  (= :false (:waterlogged (block/props-of st))))

(defn with-water
  "Returns the block with water added or drained."
  [st logged?]
  (block/state (block/block-of st) (assoc (block/props-of st) :waterlogged (if logged? :true :false))))

(defn waterlogged
  "Returns state full of water when it goes into water."
  [world pos' state]
  (if (and (= :water (liquid/liquid-class (block-at world pos')))
           (contains? (block/props-of state) :waterlogged))
    (block/state (block/block-of state) (assoc (block/props-of state) :waterlogged :true))
    state))
