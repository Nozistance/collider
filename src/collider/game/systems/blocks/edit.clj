(ns collider.game.systems.blocks.edit
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

(defn block-at ^long [world pos]
  (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos))

(defn builder-box [e]
  (case (:type e)
    :player [0.3 (if (and (:sneaking? e) (not (:flying e))) 1.5 1.8)]
    (:tnt :falling-block) [0.49 0.98]
    :item nil
    (when-let [m (get mobs/types (:type e))]
      (let [s (if (mobs/baby? e) 0.5 1.0)]
        [(* s (double (:half m))) (* s (double (:height m)))]))))

(defn box-hits? [[x1 y1 z1 x2 y2 z2] [px py pz] [half h]]
  (let [px (double px) py (double py) pz (double pz) half (double half) h (double h)]
    (and (> (+ px half) (double x1)) (< (- px half) (double x2))
         (> (+ py h) (double y1)) (< py (double y2))
         (> (+ pz half) (double z1)) (< (- pz half) (double z2)))))

(defn obstructed? [world [x y z] state]
  (let [boxes (map (fn [[a b c d e f]]
                     [(+ (long x) (/ (double a) 16.0)) (+ (long y) (/ (double b) 16.0)) (+ (long z) (/ (double c) 16.0))
                      (+ (long x) (/ (double d) 16.0)) (+ (long y) (/ (double e) 16.0)) (+ (long z) (/ (double f) 16.0))])
                   (block/collision-boxes state))]
    (some (fn [[_ e]]
            (when-let [dims (builder-box e)]
              (some #(box-hits? % (:pos e) dims) boxes)))
          (:entities world))))

(defn own-change [world eid pos]
  (out/to eid (out/blocks-changed (chunk/block-chunk pos) [[pos (block-at world pos)]])))

(defn reject-deltas [world eid pos pos']
  (cond-> [(own-change world eid pos)]
          pos' (conj (own-change world eid pos'))))

(defn change-deltas [world changes]
  (let [chunks' (chunk/chunks-set-blocks (:chunks world) gen/flat-chunk changes)
        all (into (vec changes) (connect/derived-changes chunks' (map first changes) (:tick world)))
        chunks'' (chunk/chunks-set-blocks chunks' gen/flat-chunk all)
        mixed (liquid/mix-changes chunks'' gen/flat-chunk (map first all))]
    (into [[:set-blocks (into all mixed) (dec (long (:tick world)))]]
          (map (fn [[p _]] (out/all (out/fizz p))))
          mixed)))

(defn placed-deltas
  ([world eid pos state] (placed-deltas world eid [[pos state]]))
  ([world eid changes]
   (let [[pos state] (first changes)]
     (conj (change-deltas world changes)
           (out/except eid (out/sound (data/place-sound (block/block-of state)) pos 1.0 0.8))))))

(defn be-changed [pos e]
  [[:set-block-entity pos e] (out/all (out/block-entity pos))])

(defn held-slot ^long [world eid]
  (+ 36 (long (or (get-in world [:entities eid :held-slot]) 0))))

(defn held-stack [world eid]
  (get-in world [:entities eid :inventory (held-slot world eid)]))

(defn hit-uv [face [cx cy cz]]
  (let [x (/ (double cx) 16.0) y (/ (double cy) 16.0) z (/ (double cz) 16.0)]
    (case (long face)
      2 [(- 1.0 x) y]
      3 [x y]
      4 [z y]
      5 [(- 1.0 z) y]
      nil)))

(defn section ^long [^double rel ^long n]
  (min (dec n) (max 0 (long (Math/floor (* rel n))))))

(defn hit-slot [st face cursor rows cols]
  (when (= (block/facing-of st) (dir/from-index (long face)))
    (when-let [[u v] (hit-uv face cursor)]
      (+ (section (double u) (long cols)) (* (long cols) (section (- 1.0 (double v)) (long rows)))))))

(defn waterloggable? [st]
  (= :false (:waterlogged (block/props-of st))))

(defn with-water [st logged?]
  (block/state (block/block-of st) (assoc (block/props-of st) :waterlogged (if logged? :true :false))))

(defn waterlogged [world pos' state]
  (if (and (= :water (liquid/liquid-class (block-at world pos')))
           (contains? (block/props-of state) :waterlogged))
    (block/state (block/block-of state) (assoc (block/props-of state) :waterlogged :true))
    state))
