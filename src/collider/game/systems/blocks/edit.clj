(ns collider.game.systems.blocks.edit
  "Block edit checks and change deltas."
  (:require [collider.data :as data]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.blocks.campfire :as campfire]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn block-at ^long [world pos]
  (chunk/chunks-get-block (:chunks world) pos))

(def ^:private ^:const player-half 0.3)
(def ^:private ^:const player-height 1.8)
(def ^:private ^:const crouching-height 1.5)
(def ^:private ^:const tnt-half 0.49)
(def ^:private ^:const tnt-height 0.98)

(defn builder-box
  "Returns the half width and height an entity blocks placement with, or nil
   when it never blocks."
  [e]
  (case (:type e)
    :player [player-half (if (and (:sneaking? e) (not (:flying e))) crouching-height player-height)]
    (:tnt :falling-block) [tnt-half tnt-height]
    :item nil
    (when-let [m (get mobs/types (:type e))]
      (let [s (if (mobs/baby? e) 0.5 1.0)]
        [(* s (double (:half m))) (* s (double (:height m)))]))))

(defn box-hits? [[x1 y1 z1 x2 y2 z2] [px py pz] [half h]]
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

(defn obstructed? [world [x y z] state]
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
  "Returns the effect that shows one player the true block at pos."
  [world eid pos]
  (out/to eid (out/blocks-changed (chunk/block-chunk pos) [[pos (block-at world pos)]])))

(defn reject-deltas [world eid pos pos']
  (cond-> [(own-change world eid pos)]
          pos' (conj (own-change world eid pos'))))

(defn change-deltas
  "Returns the deltas for the changes and for the changes they cause in the
   blocks around them."
  [world changes]
  (let [chunks' (chunk/chunks-set-blocks (:chunks world) changes)
        all (into (vec changes) (connect/derived-changes chunks' (map first changes) (:tick world)))
        chunks'' (chunk/chunks-set-blocks chunks' all)
        mixed (liquid/mix-changes chunks'' (map first all))]
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

(defn hit-uv
  "Returns where a click landed on a face, across and up, from zero to one."
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
  "Returns which slot of a grid drawn on the block face was clicked."
  [st face cursor rows cols]
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

(defn- unlit [^long st]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :lit :false)))

(defn candle-out-deltas
  "Returns the deltas of AbstractCandleBlock.extinguish."
  [world pos]
  (let [cur (block-at world pos)]
    (when (= :true (:lit (block/props-of cur)))
      (concat (change-deltas world [[pos (unlit cur)]])
              [(out/all (out/sound :candle/extinguish pos
                                   1.0 1.0))]))))

(defn campfire-out-deltas
  "Returns the deltas of CampfireBlock.dowse with its level event."
  [world pos]
  (when-let [st (campfire/dowsed (block-at world pos))]
    (concat (change-deltas world [[pos st]])
            [(out/all (out/level-event
                        out/sound-extinguish-fire pos))])))

(defn dowse-deltas
  "Returns the deltas of a candle or a campfire going out under water,
   as AbstractThrownPotion.dowseFire puts them out."
  [world pos]
  (case (block/type-of (block-at world pos))
    (:candle :candle-cake) (candle-out-deltas world pos)
    :campfire (campfire-out-deltas world pos)
    nil))
