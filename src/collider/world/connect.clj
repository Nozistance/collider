(ns collider.world.connect
  "Connections of fences, walls and panes to their neighbours, by the
   vanilla rules and block tags."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn- tag [t] (set (get-in @data/tags ["block" t])))

(def ^:private fences (delay (tag "fences")))
(def ^:private wooden (delay (tag "wooden_fences")))
(def ^:private walls (delay (tag "walls")))
(def ^:private leaves (delay (tag "leaves")))
(def ^:private exceptions #{:barrier :carved-pumpkin :jack-o-lantern :melon :pumpkin})
(def ^:private dirs {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})
(def ^:private neighbours (conj (vec (vals dirs)) [0 1 0] [0 -1 0]))
(def connecting-types #{:fence :wall :iron-bars :stained-glass-pane :fence-gate :door})

(defn- exception? [n]
  (or (contains? @leaves n) (contains? exceptions n) (str/ends-with? (name n) "shulker-box")))

(def ^:private opposite {:north :south :south :north :west :east :east :west})

(defn- sturdy? [st n dir]
  (and (block/face-sturdy? st (opposite dir)) (not (exception? n))))

(defn- gate-connects? [st dir]
  (let [f (block/facing-of st)]
    (if (#{:north :south} dir) (contains? #{:east :west} f) (contains? #{:north :south} f))))

(defn- fence-connects? [self nst dir]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (contains? @fences n) (= (contains? @wooden n) (contains? @wooden self))
      (= :fence-gate t) (gate-connects? nst dir)
      :else (sturdy? nst n dir))))

(defn- wall-connects? [nst dir]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (or (contains? @walls n) (contains? @fences n) (#{:iron-bars :stained-glass-pane} t)) true
      (= :fence-gate t) (gate-connects? nst dir)
      :else (sturdy? nst n dir))))

(defn- pane-connects? [nst dir]
  (let [n (block/block-of nst) t (block/type-of nst)]
    (cond
      (nil? n) false
      (or (#{:iron-bars :stained-glass-pane} t) (contains? @walls n)) true
      :else (sturdy? nst n dir))))

(defn- connects? [t self nst dir]
  (case t
    :fence (fence-connects? self nst dir)
    :wall (wall-connects? nst dir)
    (pane-connects? nst dir)))

(defn- wall-post? [sides]
  (not (or (and (= :low (:north sides)) (= :low (:south sides)) (= :none (:east sides)) (= :none (:west sides)))
           (and (= :low (:east sides)) (= :low (:west sides)) (= :none (:north sides)) (= :none (:south sides))))))

(defn- wall-at? [st] (contains? @walls (block/block-of st)))

(defn- gate-state [self st at]
  (let [axis (if (#{:north :south} (block/facing-of st)) :z :x)
        in-wall? (if (= axis :z)
                   (or (wall-at? (at [-1 0 0])) (wall-at? (at [1 0 0])))
                   (or (wall-at? (at [0 0 -1])) (wall-at? (at [0 0 1]))))]
    (block/state self (assoc (block/props-of st) :in-wall (if in-wall? :true :false)))))

(defn- door-state
  "A door half follows its other half; without it, or without a sturdy block
   under the lower half, it is gone (vanilla updateShape and canSurvive)."
  [self st at]
  (let [props (block/props-of st)
        lower? (= :lower (:half props))
        partner (at (if lower? [0 1 0] [0 -1 0]))
        pprops (block/props-of partner)]
    (cond
      (not (and (= self (block/block-of partner)) (not= (:half pprops) (:half props)))) 0
      (and lower? (not (block/face-sturdy? (at [0 -1 0]) :up))) 0
      lower? st
      :else (block/state self (assoc pprops :half :upper)))))

(defn reshape
  "New state of the block at pos after its connections to the neighbours are
   recomputed; nil if unchanged or not a connecting block."
  [chunks [x y z :as pos] ^long st]
  (let [t (block/type-of st)]
    (when (contains? connecting-types t)
      (let [self  (block/block-of st)
            at    (fn [[dx dy dz]]
                    (let [ny (+ (long y) (long dy))]
                      (if (chunk/in-range? ny)
                        (chunk/chunks-get-block chunks gen/flat-chunk [(+ (long x) (long dx)) ny (+ (long z) (long dz))])
                        0)))
            new   (case t
                    :door (door-state self st at)
                    :fence-gate (gate-state self st at)
                    (let [sides (into {} (map (fn [[dir off]]
                                                (let [c (connects? t self (at off) dir)]
                                                  [dir (if (= :wall t) (if c :low :none) (if c :true :false))])))
                                      dirs)
                          props (cond-> (merge (block/props-of st) sides)
                                  (= :wall t) (assoc :up (if (wall-post? sides) :true :false)))]
                      (block/state self props)))]
        (when (not= (long new) st) new)))))

(defn door-hinge
  "Hinge side for a door placed at pos looking facing, as vanilla getHinge:
   the side with more solid blocks, next to another door, else by the cursor."
  [chunks [x y z :as pos] facing cursor-x cursor-z]
  (let [at (fn [[dx dy dz]]
             (let [ny (+ (long y) (long dy))]
               (if (chunk/in-range? ny)
                 (chunk/chunks-get-block chunks gen/flat-chunk [(+ (long x) (long dx)) ny (+ (long z) (long dz))])
                 0)))
        left (dirs (block/counter-clockwise facing))
        right (dirs (block/clockwise facing))
        full (fn [off] (if (block/full-cube? (at off)) 1 0))
        balance (+ (- (full left)) (- (full (mapv + left [0 1 0]))) (full right) (full (mapv + right [0 1 0])))
        lower-door? (fn [st] (and (= :door (block/type-of st)) (= :lower (:half (block/props-of st)))))
        door-left (lower-door? (at left))
        door-right (lower-door? (at right))
        [sx _ sz] (dirs facing)
        cx (/ (double cursor-x) 16.0) cz (/ (double cursor-z) 16.0)]
    (cond
      (not (and (or (not door-left) door-right) (<= balance 0))) :right
      (not (and (or (not door-right) door-left) (>= balance 0))) :left
      (and (or (>= (long sx) 0) (not (< cz 0.5)))
           (or (<= (long sx) 0) (not (> cz 0.5)))
           (or (>= (long sz) 0) (not (> cx 0.5)))
           (or (<= (long sz) 0) (not (< cx 0.5)))) :left
      :else :right)))

(defn around [[x y z]]
  (map (fn [[dx dy dz]]
         [(+ (long x) (long dx))
          (+ (long y) (long dy))
          (+ (long z) (long dz))])
       neighbours))

(defn- reshaped [chunks positions]
  (into []
        (keep (fn [[_ y _ :as p]]
                (when (chunk/in-range? y)
                  (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
                    (when-let [new (reshape chunks p st)]
                      [p new])))))
        (distinct (concat positions (mapcat around positions)))))

(defn derived-changes
  "[[pos state] ...] for the blocks at positions and their neighbours whose
   connections changed, followed through: a door half that goes takes the
   other half with it. chunks already hold the changes at positions."
  [chunks positions]
  (loop [chunks chunks positions positions acc [] n 0]
    (let [changes (reshaped chunks positions)]
      (if (or (empty? changes) (= n 8))
        acc
        (recur (chunk/chunks-set-blocks chunks gen/flat-chunk changes)
               (map first changes)
               (into acc changes)
               (inc n))))))
