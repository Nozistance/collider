(ns collider.world.connect
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(defn- tag [t] (set (get-in @data/tags ["block" t])))
(def ^:private fences (delay (tag "fences")))
(def ^:private wooden (delay (tag "wooden_fences")))
(def ^:private walls (delay (tag "walls")))
(def ^:private leaves (delay (tag "leaves")))
(def ^:private exceptions #{:barrier :carved-pumpkin :jack-o-lantern :melon :pumpkin})
(def ^:private dirs {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})
(def ^:private neighbours (conj (vec (vals dirs)) [0 1 0] [0 -1 0]))
(def pair-types #{:double-plant :tall-flower :tall-seagrass})
(def placed-types
  #{:fence :wall :iron-bars :stained-glass-pane :fence-gate :stair :concrete-powder})
(def connecting-types
  (into #{:fence :wall :iron-bars :stained-glass-pane :fence-gate :door :weathering-copper-door :bed
          :stair :concrete-powder :vine :glow-lichen :multiface :sculk-vein}
        (concat pair-types block/growing-plant-types [:pitcher-crop])))

(defn- exception? [n]
  (or (contains? @leaves n) (contains? exceptions n) (str/ends-with? (name n) "shulker-box")))

(def ^:private opposite {:north :south :south :north :west :east :east :west})
(def ^:private opposite-face {:up :down :down :up :north :south :south :north :west :east :east :west})
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

(defn- door-state [self st at]
  (let [props (block/props-of st)
        lower? (= :lower (:half props))
        partner (at (if lower? [0 1 0] [0 -1 0]))
        pprops (block/props-of partner)]
    (cond
      (not (and (= self (block/block-of partner)) (not= (:half pprops) (:half props)))) 0
      (and lower? (not (block/face-sturdy? (at [0 -1 0]) :up))) 0
      lower? st
      :else (block/state self (assoc pprops :half :upper)))))

(defn bed-partner-offset [st]
  (let [f (block/facing-of st)]
    (dirs (if (= :foot (:part (block/props-of st))) f (opposite f)))))

(defn- bed-state [self st at]
  (let [partner (at (bed-partner-offset st))
        pprops  (block/props-of partner)]
    (if (and (= self (block/block-of partner))
             (not= (:part pprops) (:part (block/props-of st))))
      (block/state self (assoc (block/props-of st) :occupied (:occupied pprops)))
      0)))

(defn- stair? [st half]
  (and (= :stair (block/type-of st)) (= half (:half (block/props-of st)))))

(defn- can-take-shape? [st at dir]
  (let [n (at (dirs dir))]
    (not (and (stair? n (:half (block/props-of st)))
              (= (block/facing-of n) (block/facing-of st))))))

(defn- stair-state [self st at]
  (let [props  (block/props-of st)
        facing (:facing props)
        half   (:half props)
        axis   (fn [f] (if (#{:north :south} f) :z :x))
        behind (at (dirs facing))
        front  (at (dirs (opposite facing)))
        bf     (block/facing-of behind)
        ff     (block/facing-of front)
        left   (block/counter-clockwise facing)
        shape  (cond
                 (and (stair? behind half) (not= (axis bf) (axis facing)) (can-take-shape? st at (opposite bf)))
                 (if (= bf left) :outer_left :outer_right)
                 (and (stair? front half) (not= (axis ff) (axis facing)) (can-take-shape? st at ff))
                 (if (= ff left) :inner_left :inner_right)
                 :else :straight)]
    (block/state self (assoc props :shape shape))))

(def ^:private six {:down [0 -1 0] :up [0 1 0] :north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})
(defn- water? [st] (or (= :water (liquid/liquid-class st)) (block/waterlogged? st)))
(defn- touches-water? [st at]
  (or (and (water? st) (water? (at (six :down))))
      (some (fn [dir]
              (let [n (at (six dir))]
                (and (water? n) (not (block/face-sturdy? n (opposite-face dir))))))
            [:up :north :south :west :east])))

(defn- powder-state [st at]
  (if (or (water? (at [0 0 0])) (touches-water? (at [0 0 0]) at))
    (block/concrete-of st)
    st))

(defn- pair-state [self st at]
  (let [props (block/props-of st)
        lower? (= :lower (:half props))
        partner (at (if lower? [0 1 0] [0 -1 0]))]
    (if (and (= self (block/block-of partner))
             (not= (:half (block/props-of partner)) (:half props)))
      st
      (block/emptied st))))

(defn- pitcher-state [self st at]
  (if (>= (Long/parseLong (name (:age (block/props-of st)))) 3)
    (pair-state self st at)
    st))

(defn- growing-plant-state [pos st at tick]
  (let [{:keys [head body dir]} (block/growing-plant (block/type-of st))
        on? (contains? #{head body} (block/block-of (at (six dir))))
        berries (:berries (block/props-of st))
        props (cond-> {} berries (assoc :berries berries))]
    (cond
      (and (= head (block/block-of st)) on?) (block/state body props)
      (and (= body (block/block-of st)) (not on?)) (block/state head (assoc props :age (support/plant-age tick pos)))
      :else st)))

(defn reshape [chunks [x y z :as pos] ^long st tick]
  (let [t (block/type-of st)]
    (when (contains? connecting-types t)
      (let [self  (block/block-of st)
            at    (fn [[dx dy dz]]
                    (let [ny (+ (long y) (long dy))]
                      (if (chunk/in-range? ny)
                        (chunk/chunks-get-block chunks gen/flat-chunk [(+ (long x) (long dx)) ny (+ (long z) (long dz))])
                        0)))
            new   (case t
                    (:door :weathering-copper-door) (door-state self st at)
                    :bed (bed-state self st at)
                    :fence-gate (gate-state self st at)
                    :stair (stair-state self st at)
                    :concrete-powder (powder-state st at)
                    :vine (support/vine-updated chunks gen/flat-chunk pos st)
                    (:glow-lichen :multiface :sculk-vein) (support/multiface-updated chunks gen/flat-chunk pos st)
                    (:double-plant :tall-flower :tall-seagrass) (pair-state self st at)
                    :pitcher-crop (pitcher-state self st at)
                    (:weeping-vines :weeping-vines-plant :twisting-vines :twisting-vines-plant :cave-vines :cave-vines-plant)
                    (growing-plant-state pos st at tick)
                    (let [sides (into {} (map (fn [[dir off]]
                                                (let [c (connects? t self (at off) dir)]
                                                  [dir (if (= :wall t) (if c :low :none) (if c :true :false))])))
                                      dirs)
                          props (cond-> (merge (block/props-of st) sides)
                                  (= :wall t) (assoc :up (if (wall-post? sides) :true :false)))]
                      (block/state self props)))]
        (when (not= (long new) st) new)))))

(defn door-hinge [chunks [x y z :as pos] facing cursor-x cursor-z]
  (let [at (fn [[dx dy dz]]
             (let [ny (+ (long y) (long dy))]
               (if (chunk/in-range? ny)
                 (chunk/chunks-get-block chunks gen/flat-chunk [(+ (long x) (long dx)) ny (+ (long z) (long dz))])
                 0)))
        left (dirs (block/counter-clockwise facing))
        right (dirs (block/clockwise facing))
        full (fn [off] (if (block/full-cube? (at off)) 1 0))
        balance (+ (- (full left)) (- (full (mapv + left [0 1 0]))) (full right) (full (mapv + right [0 1 0])))
        lower-door? (fn [st] (and (contains? block/door-types (block/type-of st)) (= :lower (:half (block/props-of st)))))
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

(defn- reshaped [chunks positions tick]
  (let [origin (set positions)]
    (into []
          (keep (fn [[_ y _ :as p]]
                  (when (chunk/in-range? y)
                    (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
                      (when-not (and (contains? origin p) (= :bed (block/type-of st)))
                        (when-let [new (reshape chunks p st tick)]
                          [p new]))))))
          (distinct (concat positions (mapcat around positions))))))

(defn derived-changes [chunks positions tick]
  (loop [chunks chunks positions positions acc [] n 0]
    (let [changes (reshaped chunks positions tick)]
      (if (or (empty? changes) (= n 8))
        acc
        (recur (chunk/chunks-set-blocks chunks gen/flat-chunk changes)
               (map first changes)
               (into acc changes)
               (inc n))))))
