(ns collider.world.space.path
  (:require [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-nodes 200)
(def ^:private ^:const max-fall 3)
(defn- water-at? [chunks template x y z]
  (= :water (liquid/liquid-class (chunk/block-state chunks template x y z))))

(defn- fence-at? [chunks template [x y z]]
  (phys/fence-at? chunks template x y z))

(defn- open? [chunks template x y z]
  (and (not (phys/solid? chunks template x y z))
       (not (phys/solid? chunks template x (inc y) z))))

(defn- supported? [chunks template x y z]
  (or (and (phys/solid? chunks template x (dec y) z)
           (not (fence-at? chunks template [x (dec y) z]))
           (not (fence-at? chunks template [x (- (long y) 2) z])))
      (water-at? chunks template x (dec y) z)
      (water-at? chunks template x y z)))

(defn- wet? [chunks template x y z]
  (or (water-at? chunks template x y z)
      (water-at? chunks template x (dec y) z)))

(defn- walkable? [chunks template avoid-water? cx cy cz]
  (and (open? chunks template cx cy cz)
       (supported? chunks template cx cy cz)
       (not (and avoid-water? (wet? chunks template cx cy cz)))))

(defn- fall-cell [chunks template avoid-water? nx y nz]
  (loop [dy 1]
    (when (<= dy max-fall)
      (let [ny (- (long y) (long dy))]
        (cond
          (not (open? chunks template nx ny nz)) nil
          (supported? chunks template nx ny nz)
          (when-not (and avoid-water? (wet? chunks template nx ny nz)) [nx ny nz])
          :else (recur (inc dy)))))))

(defn- step-cell [chunks template avoid-water? [x y z] [dx dz]]
  (let [x (long x) y (long y) z (long z)
        nx (+ x (long dx)) nz (+ z (long dz))
        ok? (fn [cy] (walkable? chunks template avoid-water? nx cy nz))]
    (cond
      (ok? y) [nx y nz]
      (and (ok? (inc y)) (not (phys/solid? chunks template x (+ y 2) z))) [nx (inc y) nz]
      (open? chunks template nx y nz) (fall-cell chunks template avoid-water? nx y nz)
      :else nil)))

(defn- footprint-free? [chunks template half px pz y]
  (every? (fn [[ox oz]]
            (let [cx (long (Math/floor (+ (double px) (double ox))))
                  cz (long (Math/floor (+ (double pz) (double oz))))]
              (and (open? chunks template cx y cz)
                   (supported? chunks template cx y cz))))
          (let [h (double half)] [[(- h) (- h)] [(- h) h] [h (- h)] [h h]])))

(defn direct? [chunks template pos half [wx wy wz]]
  (let [half (double half)
        x (v/x pos) z (v/z pos)
        y (long wy)
        dx (- (+ (double wx) 0.5) x) dz (- (+ (double wz) 0.5) z)
        d (Math/sqrt (+ (* dx dx) (* dz dz)))
        n (max 1 (long (Math/ceil (/ d 0.5))))]
    (loop [i 1]
      (cond
        (> i n) true
        (footprint-free? chunks template half (+ x (/ (* dx i) n)) (+ z (/ (* dz i) n)) y) (recur (inc i))
        :else false))))

(defn- dist ^double [[x1 y1 z1] [x2 y2 z2]]
  (let [dx (- (double x2) (double x1))
        dy (- (double y2) (double y1))
        dz (- (double z2) (double z1))]
    (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(def ^:private dirs [[1 0] [-1 0] [0 1] [0 -1]])
(defn- rebuild [came cell]
  (loop [acc (list cell) c cell]
    (if-let [p (came c)]
      (recur (conj acc p) p)
      (vec (rest acc)))))

(defn- relax [chunks template avoid-water? h cur acc d]
  (let [[open g came best best-h] acc]
    (if-let [nb (step-cell chunks template avoid-water? cur d)]
      (let [ng (+ (double (g cur)) (dist cur nb))]
        (if (< ng (double (get g nb Double/MAX_VALUE)))
          (let [nh (double (h nb))]
            [(conj open [(+ ng nh) nb]) (assoc g nb ng) (assoc came nb cur)
             (if (< nh (double best-h)) nb best) (min nh (double best-h))])
          acc))
      acc)))

(defn find-path [chunks template start goal avoid-water?]
  (let [h (fn ^double [c] (dist c goal))]
    (loop [open (sorted-set [(h start) start]) closed #{} g {start 0.0} came {}
           best start best-h (double (h start)) n 0]
      (if-let [[_ cur :as entry] (first open)]
        (cond
          (= cur goal) (rebuild came cur)
          (>= n max-nodes) (when (not= best start) (rebuild came best))
          (closed cur) (recur (disj open entry) closed g came best best-h n)
          :else
          (let [[open g came best best-h]
                (reduce (partial relax chunks template avoid-water? h cur)
                        [(disj open entry) g came best best-h] dirs)]
            (recur open (conj closed cur) g came best (double best-h) (inc n))))
        (when (not= best start) (rebuild came best))))))
