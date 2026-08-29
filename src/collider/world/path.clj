(ns collider.world.path

  (:require [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.liquid :as liquid]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-nodes 200)
(def ^:private ^:const max-fall 3)
(defn- water-at? [chunks template x y z]
  (= :water (liquid/liquid-class (chunk/block-state chunks template x y z))))

(defn- fence-at? [chunks template [x y z]]
  (contains? phys/fence-ids
             (bit-shift-right (long (chunk/block-state chunks template x y z)) 4)))

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

(defn- step-cell [chunks template avoid-water? [x y z] [dx dz]]
  (let [x (long x) y (long y) z (long z)
        nx (+ x (long dx)) nz (+ z (long dz))
        ok? (fn [cx cy cz]
              (and (open? chunks template cx cy cz)
                   (supported? chunks template cx cy cz)
                   (not (and avoid-water? (wet? chunks template cx cy cz)))))]
    (cond
      (ok? nx y nz)
      [nx y nz]
      (and (ok? nx (inc y) nz)
           (not (phys/solid? chunks template x (+ y 2) z)))
      [nx (inc y) nz]
      (open? chunks template nx y nz)
      (loop [dy 1]
        (when (<= dy max-fall)
          (let [ny (- y dy)]
            (cond
              (not (open? chunks template nx ny nz)) nil
              (supported? chunks template nx ny nz)
              (when-not (and avoid-water? (wet? chunks template nx ny nz))
                [nx ny nz])
              :else (recur (inc dy))))))
      :else nil)))

(defn direct? [chunks template pos half [wx wy wz]]
  (let [half (double half)
        x (v/x pos) z (v/z pos)
        y (long wy)
        tx (+ (double wx) 0.5) tz (+ (double wz) 0.5)
        dx (- tx x) dz (- tz z)
        d (Math/sqrt (+ (* dx dx) (* dz dz)))
        n (max 1 (long (Math/ceil (/ d 0.5))))]
    (loop [i 1]
      (if (> i n)
        true
        (let [px (+ x (/ (* dx i) n))
              pz (+ z (/ (* dz i) n))
              ok? (loop [corners [[(- half) (- half)] [(- half) half]
                                  [half (- half)] [half half]]]
                    (if-let [[ox oz] (first corners)]
                      (let [cx (long (Math/floor (+ px (double ox))))
                            cz (long (Math/floor (+ pz (double oz))))]
                        (if (and (open? chunks template cx y cz)
                                 (supported? chunks template cx y cz))
                          (recur (rest corners))
                          false))
                      true))]
          (if ok? (recur (inc i)) false))))))

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

(defn find-path [chunks template start goal avoid-water?]
  (let [h (fn ^double [c] (dist c goal))]
    (loop [open   (sorted-set [(h start) start])
           closed #{}
           g      {start 0.0}
           came   {}
           best   start
           best-h (double (h start))
           n      0]
      (if-let [[_ cur :as entry] (first open)]
        (cond
          (= cur goal) (rebuild came cur)
          (>= n max-nodes) (when (not= best start) (rebuild came best))
          (closed cur) (recur (disj open entry) closed g came best best-h n)
          :else
          (let [closed (conj closed cur)
                [open g came best best-h]
                (reduce
                 (fn [[open g came best best-h :as acc] d]
                   (if-let [nb (step-cell chunks template avoid-water? cur d)]
                     (let [ng (+ (double (g cur)) (dist cur nb))]
                       (if (< ng (double (get g nb Double/MAX_VALUE)))
                         (let [nh (double (h nb))]
                           [(conj open [(+ ng nh) nb])
                            (assoc g nb ng)
                            (assoc came nb cur)
                            (if (< nh best-h) nb best)
                            (min nh best-h)])
                         acc))
                     acc))
                 [(disj open entry) g came best best-h]
                 dirs)]
            (recur open closed g came best (double best-h) (inc n))))
        (when (not= best start) (rebuild came best))))))
