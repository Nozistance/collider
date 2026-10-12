(ns collider.world.space.crossed
  "The cells a box crosses on its way, in the order a body meets them."
  (:require [collider.num :as num]))

(set! *warn-on-reflection* true)

(def ^:private eps (double (float 1.0E-5)))

(def ^:private still-sq (double (num/fmul (float 1.0E-5) (float 1.0E-5))))

(defn- step-of ^long [^double a] (if (>= a 0.0) 1 -1))

(defn- sign ^long [^double a]
  (cond (zero? a) 0 (> a 0.0) 1 :else -1))

(defn- frac ^double [^double a] (- a (double (num/floor a))))

(defn- between-corners
  "Returns the cells from corner a to corner b, first the one that
  lies against direction d, walking along y first."
  [[ax ay az] [bx by bz] [dx dy dz]]
  (let [lo [(min ax bx) (min ay by) (min az bz)]
        hi [(max ax bx) (max ay by) (max az bz)]
        d [dx dy dz]
        start (mapv (fn [i] (if (>= (double (d i)) 0.0) (lo i) (hi i)))
                    [0 1 2])
        axes (if (< (Math/abs (double dx)) (Math/abs (double dz)))
               [1 2 0]
               [1 0 2])
        [a b c] axes
        unit (fn [i] (step-of (d i)))
        span (fn [i] (- (long (hi i)) (long (lo i))))
        at (fn [i j k]
             (mapv (fn [n]
                     (+ (long (start n))
                        (if (= n a) (* (unit a) i) 0)
                        (if (= n b) (* (unit b) j) 0)
                        (if (= n c) (* (unit c) k) 0)))
                   [0 1 2]))]
    (for [i (range (inc (span a))) j (range (inc (span b)))
          k (range (inc (span c)))]
      (at i j k))))

(defn- floors [x y z] [(num/floor x) (num/floor y) (num/floor z)])

(defn- furthest-corner [[dx dy dz]]
  (let [x (Math/abs (double dx)) y (Math/abs (double dy))
        z (Math/abs (double dz))
        xs (step-of dx) ys (step-of dy) zs (step-of dz)]
    (cond (and (<= x y) (<= x z)) [(- xs) (- zs) ys]
          (<= y z) [zs (- ys) (- xs)]
          :else [(- ys) xs (- zs)])))

(defn- clip-axis
  [^doubles s da db dc point min-b max-b min-c max-c fa fb fc]
  (let [da (double da) t (/ (- (double point) (double fa)) da)
        pb (+ (double fb) (* t (double db)))
        pc (+ (double fc) (* t (double dc)))]
    (when (and (< 0.0 t) (< t (aget s 0))
               (< (- (double min-b) 1.0E-7) pb)
               (< pb (+ (double max-b) 1.0E-7))
               (< (- (double min-c) 1.0E-7) pc)
               (< pc (+ (double max-c) 1.0E-7)))
      (aset s 0 t))))

(defn box-clip
  "Returns where the segment from f to t first enters box b, or nil
  when it misses it."
  [[x0 y0 z0 x1 y1 z1] [fx fy fz] [tx ty tz]]
  (let [s (double-array [1.0]) hit (boolean-array 1)
        dx (- (double tx) (double fx)) dy (- (double ty) (double fy))
        dz (- (double tz) (double fz))
        mark (fn [r] (when r (aset hit 0 true)))]
    (cond (> dx 1.0E-7)
          (mark (clip-axis s dx dy dz x0 y0 y1 z0 z1 fx fy fz))
          (< dx -1.0E-7)
          (mark (clip-axis s dx dy dz x1 y0 y1 z0 z1 fx fy fz)))
    (cond (> dy 1.0E-7)
          (mark (clip-axis s dy dz dx y0 z0 z1 x0 x1 fy fz fx))
          (< dy -1.0E-7)
          (mark (clip-axis s dy dz dx y1 z0 z1 x0 x1 fy fz fx)))
    (cond (> dz 1.0E-7)
          (mark (clip-axis s dz dx dy z0 x0 x1 y0 y1 fz fx fy))
          (< dz -1.0E-7)
          (mark (clip-axis s dz dx dy z1 x0 x1 y0 y1 fz fx fy)))
    (when (aget hit 0)
      (let [k (aget s 0)]
        [(+ (double fx) (* k dx)) (+ (double fy) (* k dy))
         (+ (double fz) (* k dz))]))))

(defn- clip [x y z f t]
  (let [x (double x) y (double y) z (double z)]
    (box-clip [x y z (+ x 1.0) (+ y 1.0) (+ z 1.0)] f t)))

(defn- clamp ^double [^double a ^double lo ^double hi]
  (if (< a lo) lo (Math/min a hi)))

(defn- inside ^double [^double a ^long c]
  (clamp a (+ (double c) eps) (- (+ (double c) 1.0) eps)))

(defn- along-travel
  "Returns the cells the far corner of box b sweeps on its way by
  move d, each with its step, and the number of steps."
  [[x0 y0 z0 x1 y1 z1] d]
  (let [size [(- (double x1) (double x0)) (- (double y1) (double y0))
              (- (double z1) (double z0))]
        dir (furthest-corner d)
        mid [(+ (double x0) (* 0.5 (- (double x1) (double x0))))
             (+ (double y0) (* 0.5 (- (double y1) (double y0))))
             (+ (double z0) (* 0.5 (- (double z1) (double z0))))]
        to (mapv (fn [i] (+ (double (mid i))
                            (* (* (double (size i)) 0.5)
                               (double (dir i)))))
                 [0 1 2])
        from (mapv - to d)
        sg (mapv sign d)
        delta (mapv (fn [i] (if (zero? (long (sg i)))
                              Double/MAX_VALUE
                              (/ (double (sg i)) (double (d i)))))
                    [0 1 2])
        t0 (mapv (fn [i] (* (double (delta i))
                            (if (pos? (long (sg i)))
                              (- 1.0 (frac (from i)))
                              (frac (from i)))))
                 [0 1 2])]
    (loop [c (apply floors from) t t0 n 0 out []]
      (if-not (some #(<= (double %) 1.0) t)
        [out n]
        (let [[tx ty tz] t
              i (cond (< tx ty) (if (< tx tz) 0 2)
                      (< ty tz) 1
                      :else 2)
              c (update c i + (sg i))
              t (update t i + (delta i))
              [cx cy cz] c]
          (if-let [hit (clip cx cy cz from to)]
            (let [n (inc n)
                  far (mapv (fn [k]
                              (num/floor
                                (- (inside (hit k) (c k))
                                   (* (double (size k))
                                      (double (dir k))))))
                            [0 1 2])]
              (recur c t n
                     (into out (map #(conj % n)) (between-corners c far d))))
            (recur c t n out)))))))

(defn- between
  "Returns the cells of box b, x first."
  [[x0 y0 z0 x1 y1 z1]]
  (let [[ax ay az] (floors x0 y0 z0) [bx by bz] (floors x1 y1 z1)]
    (for [z (range az (inc bz)) y (range ay (inc by))
          x (range ax (inc bx))]
      [x y z 0])))

(defn- fresh [seen cells]
  (reduce (fn [[seen out] [x y z :as c]]
            (let [k [x y z]]
              (if (contains? seen k)
                [seen out]
                [(conj seen k) (conj out c)])))
          [seen []] cells))

(defn cells
  "Returns the cells box b crosses on its way from point from to point
  to, b being where it ends, as [x y z step] in the order it meets
  them."
  [from to [x0 y0 z0 x1 y1 z1 :as b]]
  (let [d (mapv - to from)]
    (if (< (reduce + (map * d d)) still-sq)
      (vec (between b))
      (let [back [(- (double x0) (double (d 0)))
                  (- (double y0) (double (d 1)))
                  (- (double z0) (double (d 2)))
                  (- (double x1) (double (d 0)))
                  (- (double y1) (double (d 1)))
                  (- (double z1) (double (d 2)))]
            first-cells (map #(conj % 0)
                             (between-corners (apply floors (subvec back 0 3))
                                      (apply floors (subvec back 3 6)) d))
            [seen out] (fresh #{} first-cells)
            [path n] (along-travel b d)
            [seen out] (let [[s o] (fresh seen path)] [s (into out o)])
            last-cells (map #(conj % (inc (long n)))
                            (between-corners (floors x0 y0 z0) (floors x1 y1 z1) d))]
        (into out (second (fresh seen last-cells)))))))
