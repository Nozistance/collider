(ns collider.world.space.sight
  "Whether blocks stand between two points: the clip of a segment
  against the collision shapes of the blocks it crosses."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const eps 1.0E-7)

(def ^:private axes [0 1 2])

(defn- lerp ^double [^double t ^double a ^double b]
  (+ a (* t (- b a))))

(defn- frac ^double [^double v] (- v (Math/floor v)))

(defn- unit ^double [^long base c] (+ base (/ (double c) 16.0)))

(defn- boxes-at [chunks [x y z :as cell]]
  (for [[a b c d e f] (block/collision-boxes (chunk/at chunks cell))]
    [[(unit x a) (unit y b) (unit z c)]
     [(unit x d) (unit y e) (unit z f)]]))

(defn- inside? [p [lo hi]]
  (every? #(and (<= (double (lo %)) (double (p %)))
                (< (double (p %)) (double (hi %))))
          axes))

(defn- near? [^double c lo hi]
  (< (- (double lo) eps) c (+ (double hi) eps)))

(defn- face-hit?
  "AABB.clipPoint of the face at plane across axis a."
  [from v a plane [lo hi]]
  (let [[b c] (remove #{a} axes)
        t (/ (- (double plane) (double (from a))) (double (v a)))
        at #(+ (double (from %)) (* t (double (v %))))]
    (and (< 0.0 t 1.0)
         (near? (at b) (lo b) (hi b))
         (near? (at c) (lo c) (hi c)))))

(defn- box-hit? [from v [lo hi :as box]]
  (some (fn [a]
          (let [d (double (v a))]
            (cond
              (> d eps) (face-hit? from v a (lo a) box)
              (< d (- eps)) (face-hit? from v a (hi a) box))))
        axes))

(defn- cell-hit? [chunks from v cell]
  (let [probe (mapv #(+ (double (from %)) (* 0.001 (double (v %))))
                    axes)]
    (some #(or (inside? probe %) (box-hit? from v %))
          (boxes-at chunks cell))))

(defn- first-t ^double [^long sg ^double td ^double f]
  (* td (if (pos? sg) (- 1.0 (frac f)) (frac f))))

(defn- stretched [a b]
  (mapv #(lerp -1.0E-7 (double (a %)) (double (b %))) axes))

(defn- step-len ^double [^long sg ^double d]
  (if (zero? sg) Double/MAX_VALUE (/ (double sg) d)))

(defn- walk
  "BlockGetter.traverseBlocks from from to to: the first cell, the
  step signs, the step lengths and the next crossings."
  [from to]
  (let [f (stretched from to)
        d (mapv - (stretched to from) f)
        sg (mapv #(long (Math/signum (double %))) d)
        td (mapv #(step-len (sg %) (d %)) axes)]
    {:cell (mapv #(long (Math/floor (double %))) f) :sg sg :td td
     :t (mapv #(first-t (sg %) (td %) (f %)) axes)}))

(defn- next-axis ^long [[tx ty tz]]
  (let [tx (double tx) ty (double ty) tz (double tz)]
    (if (< tx ty) (if (< tx tz) 0 2) (if (< ty tz) 1 2))))

(defn- crossed? [chunks from v {:keys [sg td] :as w}]
  (loop [cell (:cell w) t (:t w)]
    (cond
      (cell-hit? chunks from v cell) true
      (not-any? #(<= (double %) 1.0) t) false
      :else (let [a (next-axis t)]
              (recur (update cell a + (sg a))
                     (update t a + (td a)))))))

(defn clear?
  "Tells whether the segment from from to to meets no collision
  shape of the blocks it crosses."
  [chunks from to]
  (or (= (vec from) (vec to))
      (let [v (mapv #(- (double (to %)) (double (from %))) axes)]
        (not (crossed? chunks from v (walk from to))))))
