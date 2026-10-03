(ns collider.game.hanging
  "Paintings and item frames, their boxes and what holds them up."
  (:require [collider.data :as data]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def types
  "The types of the hanging entities."
  #{:painting :item-frame :glow-item-frame})

(def frames
  "The types of the item frames."
  #{:item-frame :glow-item-frame})

(def ^:private ^:const check-period 101)

(def ^:private ^:const first-check 100)

(defn first-check-at
  "Returns the tick of the first survival check of a hanging entity
  that starts ticking at tick t."
  ^long [t]
  (+ (long t) first-check))

(defn next-check-at
  "Returns the tick of the check after the one at tick t."
  ^long [t]
  (+ (long t) check-period))

(def ^:private ^:table variant-sizes
  (delay (into {}
               (map (fn [[id m]]
                      [(data/kebab id) [(m "width") (m "height")]]))
               (data/pack "painting_variant"))))

(def ^:private ^:table placeable
  (delay (data/tag-values "painting_variant" "placeable")))

(defn default-variant
  "Returns the variant a painting shows before any is set."
  []
  (data/entry-name "painting_variant" 0))

(def ^:private flat 0.0625)

(def ^:private to-wall 0.46875)

(def ^:private frame-side 0.75)

(defn- wall-center [[x y z] facing]
  (let [[dx dy dz] (dir/offset facing)]
    [(+ (+ (long x) 0.5) (* (long dx) (- to-wall)))
     (+ (+ (long y) 0.5) (* (long dy) (- to-wall)))
     (+ (+ (long z) 0.5) (* (long dz) (- to-wall)))]))

(defn- of-size [[x y z] ^double sx ^double sy ^double sz]
  [(- (double x) (/ sx 2.0)) (- (double y) (/ sy 2.0))
   (- (double z) (/ sz 2.0)) (+ (double x) (/ sx 2.0))
   (+ (double y) (/ sy 2.0)) (+ (double z) (/ sz 2.0))])

(defn- side ^double [^long n] (if (even? n) 0.5 0.0))

(defn- unsigned-zero ^double [^double a] (+ a 0.0))

(defn- painting-box [pos facing variant]
  (let [[w h] (@variant-sizes variant)
        [lx _ lz] (dir/offset (dir/counter-clockwise facing))
        [x y z] (wall-center pos facing)
        s (side w)
        c [(unsigned-zero (+ x (* (long lx) s))) (+ y (side h))
           (unsigned-zero (+ z (* (long lz) s)))]
        ax (dir/axis facing)]
    (of-size c (if (= :x ax) flat (double w)) (double h)
             (if (= :z ax) flat (double w)))))

(defn- frame-box [pos facing]
  (let [ax (dir/axis facing)
        size (fn [a] (if (= a ax) flat frame-side))]
    (of-size (wall-center pos facing) (size :x) (size :y) (size :z))))

(defn box
  "Returns the box [x0 y0 z0 x1 y1 z1] of the hanging entity e."
  [e]
  (if (= :painting (:type e))
    (painting-box (:block-pos e) (:facing e) (:variant e))
    (frame-box (:block-pos e) (:facing e))))

(defn- mid ^double [^double a ^double b] (+ a (* 0.5 (- b a))))

(defn center
  "Returns the middle of box b, where its entity stands."
  [[x0 y0 z0 x1 y1 z1]]
  [(mid x0 x1) (mid y0 y1) (mid z0 z1)])

(def ^:private index-2d {:south 0 :west 1 :north 2 :east 3})

(defn- turned [e]
  (let [f (:facing e)]
    (case f
      :up (assoc e :yaw 0.0 :pitch -90.0)
      :down (assoc e :yaw 0.0 :pitch 90.0)
      (assoc e :yaw (* 90.0 (long (index-2d f))) :pitch 0.0))))

(defn placed
  "Returns hanging entity e set at its block and facing. Its box gives
  its position, its facing its turn."
  [e]
  (turned (assoc e :pos (center (box e)))))

(defn- base [type pos facing t]
  {:type type :block-pos pos :facing facing :vel [0.0 0.0 0.0]
   :on-ground false :check-at (first-check-at t)})

(defn frame
  "Returns an empty item frame of type on the block at pos, facing
  away from its wall, that starts ticking at tick t."
  [type pos facing t]
  (placed (assoc (base type pos facing t) :rotation 0)))

(defn- painting-of [pos facing variant t]
  (placed (assoc (base :painting pos facing t) :variant variant)))

(defn- blocks? [eid e [oid o]]
  (and (or (= (:facing o) (:facing e))
           (and (= :painting (:type e)) (= (:type o) (:type e))))
       (not= oid eid)))

(defn- coexists? [eid e b others]
  (not-any? (fn [entry]
              (and (blocks? eid e entry)
                   (v/boxes-meet? b (box (val entry)))))
            others))

(defn- holds? [^long st vertical?]
  (or (block/legacy-solid? st)
      (and (not vertical?)
           (contains? #{:repeater :comparator} (block/type-of st)))))

(defn- frame-held? [chunks e]
  (let [f (:facing e)
        wall (mapv - (:block-pos e) (dir/offset f))]
    (holds? (chunk/at chunks wall)
            (= :y (dir/axis f)))))

(defn- cells [^double lo ^double hi]
  (range (long (Math/floor lo)) (inc (long (Math/floor hi)))))

(defn- support-box [e b]
  (let [[dx dy dz] (dir/offset (:facing e))
        m (fn [i ^long d] (+ (double (nth b i)) (* d -0.5)))
        e7 1.0E-7]
    [(+ (m 0 dx) e7) (+ (m 1 dy) e7) (+ (m 2 dz) e7)
     (- (m 3 dx) e7) (- (m 4 dy) e7) (- (m 5 dz) e7)]))

(defn- painting-held? [chunks e b]
  (let [[x0 y0 z0 x1 y1 z1] (support-box e b)]
    (every? (fn [[x y z]]
              (holds? (chunk/at chunks [x y z]) false))
            (for [x (cells x0 x1) y (cells y0 y1) z (cells z0 z1)]
              [x y z]))))

(defn- held? [chunks e b]
  (if (= :painting (:type e))
    (painting-held? chunks e b)
    (frame-held? chunks e)))

(defn survives?
  "Returns true when hanging entity e may stay where it hangs among
  the others of its level, by eid. Eid is its own, or nil when it is
  not placed yet."
  [chunks eid e others]
  (let [b (box e)]
    (and (phys/box-free? chunks b (nth (:pos e) 1))
         (held? chunks e b)
         (coexists? eid e b others))))

(defn- area ^long [v]
  (let [[w h] (@variant-sizes v)] (* (long w) (long h))))

(defn painting
  "Returns a painting on the block at pos facing away from its wall,
  the largest that fits there, picked by roll among those alike, or
  nil when none fits. It starts ticking at tick t."
  [chunks others pos facing t roll]
  (let [on (fn [v] (painting-of pos facing v t))
        fits (filterv #(survives? chunks nil (on %) others)
                      @placeable)
        top (reduce max 0 (map area fits))
        best (filterv #(= top (area %)) fits)
        n (count best)]
    (when (pos? n)
      (painting-of pos facing
                   (nth best (long (* n (double roll)))) t))))
