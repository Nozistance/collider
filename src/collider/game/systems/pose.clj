(ns collider.game.systems.pose
  "Player water state, swimming and pose."
  (:require [collider.game.state :as state]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const fluid-margin 0.001)
(def ^:private ^:const fit-eps 1.0E-7)
(def ^:private pose-box
  {:standing [0.3 1.8] :crouching [0.3 1.5]
   :swimming [0.3 0.6] :sleeping [0.1 0.2]})
(def ^:private pose-eye
  {:standing 1.62 :crouching 1.27 :swimming 0.4 :sleeping 0.2})

(defn- floor ^long [^double c] (long (Math/floor c)))

(defn- axis-hit? [c b0 b1 lo hi]
  (and (< (double lo) (+ (double c) (/ (double b1) 16.0)))
       (> (double hi) (+ (double c) (/ (double b0) 16.0)))))

(defn- box-hit? [b cx cy cz lo hi]
  (and (axis-hit? cx (nth b 0) (nth b 3) (nth lo 0) (nth hi 0))
       (axis-hit? cy (nth b 1) (nth b 4) (nth lo 1) (nth hi 1))
       (axis-hit? cz (nth b 2) (nth b 5) (nth lo 2) (nth hi 2))))

(defn- st-at [chunks cx cy cz]
  (chunk/chunks-get-block chunks cx cy cz))

(defn- cell-hit? [chunks cx cy cz lo hi]
  (let [st (st-at chunks cx cy cz)]
    (and (chunk/in-range? (long cy))
         (block/solid? st)
         (or (block/full-cube? st)
             (boolean (some #(box-hit? % cx cy cz lo hi)
                            (block/collision-boxes st)))))))

(defn- hits? [chunks lo hi]
  (let [c (fn [v i] (floor (double (nth v (long i)))))]
    (boolean
      (some (fn [[cx cy cz]] (cell-hit? chunks cx cy cz lo hi))
            (for [cx (range (c lo 0) (inc (c hi 0)))
                  cy (range (c lo 1) (inc (c hi 1)))
                  cz (range (c lo 2) (inc (c hi 2)))]
              [cx cy cz])))))

(defn- fits? [chunks pos pose]
  (let [[half h] (pose-box pose)
        x (v/x pos) y (v/y pos) z (v/z pos)
        half (double half) h (double h) e fit-eps]
    (not (hits? chunks
                [(+ (- x half) e) (+ y e) (+ (- z half) e)]
                [(- (+ x half) e) (- (+ y h) e) (- (+ z half) e)]))))

(defn- water-depth ^double [chunks pos pose]
  (let [[half h] (pose-box pose) m fluid-margin]
    (liquid/fluid-height chunks
                         [(v/x pos) (+ (v/y pos) m) (v/z pos)]
                         (- (double half) m)
                         (- (double h) (* 2.0 m))
                         :water)))

(defn- water-at? [chunks cx cy cz]
  (and (chunk/in-range? (long cy))
       (= :water (liquid/liquid-class (st-at chunks cx cy cz)))))

(defn- eye-in-water? [chunks pos pose]
  (let [ey (+ (v/y pos) (double (pose-eye pose)))
        cx (floor (v/x pos)) cy (floor ey) cz (floor (v/z pos))]
    (boolean
      (when (water-at? chunks cx cy cz)
        (when-let [h (liquid/fluid-height-of
                       chunks [cx cy cz]
                       (st-at chunks cx cy cz) nil)]
          (<= ey (+ (long cy) (double h))))))))

(defn- swims? [e in-water? under-water? feet-water?]
  (cond
    (:flying e) false
    (:swimming? e) (boolean (and (:sprinting? e) in-water?))
    :else (boolean (and (:sprinting? e) under-water? feet-water?))))

(defn- desired-pose [e swim?]
  (cond
    (:sleeping e) :sleeping
    swim? :swimming
    (and (:sneaking? e) (not (:flying e))) :crouching
    :else :standing))

(defn- fitting-pose [chunks pos want]
  (cond
    (fits? chunks pos want) want
    (fits? chunks pos :crouching) :crouching
    :else :swimming))

(defn- pose-of [chunks pos e swim?]
  (if (fits? chunks pos :swimming)
    (fitting-pose chunks pos (desired-pose e swim?))
    (:pose e :standing)))

(defn- changes [world e]
  (let [chunks (:chunks world)
        pos (:pos e)
        pose (:pose e :standing)
        in? (pos? (water-depth chunks pos pose))
        under? (boolean (and (:eye-in-water? e) in?))
        feet? (water-at? chunks (floor (v/x pos))
                         (floor (v/y pos)) (floor (v/z pos)))
        swim? (swims? e in? under? feet?)]
    {:in-water?     in? :under-water? under? :swimming? swim?
     :eye-in-water? (eye-in-water? chunks pos pose)
     :pose          (pose-of chunks pos e swim?)}))

(defn- player-deltas [world [eid e]]
  (let [same? (fn [[k vl]] (= vl (get e k)))
        m (into {} (remove same?) (changes world e))]
    (when (seq m) [[:merge-entity eid m]])))

(defn pose [world _]
  [#(into [] (mapcat (fn [entry] (player-deltas world entry)))
          (state/player-entries world))])
