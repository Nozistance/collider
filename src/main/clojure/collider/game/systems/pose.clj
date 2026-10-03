(ns collider.game.systems.pose
  "Player water state, swimming and pose."
  (:require [collider.game.entity :as entity]
            [collider.game.mode :as game-mode]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const fluid-margin 0.001)

(def ^:private ^:const fit-eps 1.0E-7)

(defn- floor ^long [^double c] (long (Math/floor c)))

(defn- st-at [chunks cx cy cz]
  (chunk/block-state chunks cx cy cz))

(defn- fits? [chunks e pose]
  (let [[half h] (entity/pose-box pose)
        p (:pos e) x (v/x p) y (v/y p) z (v/z p)
        half (double half) h (double h) d fit-eps]
    (phys/box-free? chunks
                    [(+ (- x half) d) (+ y d) (+ (- z half) d)
                     (- (+ x half) d) (- (+ y h) d) (- (+ z half) d)]
                    y (phys/context e))))

(defn- water-depth ^double [chunks pos pose]
  (let [[half h] (entity/pose-box pose) m fluid-margin
        from [(v/x pos) (+ (v/y pos) m) (v/z pos)]
        width (- (double half) m)
        height (- (double h) (* 2.0 m))]
    (liquid/fluid-height chunks from width height :water)))

(defn- water-at? [chunks cx cy cz]
  (and (chunk/in-range? (long cy))
       (block/water? (st-at chunks cx cy cz))))

(defn- eye-in-water? [chunks pos pose]
  (let [ey (+ (v/y pos) (entity/pose-eye pose))
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

(defn- fitting-pose [chunks e want]
  (cond
    (fits? chunks e want) want
    (fits? chunks e :crouching) :crouching
    :else :swimming))

(defn- pose-of [chunks e swim?]
  (let [want (desired-pose e swim?)]
    (cond
      (not (fits? chunks e :swimming)) (:pose e :standing)
      (game-mode/spectator? e) want
      :else (fitting-pose chunks e want))))

(defn- changes [world e]
  (let [chunks (:chunks world)
        pos (:pos e)
        pose (:pose e :standing)
        in? (pos? (water-depth chunks pos pose))
        under? (boolean (and (:eye-in-water? e) in?))
        fx (floor (v/x pos)) fy (floor (v/y pos)) fz (floor (v/z pos))
        feet? (water-at? chunks fx fy fz)
        swim? (swims? e in? under? feet?)]
    (cond-> {:in-water?     in? :under-water? under? :swimming? swim?
             :eye-in-water? (eye-in-water? chunks pos pose)
             :pose          (pose-of chunks e swim?)}
      (game-mode/spectator? e) (assoc :on-ground false))))

(defn player-deltas
  "Returns the deltas that set the water state and the pose of
  player p, an entry."
  [world [eid e]]
  (when (game-mode/ticks? (:chunks world) e)
    (let [same? (fn [[k vl]] (= vl (get e k)))
          m (into {} (remove same?) (changes world e))]
      (when (seq m) [[:merge-entity eid m]]))))
