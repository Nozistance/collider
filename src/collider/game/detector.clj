(ns collider.game.detector
  "Observers of the finished tick."
  (:require [collider.game.out :as out]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.blocks.liquid :as liquid]))

(set! *warn-on-reflection* true)

(defn- water-at?
  "Returns true when the point is in water."
  [world [x y z]]
  (let [y (long (Math/floor (double y)))]
    (and (chunk/in-range? y)
         (= :water (liquid/liquid-class (chunk/chunks-get-block (:chunks world) gen/flat-chunk
                                                                [(long (Math/floor (double x))) y (long (Math/floor (double z)))]))))))

(defn- cm ^long [^double d] (Math/round (* d 100.0)))
(defn- moved
  "Returns how far a player moved this tick, nil when it did not."
  [world e e']
  (let [p (:pos e) p' (:pos e')]
    (when (and p p' (not (identical? p p')))
      (let [dx (- (v/x p') (v/x p)) dy (- (v/y p') (v/y p)) dz (- (v/z p') (v/z p))
            all (cm (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
            flat (cm (Math/sqrt (+ (* dx dx) (* dz dz))))]
        (cond
          (water-at? world [(v/x p') (+ (v/y p') 1.62) (v/z p')]) (when (pos? all) [:walk-under-water-one-cm all])
          (water-at? world [(v/x p') (v/y p') (v/z p')]) (when (pos? flat) [:walk-on-water-one-cm flat])
          (:on-ground e') (when (pos? flat)
                            [(cond (:sprinting? e') :sprint-one-cm (:sneaking? e') :crouch-one-cm :else :walk-one-cm) flat])
          :else (when (> flat 25) [:fly-one-cm flat]))))))

(defn- jumped? [e e']
  (and (:on-ground e) (not (:on-ground e'))
       (:pos e) (:pos e') (> (v/y (:pos e')) (v/y (:pos e)))))

(defn- counts
  "Returns the statistics a player earned this tick."
  [world e e']
  (cond-> [[:play-time 1] [:total-world-time 1] [:time-since-death 1]]
          (:sneaking? e') (conj [:sneak-time 1])
          (not (:sleeping e')) (conj [:time-since-rest 1])
          (and (:sleeping e') (not (:sleeping e))) (conj [:sleep-in-bed 1])
          (jumped? e e') (conj [:jump 1])
          (:landed e') (conj [:fall-one-cm (cm (double (:landed e')))])
          (moved world e e') (conj (moved world e e'))))

(defn- add-counts [stats pairs]
  (reduce (fn [m [k n]] (update m (keyword "custom" (name k)) (fnil + 0) (long n))) stats pairs))

(defn- snapshot [world]
  (into {}
        (keep (fn [[eid e]]
                (when (= :player (:type e))
                  [eid (select-keys e [:pos :on-ground :sleeping])])))
        (:entities world)))

(defn- stats-observe [world d]
  (let [prev (:observed world)]
    (concat
      (for [[eid e'] (:entities world) :when (= :player (:type e'))
            :let [e (get prev eid e')
                  pairs (counts world e e')]
            :when (seq pairs)]
        [:merge-entity eid {:stats (add-counts (or (:stats e') {}) pairs)}])
      (for [[tag eid] (:input d) :when (= :stats-request tag)
            :let [e (get-in world [:entities eid])] :when e]
        (out/to eid (out/stats (or (:stats e) {}))))
      [[:observed (snapshot world)]])))

(def channels [stats-observe])

(defn observe
  "Returns the deltas the observers draw from the finished tick."
  [world d]
  (into [] (mapcat (fn [c] (c world d))) channels))
