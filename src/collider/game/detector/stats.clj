(ns collider.game.detector.stats
  (:require [collider.game.out :as out]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]))

(set! *warn-on-reflection* true)

(defn- water-at? [world [x y z]]
  (let [y (long (Math/floor (double y)))]
    (and (chunk/in-range? y)
         (= :water (liquid/liquid-class (chunk/chunks-get-block (:chunks world) gen/flat-chunk
                                                                [(long (Math/floor (double x))) y (long (Math/floor (double z)))]))))))

(defn- cm ^long [^double d] (Math/round (* d 100.0)))
(defn- moved [world e e']
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

(defn- counts [world e e']
  (cond-> [[:play-time 1] [:total-world-time 1] [:time-since-death 1]]
    (:sneaking? e')                          (conj [:crouch-time 1])
    (not (:sleeping e'))                     (conj [:time-since-rest 1])
    (and (:sleeping e') (not (:sleeping e))) (conj [:sleep-in-bed 1])
    (jumped? e e')                           (conj [:jump 1])
    (:landed e')                             (conj [:fall-one-cm (cm (double (:landed e')))])
    (moved world e e')                       (conj (moved world e e'))))

(defn- add-counts [stats pairs]
  (reduce (fn [m [k n]] (update m [:custom k] (fnil + 0) (long n))) stats pairs))

(defn observe [world events _deltas world']
  (concat
   (for [[eid e'] (:entities world') :when (= :player (:type e'))
         :let [e (get-in world [:entities eid] e')
               pairs (counts world e e')]
         :when (seq pairs)]
     [:merge-entity eid {:stats (add-counts (or (:stats e') {}) pairs)}])
   (for [[tag eid] events :when (= :stats-request tag)
         :let [e (get-in world' [:entities eid])] :when e]
     (out/to eid (out/stats (or (:stats e) {}))))))
