(ns collider.game.detector
  "Observers of the finished tick."
  (:require [collider.game.out :as out]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- cm ^long [^double sq]
  (Math/round (float (* (double (float (Math/sqrt sq))) 100.0))))

(defn- flat-cm ^long [^double dx ^double dz]
  (cm (+ (* dx dx) (* dz dz))))

(defn- full-cm ^long [^double dx ^double dy ^double dz]
  (cm (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- when-cm [k ^long n]
  (when (pos? n) [k n]))

(defn- ground-stat [m ^long n]
  (when (pos? n)
    [(cond (:sprinting? m) :sprint-one-cm
           (= :crouching (:pose m)) :crouch-one-cm
           :else :walk-one-cm)
     n]))

(defn- distance-stat [m ^double dx ^double dy ^double dz]
  (cond
    (:swimming? m) (when-cm :swim-one-cm (full-cm dx dy dz))
    (:eye-in-water? m)
    (when-cm :walk-under-water-one-cm (full-cm dx dy dz))
    (:in-water? m) (when-cm :walk-on-water-one-cm (flat-cm dx dz))
    (:climbing? m)
    (when (pos? dy) [:climb-one-cm (Math/round (* dy 100.0))])
    (:on-ground m) (ground-stat m (flat-cm dx dz))
    :else (let [n (flat-cm dx dz)] (when (> n 25) [:fly-one-cm n]))))

(defn- move-stats [m]
  (let [p (:from m) p' (:to m)
        dx (- (v/x p') (v/x p)) dy (- (v/y p') (v/y p))
        dz (- (v/z p') (v/z p))]
    (cond-> []
      (:jump? m) (conj [:jump 1])
      (:landed m) (conj [:landed (:landed m)])
      (not (and (zero? dx) (zero? dy) (zero? dz)))
      (conj (distance-stat m dx dy dz)))))

(defn- may-fly? [_e]
  true)

(defn- fall-stat [e [k n :as pair]]
  (if (= :landed k)
    (when (and (not (may-fly? e)) (>= (double n) 2.0))
      [:fall-one-cm (Math/round (* (double n) 100.0))])
    pair))

(defn- tick-stats [e was-sleeping?]
  (cond-> [[:play-time 1] [:total-world-time 1]]
    (pos? (double (:health e 20.0))) (conj [:time-since-death 1])
    (:sneaking? e) (conj [:sneak-time 1])
    (not (:sleeping e)) (conj [:time-since-rest 1])
    (and (:sleeping e) (not was-sleeping?)) (conj [:sleep-in-bed 1])))

(defn- add-counts [stats pairs]
  (reduce (fn [m [k n]]
            (update m (keyword "custom" (name k))
                    (fnil + 0) (long n)))
          stats pairs))

(defn- player-stats [world moves eid e]
  (let [was (get-in world [:observed eid :sleeping])
        moved (mapcat move-stats (get moves eid))]
    (into (tick-stats e was)
          (keep #(fall-stat e %))
          moved)))

(defn- players [world]
  (filter #(= :player (:type (val %))) (:entities world)))

(defn- award [world _]
  (let [moves (group-by :eid (:moves world))]
    (conj
      (mapv (fn [[eid e]]
              [:merge-entity eid
               {:stats (add-counts
                         (or (:stats e) {})
                         (player-stats world moves eid e))}])
            (players world))
      [:observed (into {} (map (fn [[eid e]]
                                 [eid (select-keys e [:sleeping])]))
                       (players world))])))

(defn- answer [world d]
  (for [[tag eid] (:input d) :when (= :stats-request tag)
        :let [e (get-in world [:entities eid])] :when e]
    (out/to eid (out/stats (or (:stats e) {})))))

(def channels [award answer])

(defn observe
  "Returns the deltas the observers draw from the finished tick."
  [world d]
  (into [] (mapcat (fn [c] (c world d))) channels))
