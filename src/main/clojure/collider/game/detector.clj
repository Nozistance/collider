(ns collider.game.detector
  "Observers of the finished tick."
  (:require [collider.game.deltas :as deltas]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.level :as level]
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

(defn- fall-stat [e [k n :as pair]]
  (if (= :landed k)
    (when (and (not (game-mode/may-fly? e)) (>= (double n) 2.0))
      [:fall-one-cm (Math/round (* (double n) 100.0))])
    pair))

(defn- ticked-stats [e]
  (cond-> [[:play-time 1] [:total-world-time 1]]
    (pos? (double (:health e 20.0))) (conj [:time-since-death 1])
    (:sneaking? e) (conj [:sneak-time 1])
    (not (:sleeping e)) (conj [:time-since-rest 1])))

(defn- tick-stats [chunks e was-sleeping?]
  (cond-> []
    (game-mode/ticks? chunks e) (into (ticked-stats e))
    (and (:sleeping e) (not was-sleeping?)) (conj [:sleep-in-bed 1])))

(def ^:private custom-keys
  (into {}
        (map (fn [k] [k (keyword "custom" (name k))]))
        [:play-time :total-world-time :time-since-death :sneak-time
         :time-since-rest :sleep-in-bed :jump :fall-one-cm
         :sprint-one-cm :crouch-one-cm :walk-one-cm :swim-one-cm
         :walk-under-water-one-cm :walk-on-water-one-cm :climb-one-cm
         :fly-one-cm]))

(defn- custom-key [k]
  (or (custom-keys k) (keyword "custom" (name k))))

(defn- added [m k n]
  (assoc m k (+ (or (get m k) 0) (long n))))

(defn- add-counts [stats pairs]
  (reduce (fn [m [k n]] (added m (custom-key k) n)) stats pairs))

(defn- player-stats [world moves eid e]
  (let [was (get-in world [:observed eid :sleeping])
        moved (mapcat move-stats (get moves eid))]
    (into (tick-stats (:chunks world) e was)
          (keep #(fall-stat e %))
          moved)))

(defn- add-awards [stats awards]
  (reduce (fn [m [k n]] (added m k n)) stats awards))

(defn- players [world]
  (level/of-types world [:player]))

(defn- player-delta [world moves [eid e]]
  (let [counted (player-stats world moves eid e)
        stats (-> (or (:stats e) {})
                  (add-counts counted)
                  (add-awards (:awards e)))]
    [:merge-entity eid
     (cond-> {:stats stats} (:awards e) (assoc :awards nil))]))

(defn- seen-of [e] (select-keys e [:sleeping]))

(defn- observed [ps]
  (into {} (map (fn [[eid e]] [eid (seen-of e)])) ps))

(defn- still? [world ps]
  (let [was (:observed world)]
    (and (some? was) (= (count was) (count ps))
         (every? (fn [[eid e]] (= (get was eid ::none) (seen-of e)))
                 ps))))

(defn- award [world _]
  (let [moves (group-by :eid (get-in world [:input :moves]))
        ps (players world)
        ds (mapv #(player-delta world moves %) ps)]
    (if (still? world ps)
      ds
      (conj ds [:observed (observed ps)]))))

(defn- answer [world d]
  (for [[tag eid] (:input d) :when (= :stats-request tag)
        :let [e (get-in world [:entities eid])] :when e]
    (out/to eid (out/stats (or (:stats e) {})))))

(def channels [award answer])

(defn observe
  "Returns what the detector channels make of the tick."
  {:wake :always}
  [world d]
  (deltas/of-vec (into [] (mapcat (fn [c] (c world d))) channels)))
