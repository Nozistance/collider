(ns collider.game.cost
  "The time the thunks of the tick took, kept outside the world.")

(set! *warn-on-reflection* true)

(def ^:private ^:const fork-ns 40000)

(def ^:private weights (atom {}))

(defn- weight ^long [k] (long (get @weights k 0)))

(defn- weighed! [k ^long ns]
  (swap! weights assoc k (quot (+ (* 3 (weight k)) ns) 4)))

(defn timed
  "Returns thunk f that also records how long it ran under key k."
  [k f]
  (fn []
    (let [t0 (System/nanoTime)
          r (f)]
      (weighed! k (- (System/nanoTime) t0))
      r)))

(defn heavy?
  "Returns true when the thunks under key k took long the last ticks."
  [k]
  (< fork-ns (weight k)))
