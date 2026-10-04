(ns collider.game.mob.clock
  "The clocks of a mob, which run only in the ticks the mob lives.
  A mob out of the active chunks does not tick.")

(set! *warn-on-reflection* true)

(def ^:private deadlines
  [:say-tick :jump-cd :panic-until :baby-until :love-until
   :breed-ready-at :tempt-cooldown-until :follow-at
   :stick-cooldown-until :egg-at :age-lock-at :hurt-by-player-until])

(def ^:private inner [[:task :until] [:look :until] [:body :at]])

(defn- later [m k ^long dt]
  (let [v (get m k)]
    (if (or (nil? v) (= Long/MAX_VALUE v))
      m
      (assoc m k (+ (long v) dt)))))

(defn- later-in [m [k j] dt]
  (if-let [o (get m k)] (assoc m k (later o j dt)) m))

(defn frozen?
  "Returns true when mob e has not ticked since some tick."
  [e]
  (some? (:frozen-at e)))

(defn frozen
  "Returns the changes that stop the clocks of a mob at tick t."
  [t]
  {:frozen-at t})

(defn thawed
  "Returns the changes that start the clocks of frozen mob e again
  at tick t. Each deadline moves on by the ticks it stood still."
  [e t]
  (let [dt (- (long t) (long (:frozen-at e)))
        ks (into deadlines (map first) inner)
        m (into {} (filter (comp some? val)) (select-keys e ks))
        m (reduce #(later %1 %2 dt) m deadlines)]
    (assoc (reduce #(later-in %1 %2 dt) m inner) :frozen-at nil)))
