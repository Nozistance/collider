(ns collider.game.experience
  "Player experience and the values of experience orbs."
  (:require [collider.num :as num :refer [f32 i32]]))

(set! *warn-on-reflection* true)

(def ^:private orb-sizes
  [2477 1237 617 307 149 73 37 17 7 3])

(defn orb-value
  "Returns the value of the largest orb that fits in amount."
  ^long [^long amount]
  (or (some #(when (>= amount (long %)) (long %)) orb-sizes) 1))

(defn orb-values
  "Returns the values of the orbs amount splits into, in the order
  they are made."
  [^long amount]
  (loop [n amount acc []]
    (if (pos? n)
      (let [v (orb-value n)] (recur (- n v) (conj acc v)))
      acc)))

(defn needed
  "Returns the points level needs to reach the next one."
  ^long [^long level]
  (i32 (cond (>= level 30) (+ 112 (* (- level 30) 9))
             (>= level 15) (+ 37 (* (- level 15) 5))
             :else (+ 7 (* level 2)))))

(defn account
  "Returns the experience of player e, who has lived lived ticks."
  [e ^long lived]
  {:level (long (:xp-level e 0))
   :progress (double (:xp-progress e 0.0))
   :total (long (:xp-total e 0)) :score (long (:score e 0))
   :up-at (long (:level-up-at e 0)) :lived lived :chimes []
   :dirty? false})

(defn player-fields
  "Returns the fields of the player that acc changed."
  [acc]
  (cond-> {:xp-level (:level acc) :xp-progress (:progress acc)
           :xp-total (:total acc) :score (:score acc)
           :level-up-at (:up-at acc)}
    (:dirty? acc) (assoc :xp-sent -1)))

(defn- chime-volume ^double [^long level]
  (let [v (if (> level 30) 1.0 (f32 (/ (f32 level) 30.0)))]
    (f32 (* v (f32 0.75)))))

(defn- chimes? [acc ^long amount]
  (let [level (long (:level acc))
        lived (f32 (- (f32 (:lived acc)) 100.0))]
    (and (pos? amount) (zero? (rem level 5))
         (< (f32 (:up-at acc)) lived))))

(defn- chimed [acc amount]
  (if (chimes? acc amount)
    (-> acc
        (update :chimes conj (chime-volume (:level acc)))
        (assoc :up-at (:lived acc)))
    acc))

(defn- saturated ^long [^long a ^long b]
  (max Integer/MIN_VALUE (min Integer/MAX_VALUE (+ a b))))

(defn- levels-given [acc ^long amount]
  (let [level (saturated (:level acc) amount)
        acc (if (neg? level)
              (assoc acc :level 0 :progress 0.0 :total 0)
              (assoc acc :level level))]
    (chimed acc amount)))

(defn- need ^double [acc] (f32 (needed (:level acc))))

(defn- raised [acc]
  (loop [acc acc]
    (if (>= (double (:progress acc)) 1.0)
      (let [p (f32 (* (f32 (- (double (:progress acc)) 1.0))
                      (need acc)))
            acc (levels-given acc 1)]
        (recur (assoc acc :progress (f32 (/ p (need acc))))))
      acc)))

(defn- rest-of ^double [acc ^double left]
  (f32 (+ 1.0 (f32 (/ left (need acc))))))

(defn- lowered [acc]
  (loop [acc acc]
    (if (neg? (double (:progress acc)))
      (let [left (f32 (* (double (:progress acc)) (need acc)))
            down? (pos? (long (:level acc)))
            acc (levels-given acc -1)]
        (recur (assoc acc :progress
                      (if down? (rest-of acc left) 0.0))))
      acc)))

(defn give-points
  "Returns acc after i points."
  [acc ^long i]
  (if (zero? i)
    acc
    (let [p (f32 (+ (double (:progress acc))
                    (f32 (/ (f32 i) (need acc)))))
          sum (i32 (+ (long (:total acc)) i))
          total (max 0 (min Integer/MAX_VALUE sum))]
      (-> acc
          (assoc :score (i32 (+ (long (:score acc)) i))
                 :progress p :total total :dirty? true)
          lowered
          raised))))

(defn give-levels
  "Returns acc after amount levels."
  [acc ^long amount]
  (if (zero? amount)
    acc
    (assoc (levels-given acc amount) :dirty? true)))

(defn set-points
  "Returns acc with the progress amount points make."
  [acc ^long amount]
  (let [limit (need acc)
        top (f32 (/ (f32 (- limit 1.0)) limit))
        p (max 0.0 (min top (f32 (/ (f32 amount) limit))))]
    (if (== p (double (:progress acc)))
      acc
      (assoc acc :progress p :dirty? true))))

(defn set-levels
  "Returns acc at level amount."
  [acc ^long amount]
  (if (== amount (long (:level acc)))
    acc
    (assoc acc :level amount :dirty? true)))

(defn points
  "Returns the points of the current level acc holds."
  ^long [acc]
  (num/floor (f32 (* (double (:progress acc)) (need acc)))))

(defn death-reward
  "Returns the points a dying player drops."
  ^long [e keep? spectator?]
  (if (or keep? spectator?)
    0
    (min 100 (i32 (* 7 (long (:xp-level e 0)))))))
