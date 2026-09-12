(ns collider.world.blocks.moss
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def ^:private six {:up [0 1 0] :down [0 -1 0] :north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})
(def ^:private opposite {:up :down :down :up :north :south :south :north :west :east :east :west})
(def wall-sides [:north :east :south :west])
(defn- at ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks gen/flat-chunk p) -1))

(defn- up [p] (mapv + p [0 1 0]))
(defn- down [p] (mapv + p [0 -1 0]))
(defn- with ^long [^long st k v]
  (block/state (block/block-of st) (assoc (block/props-of st) k v)))

(defn- attachable? [chunks p dir]
  (let [n (at chunks (mapv + p (six dir)))]
    (and (pos? n) (block/face-sturdy? n (opposite dir)))))

(defn- carpet-at? [chunks p dir pred]
  (let [n (at chunks p)]
    (and (= :pale-moss-carpet (block/block-of n)) (pred (block/props-of n) dir))))

(defn- carpet-side [chunks p st dir create?]
  (let [st (long st) props (block/props-of st)
        side (if (attachable? chunks p dir) (if create? :low (get props dir)) :none)]
    (cond
      (not= :low side) side
      (carpet-at? chunks (up p) dir (fn [m d] (and (not= :none (get m d)) (= :false (:bottom m))))) :tall
      (and (= :false (:bottom props)) (carpet-at? chunks (down p) dir (fn [m d] (= :none (get m d))))) :none
      :else :low)))

(defn carpet-updated ^long [chunks p ^long st create-sides?]
  (let [create? (or create-sides? (= :true (:bottom (block/props-of st))))]
    (reduce (fn [s dir] (with s dir (carpet-side chunks p s dir create?))) st wall-sides)))

(defn carpet-faces? [^long st]
  (let [props (block/props-of st)]
    (or (= :true (:bottom props)) (boolean (some #(not= :none (get props %)) wall-sides)))))

(defn carpet-supported? [chunks p ^long st]
  (let [below (at chunks (down p))]
    (if (= :true (:bottom (block/props-of st)))
      (pos? below)
      (and (= :pale-moss-carpet (block/block-of below)) (= :true (:bottom (block/props-of below)))))))

(defn carpet-reshaped ^long [chunks p ^long st]
  (if-not (carpet-supported? chunks p st)
    0
    (let [st' (carpet-updated chunks p st false)]
      (if (carpet-faces? st') st' 0))))

(defn carpet-topper [chunks p side?]
  (let [above (up p) prev (at chunks above)
        carpet? (= :pale-moss-carpet (block/block-of prev))]
    (when (and (chunk/in-range? (above 1))
               (or (not carpet?) (= :false (:bottom (block/props-of prev))))
               (or carpet? (block/can-be-replaced? prev)))
      (let [base (carpet-updated chunks above (block/state :pale-moss-carpet {:bottom :false}) true)
            st' (reduce (fn [s dir]
                          (if (and (not= :none (get (block/props-of s) dir)) (not (side? dir))) (with s dir :none) s))
                        base wall-sides)]
        (when (and (carpet-faces? st') (not= st' prev)) st')))))

(defn hanging-tip ^long [chunks p ^long st]
  (with st :tip (if (= (block/block-of st) (block/block-of (at chunks (down p)))) :false :true)))

(defn hanging-supported? [chunks p ^long st]
  (let [above (at chunks (up p))]
    (or (attachable? chunks p :up) (= (block/block-of st) (block/block-of above)))))

(defn hanging-end [chunks p self]
  (loop [q (down p)]
    (if (= self (block/block-of (at chunks q))) (recur (down q)) q)))
