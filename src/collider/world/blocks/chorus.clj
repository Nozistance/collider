(ns collider.world.blocks.chorus
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def ^:private sides {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})
(def ^:private faces (assoc sides :down [0 -1 0] :up [0 1 0]))

(defn- at ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks gen/flat-chunk p) 0))

(defn- off [p d] (mapv + p (faces d)))
(defn- plant? [^long st] (= :chorus-plant (block/type-of st)))
(defn- flower? [^long st] (= :chorus-flower (block/type-of st)))
(defn- roots? [^long st] (block/tagged? st "supports_chorus_plant"))

(defn plant-supported? [chunks p]
  (let [below (at chunks (off p :down))
        squeezed? (and (pos? (at chunks (off p :up))) (pos? below))
        branch (some (fn [d]
                       (let [q (off p d) n (at chunks q)]
                         (when (plant? n)
                           (if squeezed?
                             :blocked
                             (let [under (at chunks (off q :down))]
                               (when (or (plant? under) (roots? under)) :held))))))
                     (keys sides))]
    (case branch
      :blocked false
      :held true
      (or (plant? below) (roots? below)))))

(defn flower-supported? [chunks p]
  (let [below (at chunks (off p :down))]
    (if (or (plant? below) (block/tagged? below "supports_chorus_flower"))
      true
      (when (zero? below)
        (loop [ds (keys sides) one? false]
          (if-let [d (first ds)]
            (let [n (at chunks (off p d))]
              (cond
                (plant? n) (when-not one? (recur (next ds) true))
                (pos? n) false
                :else (recur (next ds) one?)))
            one?))))))

(defn supported? [chunks p ^long st]
  (boolean (if (flower? st) (flower-supported? chunks p) (plant-supported? chunks p))))

(defn- connects? [^long n d]
  (or (plant? n) (flower? n) (and (= :down d) (roots? n))))

(defn connected ^long [chunks p ^long st]
  (block/state (block/block-of st)
               (reduce (fn [m d]
                         (assoc m d (if (connects? (at chunks (off p d)) d) :true :false)))
                       (block/props-of st) (keys faces))))

(defn- neighbours-empty? [chunks seen p ignore]
  (every? (fn [d]
            (or (= d ignore)
                (let [q (off p d)] (zero? (long (get seen q (at chunks q)))))))
          (keys sides)))

(defn- pillar [chunks p]
  (loop [h 1 i 0]
    (if (= i 4)
      [h false]
      (let [n (at chunks (mapv + p [0 (- (inc h)) 0]))]
        (if (plant? n) (recur (inc h) (inc i)) [h (roots? n)])))))

(defn- grows-up? [chunks p pick]
  (let [below (at chunks (off p :down))]
    (cond
      (block/tagged? below "supports_chorus_flower") [true false]
      (plant? below) (let [[h on-roots?] (pillar chunks p)]
                       [(or (< h 2) (<= h (long (pick :height (if on-roots? 5 4))))) on-roots?])
      (zero? below) [true false]
      :else [false false])))

(def ^:private order [:north :east :south :west])

(defn- branches [chunks p ^long age pick]
  (let [[_ on-roots?] (grows-up? chunks p pick)
        n (+ (long (pick :tries 4)) (if on-roots? 1 0))
        flower (block/state :chorus-flower {:age (keyword (str (inc age)))})]
    (loop [i 0 seen {} acc []]
      (if (= i n)
        acc
        (let [d (order (long (pick [:dir i] 4)))
              q (off p d)]
          (if (and (zero? (long (get seen q (at chunks q))))
                   (zero? (long (get seen (off q :down) (at chunks (off q :down)))))
                   (neighbours-empty? chunks seen q ({:north :south :south :north :west :east :east :west} d)))
            (recur (inc i) (assoc seen q flower) (conj acc [q flower]))
            (recur (inc i) seen acc)))))))

(defn flower-tick [chunks p ^long st pick]
  (let [above (off p :up) age (Long/parseLong (name (:age (block/props-of st))))]
    (when (and (zero? (at chunks above)) (chunk/in-range? (long (above 1))) (< age 5))
      (let [[up? _] (grows-up? chunks p pick)]
        (cond
          (and up? (neighbours-empty? chunks {} above nil) (zero? (at chunks (off above :up))))
          [[p (connected chunks p (block/state :chorus-plant))]
           [above (block/state :chorus-flower {:age (keyword (str age))})]]
          (< age 4)
          (let [made (branches chunks p age pick)]
            (if (seq made)
              (conj (vec made) [p (connected chunks p (block/state :chorus-plant))])
              [[p (block/state :chorus-flower {:age :5})]]))
          :else [[p (block/state :chorus-flower {:age :5})]])))))
