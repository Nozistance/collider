(ns collider.world.blocks.multiface
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.blocks.liquid :as liquid]))

(set! *warn-on-reflection* true)

(def ^:private all-dirs [:down :up :north :south :west :east])
(def ^:private six {:down [0 -1 0] :up [0 1 0] :north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})
(def ^:private opposite {:down :up :up :down :north :south :south :north :west :east :east :west})
(def ^:private axis {:down :y :up :y :north :z :south :z :west :x :east :x})

(defn- at ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? (long y)) (chunk/chunks-get-block chunks gen/flat-chunk p) -1))

(defn shuffled [roll xs]
  (loop [v (vec xs) i (count v)]
    (if (< i 2)
      v
      (let [j (long (Math/floor (* (double (roll [:shuffle i])) i)))
            a (v (dec i)) b (v j)]
        (recur (assoc v (dec i) b j a) (dec i))))))

(defn- has-face? [^long st dir] (= :true (get (block/props-of st) dir)))

(defn- attachable? [chunks p dir]
  (let [n (at chunks (mapv + p (six dir)))]
    (and (pos? n) (block/face-sturdy? n (opposite dir)))))

(defn- water-source? [^long st]
  (and (pos? st) (= :water (liquid/liquid-class st)) (liquid/source-state? st)))

(defn- replaceable? [^long st self]
  (or (zero? st) (and (pos? st) (= self (block/block-of st))) (water-source? st)))

(defn- valid-placement? [chunks old p dir self]
  (and (or (not= self (block/block-of (max 0 (long old)))) (not (has-face? (max 0 (long old)) dir)))
       (attachable? chunks p dir)))

(defn- spread-into? [chunks [q dir] self]
  (let [existing (at chunks q)]
    (and (not (neg? existing))
         (replaceable? existing self)
         (valid-placement? chunks existing q dir self))))

(defn- spread-pos [p from-face spread-dir type]
  (case type
    :same-position [p spread-dir]
    :same-plane [(mapv + p (six spread-dir)) from-face]
    :wrap-around [(mapv + p (six spread-dir) (six from-face)) (opposite spread-dir)]))

(defn spread-toward [chunks p st from-face spread-dir]
  (when (and (not= (axis spread-dir) (axis from-face))
             (has-face? st from-face)
             (not (has-face? st spread-dir)))
    (first (for [type [:same-position :same-plane :wrap-around]
                 :let [sp (spread-pos p from-face spread-dir type)]
                 :when (spread-into? chunks sp (block/block-of st))]
             sp))))

(defn- placed-state ^long [chunks [q dir] self]
  (let [old (at chunks q)
        base (cond (= self (block/block-of (max 0 old))) old
                   (water-source? old) (block/with-water (block/state self))
                   :else (block/state self))]
    (block/state (block/block-of base) (assoc (block/props-of base) dir :true))))

(defn- from-face-random [chunks p st from-face roll]
  (first (keep #(spread-toward chunks p st from-face %)
               (shuffled (fn [salt] (roll [:dir from-face salt])) all-dirs))))

(defn spread-random [chunks p ^long st roll]
  (let [self (block/block-of st)]
    (when-let [sp (first (keep #(when (has-face? st %) (from-face-random chunks p st % roll))
                               (shuffled (fn [salt] (roll [:face salt])) all-dirs)))]
      [[(first sp) (placed-state chunks sp self)]])))

(defn can-spread? [chunks p ^long st]
  (boolean (some (fn [from] (some #(spread-toward chunks p st from %) all-dirs)) all-dirs)))
