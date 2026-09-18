(ns collider.world.blocks.grass
  "Grass blocks: where grass spreads and where it stays alive."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:table grass (delay (block/state :grass-block)))

(def ^:private ^:table dirt (delay (block/state :dirt)))

(defn grass-state ^long [] @grass)

(defn dirt-state ^long [] @dirt)

(defn short-grass? [st] (= :short-grass (block/block-of (long st))))

(defn- block-or-zero ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y)
    (chunk/chunks-get-block chunks p)
    0))

(def ^:private neighborhood
  (vec (for [dy [-1 0 1] [dx dz] [[1 0] [-1 0] [0 1] [0 -1]]] [dx dy dz])))

(defn grass-neighbor? [chunks [x y z]]
  (boolean
    (some (fn [[dx dy dz]]
            (= (grass-state)
               (block-or-zero chunks [(+ (long x) (long dx))
                                      (+ (long y) (long dy))
                                      (+ (long z) (long dz))])))
          neighborhood)))

(defn regrowable-dirt? [chunks p]
  (let [[x y z] p]
    (and (= (dirt-state) (block-or-zero chunks p))
         (zero? (block-or-zero chunks [x (inc (long y)) z]))
         (grass-neighbor? chunks p))))

(def rule
  {:name   :grass
   :match? (fn [_chunks st _p] (= (dirt-state) st))
   :wake   (fn [chunks tick p _old _self?]
             (when (regrowable-dirt? chunks p)
               (+ (long tick) 1200 (mod (long (hash [p tick])) 2400))))
   :due    (fn [chunks p _rules]
             (when (regrowable-dirt? chunks p) [[p (grass-state)]]))})

(defn can-stay-alive? [chunks ^long st [x y z]]
  (let [a (block-or-zero chunks [(long x) (inc (long y)) (long z)])]
    (cond
      (and (= :snow-layer (block/type-of a))
           (= :1 (:layers (block/props-of a)))) true
      (and (block/liquid? a) (block/source-state? a)) false
      :else (< (block/light-dampening-into st a :up (block/dampening a)) 15))))
