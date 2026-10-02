(ns collider.world.blocks.grow.pickles
  "Sea pickles that bone meal spreads over coral."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(defn- pickle-cells [[x y z]]
  (for [[i span] (map-indexed vector [1 3 5 3 1])
        :let [z-off ([0 1 2 1 0] i)]
        dz (range span)]
    [(+ (long x) -2 (long i)) y (+ (long z) (- (long z-off)) dz)]))

(defn- coral-below? [chunks p]
  (block/tagged? (chunk/at chunks (dir/down p)) "coral_blocks"))

(defn- pickle-spot? [chunks p roll i dy q]
  (and (not= q p)
       (zero? (random/below (roll [:seed i dy]) 6))
       (block/water? (chunk/at chunks q))
       (coral-below? chunks q)))

(defn- pickle-state [roll i dy]
  (let [n (inc (random/below (roll [:n i dy]) 4))
        pickle (block/state :sea-pickle {:waterlogged :true})]
    (block/with-long pickle :pickles n)))

(defn- column-spots [chunks p roll [i [qx qy qz]]]
  (for [dy [-1 0]
        :let [q [qx (+ (long qy) (long dy)) qz]]
        :when (pickle-spot? chunks p roll i dy q)]
    [q (pickle-state roll i dy)]))

(defn- pickle-spots [chunks p roll]
  (into []
        (comp (map-indexed vector)
              (mapcat #(column-spots chunks p roll %)))
        (pickle-cells p)))

(defn pickle-meal
  "Returns the sea pickles bone meal spreads over coral."
  [chunks p st roll]
  (when (and (= :true (:waterlogged (block/props-of st)))
             (coral-below? chunks p))
    (let [full (block/with st :pickles 4)]
      {:changes (conj (pickle-spots chunks p roll)
                      [p full nil update/clients])})))
