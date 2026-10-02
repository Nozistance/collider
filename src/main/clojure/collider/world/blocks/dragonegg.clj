(ns collider.world.blocks.dragonegg
  "The dragon egg and the jump it makes when a player touches it."
  (:require [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private ^:const tries 1000)

(def ^:private ^:const spread-xz 16)

(def ^:private ^:const spread-y 8)

(def ^:const place-delay
  "The ticks a placed dragon egg waits before it falls."
  5)

(defn- span ^long [roll i k ^long n]
  (let [a (double (roll [i k :a]))
        b (double (roll [i k :b]))]
    (- (long (* n a)) (long (* n b)))))

(defn- candidate [pos roll ^long i]
  (let [dx (span roll i :x spread-xz)
        dy (span roll i :y spread-y)
        dz (span roll i :z spread-xz)]
    (mapv + pos [dx dy dz])))

(defn- lands? [chunks [_ y _ :as q]]
  (and (chunk/in-range? y) (zero? (chunk/at chunks q))
       (not (zero? (chunk/at chunks (dir/down q))))))

(defn teleport-target
  "Returns the cell the dragon egg at pos jumps to, or nil when no
  try finds one. roll gives a number from 0 to 1 for a key."
  [chunks pos roll]
  (let [found (comp (map #(candidate pos roll %))
                    (filter #(lands? chunks %)))]
    (first (sequence found (range tries)))))
