(ns collider.world.blocks.dragonegg
  "The dragon egg and the jump it makes when a player touches it."
  (:require [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private ^:const tries 1000)

(def ^:private ^:const spread-xz 16)

(def ^:private ^:const spread-y 8)

(def ^:private ^:const place-delay 5)

(defn delay-after-place
  "Returns how many ticks a dragon egg waits before it falls."
  ^long [] place-delay)

(defn- span ^long [roll i k ^long n]
  (- (long (* n (double (roll [i k :a])))) (long (* n (double (roll [i k :b]))))))

(defn- candidate [pos roll ^long i]
  (mapv + pos [(span roll i :x spread-xz) (span roll i :y spread-y) (span roll i :z spread-xz)]))

(defn- lands? [chunks [_ y _ :as q]]
  (and (chunk/in-range? y) (zero? (chunk/at chunks q))
       (not (zero? (chunk/at chunks (dir/down q))))))

(defn teleport-target [chunks pos roll]
  (first (sequence (comp (map (fn [i] (candidate pos roll i))) (filter #(lands? chunks %)))
                   (range tries))))
