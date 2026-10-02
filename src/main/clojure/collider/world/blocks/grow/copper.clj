(ns collider.world.blocks.grow.copper
  "Weathering of copper."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const tick-chance 0.05688889)

(def ^:private near
  (for [dx (range -4 5) dy (range -4 5) dz (range -4 5)
        :let [d (+ (Math/abs (long dx)) (Math/abs (long dy))
                   (Math/abs (long dz)))]
        :when (and (<= d 4) (pos? d))]
    [dx dy dz]))

(defn- stages [chunks p]
  (for [off near
        :let [n (chunk/at chunks (mapv + p off))]
        :when (block/weathering? n)]
    (block/weather-stage n)))

(defn- odds [chunks p st]
  (let [own (block/weather-stage st)
        ages (stages chunks p)]
    (when-not (some #(< (long %) own) ages)
      (let [older (count (filter #(> (long %) own) ages))
            same (- (count ages) older)
            chance (/ (double (inc older)) (double (+ older same 1)))]
        (* chance chance (if (zero? own) 0.75 1.0))))))

(defn- here? [st]
  (or (not= :weathering-copper-door (block/type-of st))
      (= :lower (:half (block/props-of st)))))

(defn tick
  "Returns the change of a random tick that ages the copper st at p."
  [chunks p st roll]
  (when-let [o (and (here? st)
                    (< (double (roll :day)) tick-chance)
                    (odds chunks p st))]
    (when (< (double (roll :age)) (double o))
      (when-let [st' (block/weathered-next st)]
        [[p st']]))))
