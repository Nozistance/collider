(ns collider.world.blocks.grow.weather
  (:require [collider.world.block :as block]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn- odds [chunks [x y z] st]
  (let [own (block/weather-stage st)
        ages (for [dx (range -4 5) dy (range -4 5) dz (range -4 5)
                   :when (and (<= (+ (Math/abs (long dx)) (Math/abs (long dy)) (Math/abs (long dz))) 4)
                              (not (and (zero? (long dx)) (zero? (long dy)) (zero? (long dz)))))
                   :let [n (gen/at chunks [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)])]
                   :when (block/weathering? n)]
               (block/weather-stage n))]
    (when-not (some #(< (long %) own) ages)
      (let [older (count (filter #(> (long %) own) ages))
            same (- (count ages) older)
            chance (/ (double (inc older)) (double (+ older same 1)))]
        (* chance chance (if (zero? own) 0.75 1.0))))))

(defn- here? [st]
  (or (not= :weathering-copper-door (block/type-of st))
      (= :lower (:half (block/props-of st)))))

(defn tick [chunks p st roll]
  (when (and (here? st) (< (double (roll :day)) 0.05688889))
    (when-let [o (odds chunks p st)]
      (when (< (double (roll :age)) (double o))
        (when-let [next (block/weathered-next st)]
          [[p next]])))))
