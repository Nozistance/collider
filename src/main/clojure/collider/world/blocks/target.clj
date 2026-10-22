(ns collider.world.blocks.target
  "Targets, powered for a while by how near their centre
  a projectile lands."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(defn target?
  "Returns true when st is a target."
  [^long st]
  (= :target (block/type-of st)))

(defn- power ^long [^long st] (block/prop-long st :power))

(defn- off [chunks p flags]
  (let [st (chunk/at chunks p)]
    (when (and (target? st) (pos? (power st)))
      [[p (block/with-long st :power 0) nil flags]])))

(defn strength
  "Returns the power of a hit at point at on face, 15 at the centre of
  the face and down to 1 at its edge."
  ^long [at face]
  (let [d (fn [^long i]
            (let [x (double (nth at i))]
              (Math/abs (- (- x (Math/floor x)) 0.5))))
        [i j] (remove #{({:x 0 :y 1 :z 2} (dir/axis face))} [0 1 2])
        far (max (double (d i)) (double (d j)))
        f (max 0.0 (min 1.0 (/ (- 0.5 far) 0.5)))]
    (max 1 (long (Math/ceil (* 15.0 f))))))

(defn hit
  "Returns the changes of a hit of strength n at the target st at p
  that keeps its power for ticks, from tick base."
  [p st n ticks base]
  [[p (block/with-long st :power n)]
   [:tick p (+ (long base) (long ticks)) 0 :target]])

(defn- wake [chunks _dim _tick p old side]
  (let [st (chunk/at chunks p)]
    (when (and (nil? side) (pos? (power st))
               (not= (block/block-of st) (block/block-of old)))
      :neighbor)))

(defn- reset [chunks p ctx]
  (when-not ((:scheduled? ctx) p :target)
    (off chunks p (bit-or update/known-shape update/clients))))

(def rule
  "The block rule that ends the power of a target, and that of a
  target set powered with no tick to end it."
  {:name    :target
   :match?  (fn [_chunks st _p] (target? st))
   :wake    wake
   :reshape reset
   :due     (fn [chunks p _ctx] (off chunks p update/all))})
