(ns collider.game.systems.dripleaf
  "Big dripleaf tipping under players."
  (:require [collider.game.changes :as changes]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.dripleaf :as dripleaf]))

(set! *warn-on-reflection* true)

(def ^:private ^:const epsilon 1.0E-7)

(def ^:private ^:const half-width 0.3)

(defn- floor ^long [^double a] (long (Math/floor a)))

(defn- span [^double c]
  (let [a (floor (+ (- c half-width) epsilon))
        b (floor (- (+ c half-width) epsilon))]
    (if (= a b) [a] [a b])))

(defn- foot-cells [e]
  (let [p (:pos e)
        y (floor (double (v/y p)))
        zs (span (double (v/z p)))]
    (into [] (mapcat (fn [x] (mapv (fn [z] [x y z]) zs)))
          (span (double (v/x p))))))

(defn- tilt-cell [world e p]
  (when (chunk/in-range? (nth p 1))
    (let [st (chunk/chunks-get-block (:chunks world) p)]
      (when (and (dripleaf/leaf? st)
                 (= :none (dripleaf/tilt-of st))
                 (dripleaf/rests-on? p (v/y (:pos e)) (:on-ground e)))
        [p (dripleaf/tilted st :unstable)]))))

(defn- tilted-under [world acc e]
  (reduce (fn [acc p]
            (if-let [[q st] (tilt-cell world e p)]
              (assoc acc q st)
              acc))
          acc (foot-cells e)))

(defn player-deltas
  "Returns the deltas that tip the big dripleaves under player p,
  an entry."
  [world [_ e]]
  (when (:on-ground e)
    (let [changes (tilted-under world (sorted-map) e)]
      (when (seq changes) (changes/set-deltas world (vec changes))))))
