(ns collider.game.systems.dripleaf
  "Big dripleaf tipping under players."
  (:require [collider.game.state :as state]
            [collider.game.systems.blocks.edit :as edit]
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
                 (dripleaf/can-tilt? p (v/y (:pos e)) (:on-ground e)))
        [p (dripleaf/tilted st :unstable)]))))

(defn- tilted-under [world acc e]
  (reduce (fn [acc p]
            (if-let [[q st] (tilt-cell world e p)]
              (assoc acc q st)
              acc))
          acc (foot-cells e)))

(defn- tilt-changes [world]
  (reduce (fn [acc [_ e]]
            (if (:on-ground e) (tilted-under world acc e) acc))
          (sorted-map) (state/player-entries world)))

(defn- tilt-deltas [world]
  (let [changes (tilt-changes world)]
    (when (seq changes) (edit/set-deltas world (vec changes)))))

(defn dripleaf-tilt
  "Returns a step that tips big dripleaves under players."
  [world _d]
  [#(tilt-deltas world)])
