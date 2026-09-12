(ns collider.game.systems.dripleaf
  (:require [collider.game.state :as state]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def ^:private ^:const epsilon 1.0E-7)
(def ^:private ^:const half-width 0.3)

(defn- floor ^long [^double a] (long (Math/floor a)))

(defn- foot-cells [e]
  (let [p (:pos e) px (double (v/x p)) py (double (v/y p)) pz (double (v/z p))
        y (floor py)
        xs (distinct [(floor (+ (- px half-width) epsilon)) (floor (- (+ px half-width) epsilon))])
        zs (distinct [(floor (+ (- pz half-width) epsilon)) (floor (- (+ pz half-width) epsilon))])]
    (for [x xs z zs] [x y z])))

(defn- tilt-cell [world e p]
  (when (chunk/in-range? (nth p 1))
    (let [st (chunk/chunks-get-block (:chunks world) gen/flat-chunk p)]
      (when (and (dripleaf/leaf? st)
                 (= :none (dripleaf/tilt-of st))
                 (dripleaf/can-tilt? p (v/y (:pos e)) (:on-ground e)))
        [p (dripleaf/tilted st :unstable)]))))

(defn- tilt-changes [world]
  (into (sorted-map)
        (for [[_ e] (state/player-entries world)
              :when (:on-ground e)
              p (foot-cells e)
              :let [c (tilt-cell world e p)]
              :when c]
          c)))

(defn- tilt-deltas [world]
  (let [changes (tilt-changes world)]
    (when (seq changes) [[:set-blocks (vec changes)]])))

(defn dripleaf-tilt [world _events]
  [#(tilt-deltas world)])
