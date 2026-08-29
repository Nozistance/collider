(ns collider.game.sense

  (:require [collider.game.state :as state]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn block-at
  (^long [world p] (chunk/chunks-get-block (:chunks world) gen/flat-chunk p))
  (^long [world x y z] (chunk/block-state (:chunks world) gen/flat-chunk x y z)))

(defn feet-cell [p]
  [(long (Math/floor (v/x p))) (long (Math/floor (v/y p))) (long (Math/floor (v/z p)))])

(defn nearest-player [world pos r2]
  (let [r2 (double r2) ents (:entities world)]
    (reduce (fn [best oid]
              (if-let [o (get ents oid)]
                (let [d2 (v/dist-sq pos (:pos o))]
                  (if (and (< d2 r2)
                           (or (nil? best)
                               (< d2 (double (best 0)))
                               (and (= d2 (double (best 0))) (< (long oid) (long (best 1))))))
                    [d2 oid o]
                    best))
                best))
            nil
            (vals (:players world)))))

(defn nearest [world pos r2 pred]
  (let [r2 (double r2)]
    (reduce-kv (fn [best oid o]
                 (if (pred oid o)
                   (let [d2 (v/dist-sq pos (:pos o))]
                     (if (and (< d2 r2)
                              (or (nil? best)
                                  (< d2 (double (best 0)))
                                  (and (= d2 (double (best 0))) (< (long oid) (long (best 1))))))
                       [d2 oid o]
                       best))
                   best))
               nil
               (:entities world))))

(defn held-of [p]
  (get-in p [:inventory (+ 36 (long (or (:held-slot p) 0))) :item]))

(defn holders [world]
  (into []
        (keep (fn [[pid p]]
                (when-let [it (held-of p)]
                  [pid it (:pos p)])))
        (state/player-entries world)))
