(ns collider.game.mob.sense
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
  (let [r2 (double r2) entities (:entities world)]
    (reduce (fn [best oid]
              (if-let [o (get entities oid)]
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

(def ^:private ^:const cell-shift 2)

(defn- cell-key ^long [^long cx ^long cz]
  (bit-or (bit-shift-left (bit-and cx 0xFFFFFFFF) 32) (bit-and cz 0xFFFFFFFF)))

(defn- cell-of ^long [pos]
  (cell-key (bit-shift-right (long (Math/floor (v/x pos))) cell-shift)
            (bit-shift-right (long (Math/floor (v/z pos))) cell-shift)))

(defn- build-index [entities]
  (persistent!
    (reduce-kv (fn [m eid e]
                 (if-let [p (:pos e)]
                   (let [k (cell-of p)]
                     (assoc! m k (conj (get m k []) [eid e])))
                   m))
               (transient {})
               entities)))

(def ^:private index-cache (atom nil))

(defn- entity-index [entities]
  (let [c @index-cache]
    (if (and c (identical? (nth c 0) entities))
      (nth c 1)
      (let [idx (build-index entities)]
        (reset! index-cache [entities idx])
        idx))))

(defn- closer? [best ^double d2 oid]
  (or (nil? best)
      (< d2 (double (best 0)))
      (and (= d2 (double (best 0))) (< (long oid) (long (best 1))))))

(defn- scan-cell [best pos r2 pred entries]
  (reduce (fn [best [oid o]]
            (if (pred oid o)
              (let [d2 (v/dist-sq pos (:pos o))]
                (if (and (< d2 (double r2)) (closer? best d2 oid)) [d2 oid o] best))
              best))
          best
          entries))

(defn- cell-span [^double c ^double r]
  [(bit-shift-right (long (Math/floor (- c r))) cell-shift)
   (bit-shift-right (long (Math/floor (+ c r))) cell-shift)])

(defn- scan-row [index best pos r2 pred cx z0 z1]
  (loop [cz (long z0) best best]
    (if (> cz (long z1))
      best
      (recur (inc cz) (scan-cell best pos r2 pred (get index (cell-key (long cx) cz)))))))

(defn nearest [world pos r2 pred]
  (let [r2 (double r2)
        index (entity-index (:entities world))
        r (Math/sqrt r2)
        [x0 x1] (cell-span (v/x pos) r)
        [z0 z1] (cell-span (v/z pos) r)]
    (loop [cx (long x0) best nil]
      (if (> cx (long x1))
        best
        (recur (inc cx) (scan-row index best pos r2 pred cx z0 z1))))))

(defn held-of [p]
  (get-in p [:inventory (+ 36 (long (or (:held-slot p) 0))) :item]))

(defn holders [world]
  (into []
        (keep (fn [[pid p]]
                (when-let [it (held-of p)]
                  [pid it (:pos p)])))
        (state/player-entries world)))
