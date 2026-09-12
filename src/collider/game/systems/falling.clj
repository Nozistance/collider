(ns collider.game.systems.falling
  (:require [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.phys :as phys]
            [collider.world.blocks.support :as support])
  (:import (collider.world.phys Move)))

(set! *warn-on-reflection* true)

(def ^:private ^:const half 0.49)
(def ^:private ^:const height 0.98)
(def ^:private ^:const max-time 600)
(defn- block-at ^long [world [_ y _ :as pos]]
  (if (chunk/in-range? y) (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos) 0))

(defn- cell-of [pos]
  [(long (Math/floor (v/x pos))) (long (Math/floor (v/y pos))) (long (Math/floor (v/z pos)))])

(defn- item-deltas [world eid e]
  (when (get-in world [:rules :entity-drops] true)
    (let [t (:tick world)]
      [[:spawn-entity (entity/item (:pos e) (entity/pop-velocity [t eid])
                                   {:item (block/block-of (:block e)) :count 1})]])))

(defn- speleothem? [^long st] (= :pointed-dripstone (block/type-of st)))

(defn- landed-state [world cell st cur concrete? stuck?]
  (let [continues? (and (support/free-below? (:chunks world) gen/flat-chunk cell) (not (and concrete? stuck?)))]
    (when (and (block/can-be-replaced? cur) (not continues?)
               (support/supported? (:chunks world) gen/flat-chunk cell st))
      (let [in-water? (= :water (liquid/liquid-class cur))
            st (if in-water? (block/with-water st) st)]
        (if (and concrete? in-water?) (block/concrete-of st) st)))))

(defn- broken-deltas [world eid e cell]
  (concat [[:remove-entity eid]]
          (when (speleothem? (:block e))
            [(out/all (out/level-event 1045 cell 0))])
          (item-deltas world eid e)))

(defn- land-deltas [world eid e cell cur concrete? stuck?]
  (if-let [st (landed-state world cell (:block e) cur concrete? stuck?)]
    [[:remove-entity eid] [:set-blocks [[cell st]]]]
    (broken-deltas world eid e cell)))

(defn- solid-here? [world cell]
  (let [st (block-at world cell)]
    (and (pos? st) (not (liquid/liquid-state? st)) (block/blocks-motion? st))))

(defn- water-source-here? [world cell]
  (let [st (block-at world cell)]
    (or (block/waterlogged? st)
        (and (= :water (liquid/liquid-class st)) (liquid/source-state? st)))))

(defn- next-cell [from d cell]
  (let [ts (for [i (range 3)
                 :let [di (double (nth d i)) ci (long (nth cell i))]
                 :when (not (zero? di))
                 :let [bound (if (pos? di) (inc ci) ci)]]
             [(/ (- (double bound) (double (nth from i))) di) i])
        [_ axis] (first (sort-by first (remove (fn [[t _]] (< (double t) 0.0)) ts)))]
    (if axis
      (update cell axis (fn [v] (+ (long v) (if (pos? (double (nth d axis))) 1 -1))))
      cell)))

(defn- clip-cell [world from to]
  (let [d (mapv - to from) end (cell-of to)]
    (loop [cell (cell-of from) n 0]
      (cond
        (or (solid-here? world cell) (water-source-here? world cell)) cell
        (or (= cell end) (> n 64)) nil
        :else (recur (next-cell from d cell) (inc n))))))

(defn- clipped-cell [world e pos [mx my mz]]
  (when (> (+ (* (double mx) (double mx)) (* (double my) (double my)) (* (double mz) (double mz))) 1.0)
    (when-let [hit (clip-cell world (:pos e) pos)]
      (when (= :water (liquid/liquid-class (block-at world hit))) hit))))

(defn- landing [world e pos vel]
  (let [concrete? (= :concrete-powder (block/type-of (:block e)))
        clipped (when concrete? (clipped-cell world e pos vel))
        cell (or clipped (cell-of pos))
        cur (block-at world cell)]
    [cell cur concrete?
     (and concrete? (or (some? clipped) (= :water (liquid/liquid-class cur))))]))

(defn- step-deltas [world eid e]
  (let [[vx vy vz] (:vel e)
        ^Move mv (phys/move (:chunks world) gen/flat-chunk (:pos e)
                            [(double vx) (- (double vy) 0.04) (double vz)] half height)
        pos (.pos mv) on-ground (.on-ground mv)
        [mx my mz] (.vel mv)
        time (inc (long (:time e)))
        [cell cur concrete? stuck?] (landing world e pos (.vel mv))]
    (cond
      (or on-ground stuck?)
      (land-deltas world eid (assoc e :pos pos) cell cur concrete? stuck?)
      (or (> time max-time) (and (> time 100) (not (chunk/in-range? (cell 1)))))
      (cons [:remove-entity eid] (item-deltas world eid (assoc e :pos pos)))
      :else
      [[:merge-entity eid {:pos pos :on-ground false :time time
                           :vel [(* (double mx) 0.98) (* (double my) 0.98) (* (double mz) 0.98)]}]])))

(defn first-step-deltas [world]
  (let [active (state/active-chunks world)]
    (into []
          (comp (filter (fn [[_ e]] (and (= :falling-block (:type e))
                                         (zero? (long (:time e 0)))
                                         (state/active-at? active (:pos e)))))
                (mapcat (fn [[eid e]] (step-deltas world eid e))))
          (sort-by key (:entities world)))))

(defn falling-blocks [world _events]
  (let [active (state/active-chunks world)]
    (into []
          (comp (filter (fn [[_ e]] (and (= :falling-block (:type e)) (state/active-at? active (:pos e)))))
                (map (fn [[eid e]] #(step-deltas world eid e))))
          (sort-by key (:entities world)))))
