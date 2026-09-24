(ns collider.game.systems.falling
  "Falling blocks such as sand, gravel and anvils."
  (:require [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.motion :as motion]
            [collider.world.phys :as phys]
            [collider.world.blocks.support :as support])
  (:import (collider.world.phys Move)))

(set! *warn-on-reflection* true)

(def ^:private ^:const half (double (float 0.49)))

(def ^:private ^:const height (double (float 0.98)))

(def ^:private ^:const max-time 600)

(defn- block-at ^long [world [_ y _ :as pos]]
  (if (chunk/in-range? y)
    (chunk/chunks-get-block (:chunks world) pos)
    0))

(defn- cell-of [pos]
  [(long (Math/floor (v/x pos)))
   (long (Math/floor (v/y pos)))
   (long (Math/floor (v/z pos)))])

(defn- item-deltas [world eid e]
  (when (get-in world [:rules :entity-drops] true)
    (let [vel (entity/pop-velocity [(:tick world) eid])
          stack {:item (block/block-of (:block e)) :count 1}]
      [[:spawn-entity (entity/item (:pos e) vel stack)]])))

(defn- speleothem? [^long st]
  (= :pointed-dripstone (block/type-of st)))

(defn- anvil? [^long st] (= :anvil (block/type-of st)))

(defn- land-event [sound cell]
  (out/all (out/level-event sound cell 0)))

(defn- landed-state [world e cell cur concrete? stuck?]
  (let [st (:block e)
        free? (support/free-below? (:chunks world) cell)
        continues? (and free? (not (and concrete? stuck?)))]
    (when (and (block/can-be-replaced? cur) (not continues?)
               (support/supported? (:chunks world) cell st))
      (let [in-water? (block/water? cur)
            st (if in-water? (block/with-water st) st)]
        (if (and concrete? in-water?) (block/concrete-of st) st)))))

(defn- broken-deltas [world eid e cell]
  (concat [[:remove-entity eid]]
          (when (speleothem? (:block e))
            [(land-event out/sound-pointed-dripstone-land cell)])
          (when (anvil? (:block e))
            [(land-event out/sound-anvil-broken cell)])
          (item-deltas world eid e)))

(defn- land-deltas [world eid e cell cur concrete? stuck?]
  (if-let [st (landed-state world e cell cur concrete? stuck?)]
    (cond-> [[:remove-entity eid] [:set-blocks [[cell st]]]]
            (anvil? st)
            (conj (land-event out/sound-anvil-land cell)))
    (broken-deltas world eid e cell)))

(defn- solid-here? [world cell]
  (let [st (block-at world cell)]
    (and (pos? st) (not (block/liquid? st))
         (block/blocks-motion? st))))

(defn- water-source-here? [world cell]
  (let [st (block-at world cell)]
    (or (block/waterlogged? st)
        (block/water-source? st))))

(defn- stops-here? [world cell]
  (or (solid-here? world cell) (water-source-here? world cell)))

(defn- axis-time [from d cell ^long i]
  (let [di (double (nth d i))
        ci (long (nth cell i))]
    (when-not (zero? di)
      (let [bound (if (pos? di) (inc ci) ci)]
        [(/ (- (double bound) (double (nth from i))) di) i]))))

(defn- soonest-axis [from d cell]
  (->> (range 3)
       (keep (fn [i] (axis-time from d cell i)))
       (remove (fn [[t _]] (< (double t) 0.0)))
       (sort-by first)
       first
       second))

(defn- next-cell [from d cell]
  (if-let [axis (soonest-axis from d cell)]
    (let [step (if (pos? (double (nth d axis))) 1 -1)]
      (update cell axis (fn [c] (+ (long c) step))))
    cell))

(defn- clip-cell [world from to]
  (let [d (mapv - to from) end (cell-of to)]
    (loop [cell (cell-of from) n 0]
      (cond
        (stops-here? world cell) cell
        (or (= cell end) (> n 64)) nil
        :else (recur (next-cell from d cell) (inc n))))))

(defn- clipped-cell [world e pos [mx my mz]]
  (let [mx (double mx) my (double my) mz (double mz)]
    (when (> (+ (* mx mx) (* my my) (* mz mz)) 1.0)
      (when-let [hit (clip-cell world (:pos e) pos)]
        (when (block/water? (block-at world hit)) hit)))))

(defn- landing [world e pos vel]
  (let [concrete? (= :concrete-powder (block/type-of (:block e)))
        clipped (when concrete? (clipped-cell world e pos vel))
        cell (or clipped (cell-of pos))
        cur (block-at world cell)]
    [cell cur concrete?
     (and concrete? (or (some? clipped) (block/water? cur)))]))

(defn- fall-move ^Move [world e]
  (let [[vx vy vz] (:vel e)
        d [(double vx) (- (double vy) 0.04) (double vz)]]
    (phys/move (:chunks world) (:pos e)
               (if (:stuck e) (mapv * d (:stuck e)) d) half height)))

(defn- drift-vel [stuck [mx my mz]]
  (if stuck
    [0.0 0.0 0.0]
    [(* (double mx) 0.98) (* (double my) 0.98)
     (* (double mz) 0.98)]))

(defn- stuck-now [world pos]
  (motion/stuck-speed (:chunks world) pos half height))

(defn- drift-deltas [eid pos time vel stuck stuck']
  (let [m {:pos pos :on-ground false :time time
           :vel (drift-vel stuck vel)}
        m (cond-> m (or stuck' stuck) (assoc :stuck stuck'))]
    [[:merge-entity eid m]]))

(defn- drift [world eid e pos time vel]
  (drift-deltas eid pos time vel (:stuck e) (stuck-now world pos)))

(defn- expired? [world ^long time cell]
  (or (> time max-time)
      (and (> time 100)
           (not (chunk/in-level? world (long (cell 1)))))))

(defn- step-deltas [world eid e]
  (let [^Move mv (fall-move world e)
        pos (phys/pos mv)
        time (inc (long (:time e)))
        vel (phys/vel mv)
        [cell cur concrete? stuck?] (landing world e pos vel)]
    (cond
      (or (phys/on-ground? mv) stuck?)
      (land-deltas world eid (assoc e :pos pos)
                   cell cur concrete? stuck?)
      (expired? world time cell)
      (cons [:remove-entity eid]
            (item-deltas world eid (assoc e :pos pos)))
      :else (drift world eid e pos time vel))))

(defn- falling? [active e]
  (and (= :falling-block (:type e))
       (state/active-at? active (:pos e))))

(defn falling-blocks
  "Returns a step for every falling block in an active chunk."
  [world _d]
  (let [active (state/active-chunks world)]
    (into []
          (comp (filter (fn [[_ e]] (falling? active e)))
                (map (fn [[eid e]] #(step-deltas world eid e))))
          (sort-by key (:entities world)))))
