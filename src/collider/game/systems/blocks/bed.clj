(ns collider.game.systems.blocks.bed
  "Going to sleep in a bed."
  (:require [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.daynight :as daynight]
            [collider.game.systems.sleep :as sleep]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.bed :as bed]
            [collider.world.blocks.connect :as connect]))

(set! *warn-on-reflection* true)

(defn uses-bed? [world eid pos item use-item?]
  (and (not use-item?)
       (= :bed (block/type-of (edit/block-at world pos)))
       (not (and item (get-in world [:entities eid :sneaking?])))))

(defn- bed-in-range? [world eid head]
  (let [p (get-in world [:entities eid :pos])
        st (edit/block-at world head)
        foot (mapv + head (connect/partner-offset st))]
    (some (fn [[x y z]]
            (and (<= (Math/abs (- (v/x p) (+ (double x) 0.5))) 3.0)
                 (<= (Math/abs (- (v/y p) (double y))) 2.0)
                 (<= (Math/abs (- (v/z p) (+ (double z) 0.5))) 3.0)))
          [head foot])))

(defn- bed-blocked? [world head]
  (let [st (edit/block-at world head)
        above (mapv + head [0 1 0])
        other (mapv + above (connect/partner-offset st))]
    (or (block/full-cube? (edit/block-at world above))
        (block/full-cube? (edit/block-at world other)))))

(defn- spawn-deltas [world eid head]
  (when (not= head (get-in world [:entities eid :spawn]))
    [[:merge-entity eid {:spawn head}]
     (out/to eid (out/system-chat
                  {:translate "block.minecraft.set_spawn"}))]))

(defn- say-deltas [eid k]
  [(out/to eid (out/overlay {:translate k}))])

(defn- occupied [st]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :occupied :true)))

(defn- lying [world head]
  (let [[x y z] head
        lie [(+ (long x) 0.5) (+ (long y) 0.6875) (+ (long z) 0.5)]]
    {:sleeping {:pos head :since (:tick world)} :pos (v/v3 lie)
     :vel [0.0 0.0 0.0] :client-vel [0.0 0.0 0.0] :leave-bed? nil}))

(defn- lie-deltas [world eid head st]
  (let [asleep (inc (count (sleep/sleepers world)))]
    (concat (spawn-deltas world eid head)
            (edit/change-deltas world [[head (occupied st)]])
            [[:merge-entity eid (lying world head)]
             (sleep/announcement world asleep)])))

(defn- refusal
  "Returns why the player may not lie in the bed at head: a message
  key, :asleep when it already sleeps, or nil."
  [world eid head st]
  (cond
    (= :true (:occupied (block/props-of st)))
    "block.minecraft.bed.occupied"
    (get-in world [:entities eid :sleeping]) :asleep
    (not (bed-in-range? world eid head))
    "block.minecraft.bed.too_far_away"
    (bed-blocked? world head) "block.minecraft.bed.obstructed"))

(defn sleep-deltas [world eid pos]
  (let [head (bed/head-pos (:chunks world) pos)
        st (when head (edit/block-at world head))
        why (when head (refusal world eid head st))]
    (cond
      (or (nil? head) (= :asleep why)) nil
      why (say-deltas eid why)
      (not (daynight/dark? (:time-of-day world 0)))
      (concat (spawn-deltas world eid head)
              (say-deltas eid "block.minecraft.bed.no_sleep"))
      :else (lie-deltas world eid head st))))
