(ns collider.game.systems.blocks.bed
  "Going to sleep in a bed."
  (:require [collider.game.blast :as blast]
            [collider.game.changes :as changes]
            [collider.game.delta :as delta]
            [collider.game.out :as out]
            [collider.game.sleep :as sleeping]
            [collider.game.systems.sleep :as sleep]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.bed :as bed]
            [collider.world.blocks.halves :as halves]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn uses-bed? [world pos]
  (= :bed (block/type-of (changes/block-at world pos))))

(defn- bed-in-range? [world eid head]
  (let [p (get-in world [:entities eid :pos])
        st (changes/block-at world head)
        foot (mapv + head (halves/partner-offset st))]
    (some (fn [[x y z]]
            (and (<= (Math/abs (- (v/x p) (+ (double x) 0.5))) 3.0)
                 (<= (Math/abs (- (v/y p) (double y))) 2.0)
                 (<= (Math/abs (- (v/z p) (+ (double z) 0.5))) 3.0)))
          [head foot])))

(defn- bed-blocked? [world head]
  (let [st (changes/block-at world head)
        above (mapv + head [0 1 0])
        other (mapv + above (halves/partner-offset st))]
    (or (block/full-cube? (changes/block-at world above))
        (block/full-cube? (changes/block-at world other)))))

(defn- spawn-deltas [world eid head]
  (when (not= head (get-in world [:entities eid :spawn]))
    [[:merge-entity eid {:spawn head}]
     (out/to eid (out/system-chat
                  {:translate "block.minecraft.set_spawn"}))]))

(defn- say-deltas [eid text]
  (when text [(out/to eid (out/overlay text))]))

(defn- occupied [st]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :occupied :true)))

(defn- lying [world head]
  (let [[x y z] head
        lie [(+ (long x) 0.5) (+ (long y) 0.6875) (+ (long z) 0.5)]]
    {:sleeping {:pos head :since (:tick world)} :pose :sleeping
     :pos (v/v3 lie)
     :vel [0.0 0.0 0.0] :leave-bed? nil}))

(defn- sleep-status [world asleep eid]
  (if (<= (long (get-in world [:rules :players-sleeping-percentage]
                        100))
          100)
    (sleeping/announcement world asleep)
    (out/to eid (out/overlay {:translate "sleep.not_possible"}))))

(defn- lie-deltas [world eid head st]
  (let [asleep (inc (count (sleeping/sleepers world)))]
    (concat (changes/change-deltas world [[head (occupied st)]])
            [[:merge-entity eid (lying world head)]
             (sleep-status world asleep eid)])))

(defn- refusal [world eid head st rule]
  (cond
    (= :true (:occupied (block/props-of st)))
    {:translate "block.minecraft.bed.occupied"}
    (get-in world [:entities eid :sleeping]) :quiet
    (not (or (:spawn? rule) (:sleep? rule)))
    (or (:error-message rule) :quiet)
    (not (bed-in-range? world eid head))
    {:translate "block.minecraft.bed.too_far_away"}
    (bed-blocked? world head)
    {:translate "block.minecraft.bed.obstructed"}))

(defn- rest-deltas [world eid head st rule]
  (let [why (refusal world eid head st rule)]
    (cond
      (= :quiet why) nil
      why (say-deltas eid why)
      :else
      (concat (when (:spawn? rule) (spawn-deltas world eid head))
              (if (:sleep? rule)
                (lie-deltas world eid head st)
                (say-deltas eid (:error-message rule)))))))

(defn- removed [world pos]
  (let [ds (changes/change-deltas world [[pos 0]])
        all (second (first ds))]
    [(update world :chunks chunk/chunks-set-blocks all) ds]))

(defn- center-of [[x y z]]
  [(+ (long x) 0.5) (+ (long y) 0.5) (+ (long z) 0.5)])

(defn- blast-spec [head]
  (let [center (center-of head)]
    {:center center :power 5.0 :source :block :fire? true
     :src {:type :bad-respawn-point :pos center}}))

(defn- same-block? [world pos st]
  (= (block/block-of (changes/block-at world pos))
     (block/block-of st)))

(defn- explode-deltas
  "Returns the deltas of a bed that blows up, as
  BedBlock.useWithoutItem: both halves go, then the blast."
  [world eid head st rule]
  (let [[world' ds] (removed world head)
        foot (mapv + head (halves/partner-offset st))
        both? (same-block? world' foot st)
        [world'' more] (if both? (removed world' foot) [world' nil])]
    (concat (say-deltas eid (:error-message rule))
            (delta/authored
              (concat ds more (blast/deltas world'' (blast-spec head)))
              {:by eid :with :bed}))))

(defn sleep-deltas [world eid pos]
  (when-let [head (bed/head-pos (:chunks world) pos)]
    (let [st (changes/block-at world head)
          rule (sleep/bed-rule world)]
      (if (:explodes rule)
        (explode-deltas world eid head st rule)
        (rest-deltas world eid head st rule)))))
