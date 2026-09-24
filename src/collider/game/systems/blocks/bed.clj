(ns collider.game.systems.blocks.bed
  "Going to sleep in a bed."
  (:require [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.sleep :as sleep]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.bed :as bed]
            [collider.world.blocks.connect :as connect]
            [collider.world.chunk :as chunk]))

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

(defn- say-deltas [eid text]
  (when text [(out/to eid (out/overlay text))]))

(defn- occupied [st]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :occupied :true)))

(defn- lying [world head]
  (let [[x y z] head
        lie [(+ (long x) 0.5) (+ (long y) 0.6875) (+ (long z) 0.5)]]
    {:sleeping {:pos head :since (:tick world)} :pos (v/v3 lie)
     :vel [0.0 0.0 0.0] :client-vel [0.0 0.0 0.0] :leave-bed? nil}))

(defn- sleep-status [world asleep eid]
  (if (<= (long (get-in world [:rules :players-sleeping-percentage]
                        100))
          100)
    (sleep/announcement world asleep)
    (out/to eid (out/overlay {:translate "sleep.not_possible"}))))

(defn- lie-deltas [world eid head st]
  (let [asleep (inc (count (sleep/sleepers world)))]
    (concat (edit/change-deltas world [[head (occupied st)]])
            [[:merge-entity eid (lying world head)]
             (sleep-status world asleep eid)])))

(defn- refusal
  "Returns why the player may not lie in the bed at head: a message,
  :quiet when there is none to say, or nil."
  [world eid head st rule]
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

(defn- broken-beds [chunks changes]
  (for [[p st] changes
        :let [old (chunk/chunks-get-block chunks p)]
        :when (and (zero? (long st)) (= :bed (block/type-of old)))]
    (out/all (out/break-effect p old))))

(defn- removed [world pos]
  (let [ds (edit/change-deltas world [[pos 0]])
        all (second (first ds))]
    [(update world :chunks chunk/chunks-set-blocks all)
     (concat ds (broken-beds (:chunks world) (rest all)))]))

(defn- center-of [[x y z]]
  [(+ (long x) 0.5) (+ (long y) 0.5) (+ (long z) 0.5)])

(def ^:private ^:const bed-power 5.0)

(defn- near? [p [cx cy cz]]
  (let [dx (- (v/x p) (double cx)) dy (- (v/y p) (double cy))
        dz (- (v/z p) (double cz))
        r (+ (* 2.0 bed-power) 2.0)]
    (< (+ (* dx dx) (* dy dy) (* dz dz)) (* r r))))

(defn- unstepped [world center]
  (let [stepped? (fn [o]
                   (and (not= :player (:type o))
                        (near? (:pos o) center)))]
    (into {}
          (keep (fn [[oid o]] (when (stepped? o) [oid (:pos o)])))
          (:entities world))))

(defn- blast-request [world head]
  (let [center (center-of head)]
    {:center center :power bed-power :source :block :fire? true
     :by nil :with :bad-respawn-point
     :later (unstepped world center)}))

(defn- same-block? [world pos st]
  (= (block/block-of (edit/block-at world pos)) (block/block-of st)))

(defn- explode-deltas [world eid head st rule]
  (let [[world' ds] (removed world head)
        foot (mapv + head (connect/partner-offset st))
        more (when (same-block? world' foot st)
               (second (removed world' foot)))]
    (concat (say-deltas eid (:error-message rule)) ds more
            [[:explode (blast-request world head)]])))

(defn sleep-deltas [world eid pos]
  (when-let [head (bed/head-pos (:chunks world) pos)]
    (let [st (edit/block-at world head)
          rule (sleep/bed-rule world)]
      (if (:explodes rule)
        (explode-deltas world eid head st rule)
        (rest-deltas world eid head st rule)))))
