(ns collider.game.systems.blocks.place
  "Block placement."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.container :as container]
            [collider.game.block.sign :as sign]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private water-plant-types #{:kelp :seagrass})

(def ^:private face-directions
  {0 :down 1 :up 2 :north 3 :south 4 :west 5 :east})

(def ^:private horizontals #{:north :south :west :east})

(defn- snow-replaceable? [^long cur item]
  (let [n (block/prop-long cur :layers)]
    (if (= item :snow) (< n 8) (= n 1))))

(defn replaceable-state?
  "Returns true when the state yields to a block put in its cell."
  ([cur] (replaceable-state? cur nil))
  ([^long cur item]
   (cond
     (zero? cur) true
     (block/liquid? cur) true
     (fire/fire-state? cur) true
     (= :snow-layer (block/type-of cur)) (snow-replaceable? cur item)
     (block/stackable? cur item) true
     :else (block/can-be-replaced? cur))))

(defn replaceable?
  "Returns true when the block at the position yields to the item."
  ([world pos] (replaceable-state? (edit/block-at world pos) nil))
  ([world pos item]
   (replaceable-state? (edit/block-at world pos) item)))

(defn- restated [^long state props]
  (block/state (block/block-of state)
               (merge (block/props-of state) props)))

(defn- snow-stacked [^long cur]
  (let [n (min 8 (inc (block/prop-long cur :layers)))]
    (block/state :snow {:layers (keyword (str n))})))

(defn- stacked [world pos' state item]
  (let [cur (edit/block-at world pos')]
    (cond
      (and (= item :snow) (= :snow-layer (block/type-of cur)))
      (snow-stacked cur)
      (and (block/stackable? cur item)
           (block/stack-props (block/type-of cur)))
      (block/stacked cur)
      :else state)))

(defn- double-slab-at [world pos' item]
  (let [at' (edit/block-at world pos')]
    (when (and (block/same-slab? at' item)
               (not= :double (block/slab-part at')))
      [pos' (block/double-slab item)])))

(defn- slab-merge [world pos pos' face item]
  (let [clicked (edit/block-at world pos)
        part (block/slab-part clicked)
        lower? (and (= 1 (long face)) (= :bottom part))
        upper? (and (= 0 (long face)) (= :top part))]
    (cond
      (and (block/same-slab? clicked item) (or lower? upper?))
      [pos (block/double-slab item)]
      (nil? pos') nil
      :else (double-slab-at world pos' item))))

(defn- water-plant-ok? [world [_ y _ :as pos'] ^long state]
  (let [cur (edit/block-at world pos')]
    (and (block/water? cur)
         (contains? #{0 8} (block/liquid-level cur))
         (pos? (long y))
         (support/supported? (:chunks world) pos' state))))

(defn- moss-side? [world pos dir]
  (< (double (random/of-key (:tick world) pos :moss dir)) 0.5))

(defn- carpet-place-deltas [world eid pos state]
  (let [chunks (chunk/chunks-set-blocks (:chunks world) [[pos state]])
        side? #(moss-side? world pos %)
        topper (moss/carpet-topper chunks pos side?)
        changes [[pos state] [(dir/up pos) topper]]]
    (if topper
      (edit/placed-deltas world eid changes)
      (edit/placed-deltas world eid pos state))))

(defn- block-entity-place-deltas [world eid pos state]
  (let [stack (edit/held-stack world eid)
        entity (be/from-stack (be/fresh (be/kind state) eid) stack)
        editor (when (sign/kind state)
                 [(out/to eid (out/sign-editor pos true))])]
    (concat (edit/placed-deltas world eid pos state)
            [[:set-block-entity pos entity]]
            editor)))

(defn- second-cell-deltas
  "Places a two-cell block, clearing players from the clicked cell
  only, since the other half goes down after the fact."
  [world eid pos pos' state ppos pstate ok?]
  (if (and (chunk/in-range? (ppos 1)) (ok?)
           (not (edit/obstructed? world pos' state)))
    (edit/placed-deltas world eid [[pos' state] [ppos pstate]])
    (edit/reject-deltas world eid pos pos')))

(defn- door-lower [world pos' state [cx _ cz]]
  (let [facing (block/facing-of state)
        hinge (connect/door-hinge (:chunks world) pos' facing cx cz)]
    (restated state {:hinge hinge})))

(defn- door-base-ok? [world pos' above]
  (let [below (edit/block-at world (dir/down pos'))]
    (and (replaceable? world above)
         (block/face-sturdy? below :up))))

(defn- door-place-deltas [world eid pos pos' state cursor]
  (let [above (dir/up pos')
        lower (door-lower world pos' state cursor)
        upper (restated lower {:half :upper})
        ok? #(door-base-ok? world pos' above)]
    (second-cell-deltas world eid pos pos' lower above upper ok?)))

(defn- bed-place-deltas [world eid pos pos' state item]
  (let [head-pos (mapv + pos' (connect/partner-offset state))
        head (restated state {:part :head})
        ok? #(replaceable? world head-pos item)]
    (second-cell-deltas world eid pos pos' state head-pos head ok?)))

(defn- pair-upper [world above state]
  (let [upper (restated state {:half :upper})
        water? (block/water? (edit/block-at world above))]
    (if (contains? (block/props-of state) :waterlogged)
      (edit/with-water upper water?)
      upper)))

(defn- pair-place-deltas [world eid pos pos' state]
  (let [above (dir/up pos')
        upper (pair-upper world above state)
        ok? #(block/can-be-replaced? (edit/block-at world above))]
    (second-cell-deltas world eid pos pos' state above upper ok?)))

(defn- scaffold-direction [world eid face]
  (let [e (get-in world [:entities eid])]
    (cond
      (:sneaking? e) (get face-directions face)
      (= 1 (long face)) (dir/player-direction (:yaw e 0.0))
      :else :up)))

(defn- scaffold-target [world eid pos face]
  (let [dir (scaffold-direction world eid face)
        off (dir/offset dir)
        horizontal? (contains? horizontals dir)]
    (loop [p (mapv + pos off) n 0]
      (when (and (chunk/in-range? (p 1)) (< (long n) 7))
        (let [st (edit/block-at world p)]
          (cond
            (= :scaffolding (block/type-of st))
            (recur (mapv + p off) (if horizontal? (inc n) n))
            (block/can-be-replaced? st) p
            :else nil))))))

(defn scaffold-place-deltas
  "Returns the deltas for scaffolding, which travels from the clicked
  cell until it reaches a free one."
  [world eid pos face]
  (if-let [target (scaffold-target world eid pos face)]
    (let [base (block/state :scaffolding)
          st (support/scaffold-state (:chunks world) target base)
          logged (edit/waterlogged world target st)]
      (if (edit/obstructed? world target st)
        (edit/reject-deltas world eid pos target)
        (edit/placed-deltas world eid target logged)))
    (edit/reject-deltas world eid pos nil)))

(defn- player-pose [world eid]
  (let [e (get-in world [:entities eid])]
    [(or (:yaw e) 0.0) (or (:pitch e) 0.0) (boolean (:sneaking? e))]))

(defn- reshaped [world pos' state]
  (or (when (contains? connect/placed-types (block/type-of state))
        (connect/reshape (:chunks world) pos' state (:tick world)))
      state))

(defn- fitted-state [world pos' state face same? [yaw pitch sneak?]]
  (let [chunks (:chunks world)
        tick (:tick world)
        st (reshaped world pos' state)]
    (support/fitted chunks pos' st face yaw pitch sneak? tick same?)))

(defn- container? [state]
  (and state
       (contains? container/container-types (block/type-of state))))

(defn- contained [world pos' state face [yaw pitch sneak?]]
  (let [chunks (:chunks world)]
    (container/placed-state chunks pos' state face sneak? yaw pitch)))

(defn- refined [world eid pos pos' state face item]
  (let [pose (player-pose world eid)
        same? (= pos' pos)
        st (fitted-state world pos' state face same? pose)
        st (if (container? st) (contained world pos' st face pose) st)
        st (if st (edit/waterlogged world pos' st) st)]
    (stacked world pos' st item)))

(defn- rejected? [world pos' state item]
  (or (nil? state)
      (not (replaceable? world pos' item))
      (and (contains? water-plant-types (block/type-of state))
           (not (water-plant-ok? world pos' state)))
      (edit/obstructed? world pos' state)))

(defn- merged-deltas [world eid pos pos' [mp ms]]
  (if (edit/obstructed? world mp ms)
    (edit/reject-deltas world eid pos pos')
    (edit/placed-deltas world eid mp ms)))

(defn- kind-deltas [world eid pos pos' state item cursor]
  (let [type (block/type-of state)]
    (cond
      (contains? block/door-types type)
      (door-place-deltas world eid pos pos' state cursor)
      (= :bed type) (bed-place-deltas world eid pos pos' state item)
      (= :mossy-carpet type)
      (carpet-place-deltas world eid pos' state)
      (contains? connect/pair-types type)
      (pair-place-deltas world eid pos pos' state)
      (be/kind state) (block-entity-place-deltas world eid pos' state)
      :else (edit/placed-deltas world eid pos' state))))

(defn- placement-state [world eid pos face item cursor]
  (let [cur (edit/block-at world pos)
        yaw (get-in world [:entities eid :yaw] 0.0)]
    (block/placement item face yaw (nth cursor 1)
                     (replaceable-state? cur))))

(defn- place-pos [world eid pos off item]
  (let [cur (edit/block-at world pos)
        pile (when-not (get-in world [:entities eid :sneaking?]) item)
        over? (replaceable-state? cur pile)
        target (if over? pos (mapv + pos off))]
    (when (chunk/in-range? (nth target 1)) target)))

(defn solid-place-deltas
  "Returns the deltas for a held block put against a clicked face."
  [world [eid pos face item cursor]]
  (when-let [off (dir/face-offset face)]
    (when-let [base (placement-state world eid pos face item cursor)]
      (let [pos' (place-pos world eid pos off item)
            st (when pos' (refined world eid pos pos' base face item))
            merged (slab-merge world pos pos' face item)]
        (cond
          merged (merged-deltas world eid pos pos' merged)
          (nil? pos') nil
          (rejected? world pos' st item)
          (edit/reject-deltas world eid pos pos')
          :else (kind-deltas world eid pos pos' st item cursor))))))
