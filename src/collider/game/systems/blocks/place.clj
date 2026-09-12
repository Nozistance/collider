(ns collider.game.systems.blocks.place
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.container :as container]
            [collider.game.block.sign :as sign]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn replaceable?
  ([world pos] (replaceable? world pos nil))
  ([world pos item]
   (let [cur (edit/block-at world pos)]
     (cond
       (zero? cur) true
       (liquid/liquid-state? cur) true
       (fire/fire-state? cur) true
       (= :snow-layer (block/type-of cur)) (let [n (block/prop-long cur :layers)]
                                             (if (= item :snow) (< n 8) (= n 1)))
       (block/stackable? cur item) true
       :else (block/can-be-replaced? cur)))))

(defn- stacked [world pos' state item]
  (let [cur (edit/block-at world pos')]
    (cond
      (and (= item :snow) (= :snow-layer (block/type-of cur)))
      (block/state :snow {:layers (keyword (str (min 8 (inc (block/prop-long cur :layers)))))})
      (and (block/stackable? cur item) (block/stack-props (block/type-of cur))) (block/stacked cur)
      :else state)))

(defn- slab-merge [world pos pos' face item]
  (let [clicked (edit/block-at world pos)
        part (block/slab-part clicked)
        lower? (and (= 1 (long face)) (= :bottom part))
        upper? (and (= 0 (long face)) (= :top part))]
    (cond
      (and (block/same-slab? clicked item) (or lower? upper?))
      [pos (block/double-slab item)]
      (and pos' (block/same-slab? (edit/block-at world pos') item)
           (not= :double (block/slab-part (edit/block-at world pos'))))
      [pos' (block/double-slab item)])))

(def ^:private water-plant-types #{:kelp :seagrass})

(defn- water-plant-ok? [world [_ y _ :as pos'] ^long state]
  (let [cur (edit/block-at world pos')]
    (and (= :water (liquid/liquid-class cur))
         (contains? #{0 8} (liquid/level cur))
         (pos? (long y))
         (support/supported? (:chunks world) gen/flat-chunk pos' state))))

(defn- carpet-place-deltas [world eid pos state]
  (let [chunks (chunk/chunks-set-blocks (:chunks world) gen/flat-chunk [[pos state]])
        side? (fn [dir] (< (double (random/of-key [(:tick world) pos :moss dir])) 0.5))]
    (if-let [topper (moss/carpet-topper chunks pos side?)]
      (edit/placed-deltas world eid [[pos state] [(mapv + pos [0 1 0]) topper]])
      (edit/placed-deltas world eid pos state))))

(defn- block-entity-place-deltas [world eid pos state]
  (concat (edit/placed-deltas world eid pos state)
          [[:set-block-entity pos (be/from-stack (be/fresh (be/kind state) eid)
                                                 (edit/held-stack world eid))]]
          (when (sign/kind state) [(out/to eid (out/sign-editor pos true))])))

(defn- second-cell-deltas [world eid pos pos' state ppos pstate ok?]
  (if (and (chunk/in-range? (ppos 1)) (ok?) (not (edit/obstructed? world ppos pstate)))
    (edit/placed-deltas world eid [[pos' state] [ppos pstate]])
    (edit/reject-deltas world eid pos pos')))

(defn- door-place-deltas [world eid pos pos' state [cx _ cz]]
  (let [above (dir/up pos')
        hinge (connect/door-hinge (:chunks world) pos' (block/facing-of state) cx cz)
        lower (block/state (block/block-of state) (assoc (block/props-of state) :hinge hinge))
        upper (block/state (block/block-of state) (assoc (block/props-of lower) :half :upper))]
    (second-cell-deltas world eid pos pos' lower above upper
                        #(and (replaceable? world above)
                              (block/face-sturdy? (edit/block-at world (dir/down pos')) :up)))))

(defn- bed-place-deltas [world eid pos pos' state item]
  (let [head-pos (mapv + pos' (connect/partner-offset state))
        head (block/state (block/block-of state) (assoc (block/props-of state) :part :head))]
    (second-cell-deltas world eid pos pos' state head-pos head
                        #(replaceable? world head-pos item))))

(defn- pair-place-deltas [world eid pos pos' state]
  (let [above (dir/up pos')
        props (block/props-of state)
        upper (block/state (block/block-of state) (assoc props :half :upper))
        upper (if (contains? props :waterlogged)
                (edit/with-water upper (= :water (liquid/liquid-class (edit/block-at world above))))
                upper)]
    (second-cell-deltas world eid pos pos' state above upper
                        #(block/can-be-replaced? (edit/block-at world above)))))

(defn- scaffold-target [world eid pos face]
  (let [sneaking? (get-in world [:entities eid :sneaking?])
        dir (cond
              sneaking? (get {0 :down 1 :up 2 :north 3 :south 4 :west 5 :east} face)
              (= 1 (long face)) (dir/player-direction (get-in world [:entities eid :yaw] 0.0))
              :else :up)
        off (dir/offset dir)
        horizontal? (contains? #{:north :south :west :east} dir)]
    (loop [p (mapv + pos off) n 0]
      (let [[_ y _] p]
        (when (and (chunk/in-range? y) (< n 7))
          (let [st (edit/block-at world p)]
            (cond
              (= :scaffolding (block/type-of st)) (recur (mapv + p off) (if horizontal? (inc n) n))
              (block/can-be-replaced? st) p
              :else nil)))))))

(defn scaffold-place-deltas [world eid pos face]
  (if-let [target (scaffold-target world eid pos face)]
    (let [st (support/scaffold-state (:chunks world) gen/flat-chunk target (block/state :scaffolding))]
      (if (not (edit/obstructed? world target st))
        (edit/placed-deltas world eid target (edit/waterlogged world target st))
        (edit/reject-deltas world eid pos target)))
    (edit/reject-deltas world eid pos nil)))

(defn- pose [world eid]
  (let [e (get-in world [:entities eid])]
    [(or (:yaw e) 0.0) (or (:pitch e) 0.0) (boolean (:sneaking? e))]))

(defn- reshaped [world pos' state]
  (or (when (contains? connect/placed-types (block/type-of state))
        (connect/reshape (:chunks world) pos' state (:tick world)))
      state))

(defn- refined [world eid pos pos' state face item]
  (let [[yaw pitch sneaking?] (pose world eid)
        state (support/fitted (:chunks world) gen/flat-chunk pos' (reshaped world pos' state) face
                              yaw pitch sneaking? (:tick world) (= pos' pos))
        state (if (and state (contains? container/container-types (block/type-of state)))
                (container/placed-state (:chunks world) pos' state face sneaking? yaw pitch)
                state)]
    (stacked world pos' (if state (edit/waterlogged world pos' state) state) item)))

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
      (contains? block/door-types type) (door-place-deltas world eid pos pos' state cursor)
      (= :bed type) (bed-place-deltas world eid pos pos' state item)
      (= :mossy-carpet type) (carpet-place-deltas world eid pos' state)
      (contains? connect/pair-types type) (pair-place-deltas world eid pos pos' state)
      (be/kind state) (block-entity-place-deltas world eid pos' state)
      :else (edit/placed-deltas world eid pos' state))))

(defn solid-place-deltas [world [eid pos face item cursor]]
  (when-let [off (dir/face-offset face)]
    (when-let [state (block/placement item face (get-in world [:entities eid :yaw] 0.0) (nth cursor 1)
                                      (replaceable? world pos))]
      (let [pile (when-not (get-in world [:entities eid :sneaking?]) item)
            [_ y' _ :as target] (if (replaceable? world pos pile) pos (mapv + pos off))
            pos' (when (chunk/in-range? y') target)
            state (when pos' (refined world eid pos pos' state face item))]
        (if-let [merged (slab-merge world pos pos' face item)]
          (merged-deltas world eid pos pos' merged)
          (when pos'
            (if (rejected? world pos' state item)
              (edit/reject-deltas world eid pos pos')
              (kind-deltas world eid pos pos' state item cursor))))))))
