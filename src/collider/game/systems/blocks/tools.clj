(ns collider.game.systems.blocks.tools
  (:require [collider.data :as data]
            [collider.game.block.tnt :as tnt]
            [collider.game.entity :as entity]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.systems.blocks.cauldron :as cauldron]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.grow :as grow]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn- tool-set [tag] (delay (set (data/tag-values "item" tag))))
(def axes (tool-set "axes"))
(def hoes (tool-set "hoes"))
(def shovels (tool-set "shovels"))

(defn- candle-lit [world pos]
  (let [cur (edit/block-at world pos) props (block/props-of cur)]
    (when (and (contains? #{:candle :candle-cake} (block/type-of cur))
               (= :false (:lit props)) (not= :true (:waterlogged props)))
      (block/state (block/block-of cur) (assoc props :lit :true)))))

(defn- fire-deltas [world eid pos off]
  (let [[_ y' _ :as pos'] (mapv + pos off)
        st (fire/state-for (:chunks world) pos')]
    (when (and (chunk/in-range? y')
               (zero? (edit/block-at world pos'))
               (support/supported? (:chunks world) gen/flat-chunk pos' st))
      [[:set-blocks [[pos' st]] (dec (long (:tick world)))]
       (out/except eid (out/sound :fire/ignite pos' 1.0 (random/pitch [(:tick world) pos' :flint])))])))

(defn flint-deltas [world [eid pos face]]
  (when-let [off (dir/face-offset face)]
    (cond
      (candle-lit world pos)
      (edit/change-deltas world [[pos (candle-lit world pos)]])
      (and (tnt/tnt-state? (edit/block-at world pos))
           (get-in world [:rules :tnt-explodes] true)
           (not (get-in world [:entities eid :sneaking?]))
           (not ((tnt/primed-origins world) pos)))
      (let [primed (tnt/primed pos [(:tick world) pos])]
        (into (edit/change-deltas world [[pos 0]])
              [[:spawn-entity primed]
               (out/all (out/sound :tnt/primed (:pos primed) 1.0 1.0))]))
      :else (fire-deltas world eid pos off))))

(defn bonemeal-deltas [world [_eid pos _ _ _]]
  (let [st (edit/block-at world pos)]
    (when-let [{:keys [changes drops]} (grow/bonemeal (:chunks world) pos st (fn [salt] (random/of-key [(:tick world) pos :meal salt])))]
      (concat
        (when (seq changes) (edit/change-deltas world changes))
        (map-indexed (fn [i stack] [:spawn-entity (items/popped world pos stack [:meal i])]) drops)
        [(out/all (out/bonemeal pos))]))))

(defn till-deltas [world [_eid pos _ _ _]]
  (let [cur (edit/block-at world pos)]
    (when-let [[to freed] (grow/tilled (block/block-of cur))]
      (when (zero? (edit/block-at world (mapv + pos [0 1 0])))
        (concat
          (edit/change-deltas world [[pos (block/state to)]])
          (when freed [[:spawn-entity (items/popped world pos {:item freed :count 1} :till)]])
          [(out/all (out/sound :hoe/till pos 1.0 1.0))])))))

(defn flatten-deltas [world [_eid pos face _ _]]
  (let [cur (edit/block-at world pos)]
    (when (and (not= 0 (long face))
               (contains? grow/flattened (block/block-of cur))
               (zero? (edit/block-at world (mapv + pos [0 1 0]))))
      (concat
        (edit/change-deltas world [[pos (block/state :dirt-path)]])
        [(out/all (out/sound :shovel/flatten pos 1.0 1.0))]))))

(defn- half-changes [world pos ^long st]
  (let [cur (edit/block-at world pos)]
    (if-let [[ppos pst] (when (contains? block/door-types (block/type-of cur))
                          (connect/partner (:chunks world) pos cur))]
      [[pos st] [ppos (block/state (block/block-of st) (block/props-of pst))]]
      [[pos st]])))

(defn wax-deltas [world [_ pos _ _ _]]
  (when-let [st (block/waxed (edit/block-at world pos))]
    (concat (edit/change-deltas world (half-changes world pos st))
            [(out/all (out/sound :honeycomb/wax-on pos 1.0 1.0)) (out/all (out/level-event 3003 pos))])))

(defn axe-deltas [world [_ pos _ _ _]]
  (let [cur (edit/block-at world pos)]
    (if-let [st (block/stripped cur)]
      (concat (edit/change-deltas world [[pos st]]) [(out/all (out/sound :axe/strip pos 1.0 1.0))])
      (if-let [st (block/weathered-prev cur)]
        (concat (edit/change-deltas world (half-changes world pos st))
                [(out/all (out/sound :axe/scrape pos 1.0 1.0)) (out/all (out/level-event 3005 pos))])
        (when-let [st (block/unwaxed cur)]
          (concat (edit/change-deltas world (half-changes world pos st))
                  [(out/all (out/sound :axe/wax-off pos 1.0 1.0)) (out/all (out/level-event 3004 pos))]))))))

(def ^:private armor-slot {:head 5 :chest 6 :legs 7 :feet 8})

(defn armor-slot-of [item]
  (armor-slot (data/equip-slot item)))

(defn equip-armor-deltas [world eid item slot]
  (let [e (get-in world [:entities eid])]
    (when (and e (nil? (get-in e [:inventory slot])))
      (let [held (+ 36 (long (or (:held-slot e) 0)))
            stack (or (get-in e [:inventory held])
                      {:item item :count 1})]
        [[:set-slot eid slot stack]
         [:set-slot eid held nil]]))))

(defn spawn-egg-deltas [world [_ pos face item]]
  (when-let [off (dir/face-offset face)]
    (when-let [mob (mobs/egg-type item)]
      (let [[x y z] (mapv + pos off)
            t (:tick world)
            at [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
            pitch (+ 1.0 (* 0.2 (- (random/of-key [t pos :p1]) (random/of-key [t pos :p2]))))]
        (when (chunk/in-range? y)
          (cons [:spawn-entity (mobs/egg-mob mob at [t pos] t)]
                (when-let [say (mobs/say-sound mob)]
                  [(out/all (out/sound say at 1.0 pitch))])))))))

(defn carve-deltas [world eid pos face]
  (let [dir (if (<= (long face) 1)
              (dir/opposite (dir/player-direction (get-in world [:entities eid :yaw] 0.0)))
              (get {2 :north 3 :south 4 :west 5 :east} face))
        [ox _ oz] (dir/offset dir)
        [x y z] pos
        t (:tick world)]
    (concat
      (edit/change-deltas world [[pos (block/state :carved-pumpkin {:facing dir})]])
      [[:spawn-entity (entity/item [(+ (long x) 0.5 (* 0.65 (long ox))) (+ (long y) 0.1) (+ (long z) 0.5 (* 0.65 (long oz)))]
                                   [(+ (* 0.05 (long ox)) (* 0.02 (random/of-key [t pos :sx]))) 0.05 (+ (* 0.05 (long oz)) (* 0.02 (random/of-key [t pos :sz])))]
                                   {:item :pumpkin-seeds :count 4})]
       (out/all (out/sound :pumpkin/carve pos 1.0 1.0))])))

(def ^:private mud-blocks (delay (set (get-in @data/tags ["block" "convertable_to_mud"]))))

(defn mud-deltas [world eid pos face]
  (when (and (not= 0 (long face))
             (contains? @mud-blocks (block/block-of (edit/block-at world pos)))
             (cauldron/water-bottle? (edit/held-stack world eid)))
    (concat (edit/change-deltas world [[pos (block/state :mud)]])
            [(out/all (out/sound :splash pos 1.0 1.0))
             (out/all (out/sound :bottle/empty pos 1.0 1.0))])))
