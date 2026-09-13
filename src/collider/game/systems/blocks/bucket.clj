(ns collider.game.systems.blocks.bucket
  (:require [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.reach :as reach]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn- break-drops [world pos cur may-replace?]
  (when (and may-replace? (pos? cur) (not (liquid/liquid-state? cur))
             (get-in world [:rules :block-drops] true))
    (map-indexed (fn [i stack] [:spawn-entity (items/popped world pos stack [:bucket i])])
                 (block/drops cur (fn [salt] (random/of-key [(:tick world) pos salt]))))))

(defn- pour-deltas [world eid pos state relative]
  (let [cur (edit/block-at world pos) water? (= :water (liquid/liquid-class state))
        may-replace? (or (block/can-be-replaced? cur) (not (block/blocks-motion? cur)))
        holds? (and water? (edit/waterloggable? cur))
        shift? (get-in world [:entities eid :sneaking?])
        splash [(out/except eid (out/sound (if water? :bucket/empty :bucket/empty-lava) pos 1.0 1.0))]]
    (cond
      (not (or (zero? cur) (and (or may-replace? holds?) (or (not shift?) (nil? relative)))))
      (when relative (pour-deltas world eid relative state nil))
      holds? (concat (edit/change-deltas world [[pos (edit/with-water cur true)]]) splash)
      :else (concat (break-drops world pos cur may-replace?)
                    (edit/change-deltas world [[pos state]])
                    splash))))

(defn add [world eid e state]
  (when-let [{:keys [pos face]} (reach/clip world e :none)]
    (let [relative (mapv + pos (dir/offset face))
          hit (edit/block-at world pos)
          target (if (and (contains? (block/props-of hit) :waterlogged) (= :water (liquid/liquid-class state))) pos relative)]
      (when (chunk/in-range? (target 1))
        (pour-deltas world eid target state (when (= target pos) relative))))))

(defn- scoop-target [world e]
  (when-let [{:keys [pos]} (reach/clip world e :source-only)]
    (let [st (edit/block-at world pos)]
      (cond
        (= :powder-snow (block/type-of st)) [:powder-snow pos]
        (liquid/bubble-column? st) [:bubble-column pos]
        (liquid/source-state? st) [:source pos]
        (= :true (:waterlogged (block/props-of st))) [:waterlogged pos]))))

(defn scoop-deltas [world eid e]
  (when-let [[kind pos] (scoop-target world e)]
    (let [st (edit/block-at world pos)
          sound (cond
                  (= :powder-snow kind) :bucket/fill-snow
                  (= :lava (liquid/liquid-class st)) :bucket/fill-lava
                  :else :bucket/fill)]
      (concat (case kind
                (:source :bubble-column) [[:set-blocks [[pos 0]] (dec (long (:tick world)))]]
                :powder-snow (edit/change-deltas world [[pos 0]])
                :waterlogged (edit/change-deltas world [[pos (edit/with-water st false)]]))
              (when (= :powder-snow kind) [(out/all (out/level-event out/particles-destroy-block pos st))])
              [(out/except eid (out/sound sound pos 1.0 1.0))]))))

(defn lily-deltas [world eid e]
  (when-let [[kind pos] (scoop-target world e)]
    (let [[_ y' _ :as above] (mapv + pos [0 1 0])
          st (block/state :lily-pad)]
      (when (and (= :source kind)
                 (= :water (liquid/liquid-class (edit/block-at world pos)))
                 (chunk/in-range? y')
                 (block/can-be-replaced? (edit/block-at world above))
                 (not (edit/obstructed? world above st))
                 (support/supported? (:chunks world) gen/flat-chunk above st))
        (edit/placed-deltas world eid above st)))))
