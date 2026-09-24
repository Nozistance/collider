(ns collider.game.systems.blocks.bucket
  "Filling and emptying buckets."
  (:require [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.reach :as reach]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.campfire :as campfire]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- break-drops [world pos cur may-replace?]
  (when (and may-replace? (pos? cur) (not (block/liquid? cur))
             (get-in world [:rules :block-drops] true))
    (map-indexed
      (fn [i stack]
        [:spawn-entity (items/popped world pos stack [:bucket i])])
      (block/drops cur #(random/of-key (:tick world) pos %)))))

(defn- may-replace? [cur]
  (or (block/can-be-replaced? cur) (not (block/blocks-motion? cur))))

(defn- pourable? [world eid cur relative replace? holds?]
  (let [shift? (get-in world [:entities eid :sneaking?])]
    (or (zero? cur)
        (and (or replace? holds?)
             (or (not shift?) (nil? relative))))))

(defn- splash [eid pos water?]
  (let [snd (if water? :bucket/empty :bucket/empty-lava)]
    [(out/except eid (out/sound snd pos 1.0 1.0))]))

(defn- drown-deltas [world pos cur]
  (concat (edit/change-deltas world [[pos (campfire/drowned cur)]])
          (when (= :true (:lit (block/props-of cur)))
            (let [snd :generic/extinguish-fire]
              [(out/all (out/sound snd pos 1.0 1.0))]))))

(defn- hold-deltas [world pos cur]
  (edit/change-deltas world [[pos (edit/with-water cur true)]]))

(defn- pour-deltas [world eid pos state relative]
  (let [cur (edit/block-at world pos) water? (block/water? state)
        replace? (may-replace? cur)
        holds? (and water? (edit/waterloggable? cur))
        fx (splash eid pos water?)]
    (cond
      (not (pourable? world eid cur relative replace? holds?))
      (when relative (pour-deltas world eid relative state nil))
      (and water? (campfire/drowned cur))
      (concat (drown-deltas world pos cur) fx)
      holds? (concat (hold-deltas world pos cur) fx)
      :else (concat (break-drops world pos cur replace?)
                    (edit/change-deltas world [[pos state]])
                    fx))))

(defn- into-hit? [hit state]
  (and (contains? (block/props-of hit) :waterlogged)
       (block/water? state)))

(defn add
  "Returns the deltas for a player who empties a bucket.
  The bucket holds state and pours at the block in view."
  [world eid e state]
  (when-let [{:keys [pos face]} (reach/clip world e :none)]
    (let [relative (mapv + pos (dir/offset face))
          hit (edit/block-at world pos)
          target (if (into-hit? hit state) pos relative)
          next-pos (when (= target pos) relative)]
      (when (chunk/in-level? world (target 1))
        (pour-deltas world eid target state next-pos)))))

(defn- scoop-target [world e]
  (when-let [{:keys [pos]} (reach/clip world e :source-only)]
    (let [st (edit/block-at world pos)]
      (cond
        (= :powder-snow (block/type-of st)) [:powder-snow pos]
        (liquid/bubble-column? st) [:bubble-column pos]
        (block/source-state? st) [:source pos]
        (= :true (:waterlogged (block/props-of st)))
        [:waterlogged pos]))))

(defn- scooped-item [kind st]
  (cond (= :powder-snow kind) :powder-snow-bucket
        (block/lava? st) :lava-bucket
        :else :water-bucket))

(defn- fill-sound [kind st]
  (cond
    (= :powder-snow kind) :bucket/fill-snow
    (block/lava? st) :bucket/fill-lava
    :else :bucket/fill))

(defn- fill-fx [kind pos st]
  (out/sound (fill-sound kind st) pos 1.0 1.0))

(defn- drained-deltas [world kind pos st]
  (case kind
    (:source :bubble-column)
    [[:set-blocks [[pos 0]] (dec (long (:tick world)))]]
    :powder-snow (edit/change-deltas world [[pos 0]])
    :waterlogged
    (edit/change-deltas world [[pos (edit/with-water st false)]])))

(defn- snow-fx [kind pos st]
  (when (= :powder-snow kind)
    [(out/all (out/level-event out/particles-destroy-block pos st))]))

(defn scoop-deltas
  "Returns the deltas of a player filling an empty bucket.
  The bucket fills from the block in view."
  [world eid e]
  (when-let [[kind pos] (scoop-target world e)]
    (let [st (edit/block-at world pos)
          filled {:item (scooped-item kind st) :count 1}]
      (concat (drained-deltas world kind pos st)
              (snow-fx kind pos st)
              [(out/except eid (fill-fx kind pos st))
               [:award eid :used/bucket 1]]
              (items/filled-result-deltas world eid filled)))))

(defn lily-deltas
  "Returns the deltas of a player placing a lily pad.
  It goes on the water source in view."
  [world eid e]
  (when-let [[kind pos] (scoop-target world e)]
    (let [[_ y' _ :as above] (mapv + pos [0 1 0])
          st (block/state :lily-pad)]
      (when (and (= :source kind)
                 (block/water? (edit/block-at world pos))
                 (chunk/in-level? world y')
                 (block/can-be-replaced? (edit/block-at world above))
                 (not (edit/obstructed? world above st))
                 (support/supported? (:chunks world) above st))
        (edit/placed-deltas world eid above st)))))
