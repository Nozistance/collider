(ns collider.game.systems.blocks.bucket
  "Filling and emptying buckets."
  (:require [collider.data :as data]
            [collider.game.changes :as changes]
            [collider.game.inventory :as inventory]
            [collider.game.item :as item]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.reach :as reach]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.campfire :as campfire]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.attribute :as attribute]))

(set! *warn-on-reflection* true)

(defn- break-drops [world pos cur replace?]
  (when (and replace? (pos? cur) (not (block/liquid? cur))
             (get-in world [:rules :block-drops] true))
    (map-indexed
      (fn [i stack]
        [:spawn-entity (item/popped world pos stack [:bucket i])])
      (block/drops cur #(random/of-key (:tick world) pos %)))))

(defn- may-replace? [cur]
  (or (block/can-be-replaced? cur) (not (block/blocks-motion? cur))))

(defn- pourable? [world eid cur relative replace? holds?]
  (let [shift? (get-in world [:entities eid :sneaking?])]
    (or (zero? cur)
        (and (or replace? holds?)
             (or (not shift?) (nil? relative))))))

(defn- mob-splash [snd pos]
  (out/block-sound snd pos 1.0 1.0 :neutral))

(defn- splash [eid pos water? mob-snd]
  (let [snd (if water? :bucket/empty :bucket/empty-lava)
        fx (if mob-snd
             (mob-splash mob-snd pos)
             (out/block-sound snd pos 1.0 1.0))]
    [(out/except eid fx)]))

(defn- drown-deltas [world pos cur]
  (concat (changes/change-deltas
            world [[pos (campfire/drowned cur) [:fluid-tick]]])
          (when (= :true (:lit (block/props-of cur)))
            (let [snd :generic/extinguish-fire]
              [(out/all (out/block-sound snd pos 1.0 1.0))]))))

(defn- hold-deltas [world pos cur]
  (let [st (block/with-water cur)]
    (changes/change-deltas world [[pos st [:fluid-tick]]])))

(defn- fizz-pitch ^double [world pos]
  (let [roll #(random/of-key (:tick world) pos %)]
    (random/triangle 2.6 0.8 (roll :fizz-a) (roll :fizz-b))))

(defn- evaporated [world eid pos]
  (let [snd (out/block-sound :block.fire.extinguish pos 0.5
                       (fizz-pitch world pos))]
    [(out/except eid snd)]))

(defn- settled [world pos state cur replace? holds?]
  (cond
    (and (block/water? state) (campfire/drowned cur))
    (drown-deltas world pos cur)
    holds? (hold-deltas world pos cur)
    :else (concat (break-drops world pos cur replace?)
                  (changes/change-deltas world [[pos state]]))))

(defn- pour-deltas [world eid pos state relative mob-snd]
  (let [cur (changes/block-at world pos)
        water? (block/water? state)
        replace? (may-replace? cur)
        holds? (and water? (edit/waterloggable? cur))]
    (cond
      (not (pourable? world eid cur relative replace? holds?))
      (when relative
        (pour-deltas world eid relative state nil mob-snd))
      (and water? (attribute/water-evaporates? (:dim world)))
      (evaporated world eid pos)
      :else (concat (settled world pos state cur replace? holds?)
                    (splash eid pos water? mob-snd)))))

(defn- into-hit? [hit state]
  (and (contains? (block/props-of hit) :waterlogged)
       (block/water? state)))

(defn- poured [world eid e state mob-snd]
  (when-let [{:keys [pos face]} (reach/clip world e :none)]
    (let [relative (dir/toward pos face)
          hit (changes/block-at world pos)
          target (if (into-hit? hit state) pos relative)
          next-pos (when (= target pos) relative)]
      (when (chunk/in-level? world (target 1))
        (pour-deltas world eid target state next-pos mob-snd)))))

(defn- emptied [world eid e item ds]
  (when (seq ds)
    (concat ds
            [[:award eid (keyword "used" (name item)) 1]]
            (when-not (player/infinite-materials? e)
              (inventory/filled-result-deltas
                world eid {:item :bucket :count 1} false
                (:use-hand e))))))

(defn empty-deltas
  "Returns the deltas of a player who empties a bucket of state at
  the block in view. Out of creative the hand keeps an empty bucket."
  [world eid e item state]
  (emptied world eid e item (poured world eid e state nil)))

(defn- mob-splash-deltas [world eid e snd]
  (when-let [{:keys [pos face]} (reach/clip world e :none)]
    [(out/except eid (mob-splash snd (dir/toward pos face)))]))

(defn mob-deltas
  "Returns the deltas for a player who empties a bucket that holds a
  mob. Its water pours like a water bucket, with the sound of the
  bucket."
  [world eid e item]
  (let [{:keys [fluid sound]} (data/mob-bucket item)]
    (emptied world eid e item
             (if (= :empty fluid)
               (mob-splash-deltas world eid e sound)
               (poured world eid e (liquid/liquid-state fluid 0)
                       sound)))))

(defn- scoop-target [world e]
  (when-let [{:keys [pos]} (reach/clip world e :source-only)]
    (let [st (changes/block-at world pos)]
      (cond
        (= :powder-snow (block/type-of st)) [:powder-snow pos]
        (liquid/bubble-column? st) [:bubble-column pos]
        (block/source-state? st) [:source pos]
        (= :true (:waterlogged (block/props-of st)))
        [:waterlogged pos]))))

(def ^:private fills
  {:powder-snow [:powder-snow-bucket :bucket/fill-snow]
   :lava [:lava-bucket :bucket/fill-lava]
   :water [:water-bucket :bucket/fill]})

(defn- fill-of [kind st]
  (fills (cond (= :powder-snow kind) :powder-snow
               (block/lava? st) :lava
               :else :water)))

(defn- fill-fx [kind e st]
  (let [[_ snd] (fill-of kind st)]
    (out/sound snd (:pos e) 1.0 1.0 :players)))

(defn- drained-deltas [world kind pos st]
  (case kind
    (:source :bubble-column :powder-snow)
    (changes/change-deltas world [[pos 0]])
    :waterlogged
    (changes/change-deltas world [[pos (block/without-water st)]])))

(defn- snow-fx [kind pos st]
  (when (= :powder-snow kind)
    [(out/all (out/level-event out/particles-destroy-block pos st))]))

(defn scoop-deltas
  "Returns the deltas of a player filling an empty bucket.
  The bucket fills from the block in view."
  [world eid e]
  (when-let [[kind pos] (scoop-target world e)]
    (let [st (changes/block-at world pos)
          filled {:item (first (fill-of kind st)) :count 1}]
      (concat (drained-deltas world kind pos st)
              (snow-fx kind pos st)
              [(out/except eid (fill-fx kind e st))
               [:award eid :used/bucket 1]]
              (inventory/filled-result-deltas world eid filled)))))

(defn lily-deltas
  "Returns the deltas of a player placing a lily pad.
  It goes on the water source in view."
  [world eid e item]
  (when-let [[kind pos] (scoop-target world e)]
    (let [[_ y' _ :as above] (dir/up pos)
          st (block/state item)]
      (when (and (= :source kind)
                 (block/water? (changes/block-at world pos))
                 (chunk/in-level? world y')
                 (block/can-be-replaced?
                   (changes/block-at world above))
                 (not (edit/obstructed? world above st))
                 (support/supported? (:chunks world) above st))
        (changes/placed-deltas world eid above st)))))
