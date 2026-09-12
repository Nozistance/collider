(ns collider.game.systems.blocks.cauldron
  (:require [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]))

(set! *warn-on-reflection* true)

(defn water-bottle? [stack]
  (and (= :potion (:item stack))
       (= :water (get-in stack [:components :potion-contents :potion]))))

(defn- cauldron-filled [world pos item]
  (let [under-water? (= :water (liquid/liquid-class (edit/block-at world (mapv + pos [0 1 0]))))]
    (case item
      :water-bucket [(block/state :water-cauldron {:level :3}) :bucket/empty]
      :lava-bucket (when-not under-water? [(block/state :lava-cauldron) :bucket/empty-lava])
      :powder-snow-bucket (when-not under-water? [(block/state :powder-snow-cauldron {:level :3}) :bucket/empty-snow])
      nil)))

(defn- cauldron-scooped [cur]
  (let [n (block/block-of cur)]
    (cond
      (= :lava-cauldron n) :bucket/fill-lava
      (and (= :water-cauldron n) (= 3 (block/prop-long cur :level))) :bucket/fill
      (and (= :powder-snow-cauldron n) (= 3 (block/prop-long cur :level))) :bucket/fill-snow)))

(defn- cauldron-lowered ^long [cur]
  (let [lvl (block/prop-long cur :level)]
    (if (= 1 lvl) (block/state :cauldron) (block/state (block/block-of cur) {:level (keyword (str (dec lvl)))}))))

(defn- cauldron-raised [cur]
  (if (= :cauldron (block/block-of cur))
    (block/state :water-cauldron)
    (let [lvl (block/prop-long cur :level)]
      (when (< lvl 3)
        (block/state (block/block-of cur) {:level (keyword (str (inc lvl)))})))))

(defn- bottle-deltas [world pos]
  (let [cur (edit/block-at world pos)]
    (when (= :water-cauldron (block/block-of cur))
      (concat (edit/change-deltas world [[pos (cauldron-lowered cur)]])
              [(out/all (out/sound :bottle/fill pos 1.0 1.0))]))))

(defn- pour-bottle-deltas [world pos]
  (let [cur (edit/block-at world pos)]
    (when (contains? #{:cauldron :water-cauldron} (block/block-of cur))
      (when-let [st (cauldron-raised cur)]
        (concat (edit/change-deltas world [[pos st]])
                [(out/all (out/sound :bottle/empty pos 1.0 1.0))])))))

(defn cauldron-deltas [world pos item stack]
  (let [cur (edit/block-at world pos)]
    (cond
      (and (= :potion item) (water-bottle? stack)) (pour-bottle-deltas world pos)
      (= :bucket item)
      (when-let [sound (cauldron-scooped cur)]
        (concat (edit/change-deltas world [[pos (block/state :cauldron)]]) [(out/all (out/sound sound pos 1.0 1.0))]))
      (= :glass-bottle item) (bottle-deltas world pos)
      :else
      (when-let [[st sound] (cauldron-filled world pos item)]
        (concat (edit/change-deltas world [[pos st]]) [(out/all (out/sound sound pos 1.0 1.0))])))))
