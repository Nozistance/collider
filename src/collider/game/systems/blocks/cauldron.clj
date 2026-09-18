(ns collider.game.systems.blocks.cauldron
  "Filling and emptying cauldrons."
  (:require [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]))

(set! *warn-on-reflection* true)

(def ^:private water-bottle
  {:item       :potion :count 1
   :components {:potion-contents {:potion :water :custom-color nil
                                  :custom-effects [] :custom-name nil}}})

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
      (= :lava-cauldron n) [:lava-bucket :bucket/fill-lava]
      (and (= :water-cauldron n) (= 3 (block/prop-long cur :level))) [:water-bucket :bucket/fill]
      (and (= :powder-snow-cauldron n) (= 3 (block/prop-long cur :level))) [:powder-snow-bucket :bucket/fill-snow])))

(defn- cauldron-lowered ^long [cur]
  (let [lvl (block/prop-long cur :level)]
    (if (= 1 lvl) (block/state :cauldron) (block/state (block/block-of cur) {:level (keyword (str (dec lvl)))}))))

(defn- cauldron-raised [cur]
  (if (= :cauldron (block/block-of cur))
    (block/state :water-cauldron)
    (let [lvl (block/prop-long cur :level)]
      (when (< lvl 3)
        (block/state (block/block-of cur) {:level (keyword (str (inc lvl)))})))))

(defn- used-deltas
  "Returns the deltas every cauldron use shares: the result item, the
   sound and the two stats, as CauldronInteractions gives them."
  [world eid pos item result sound stat]
  (concat (items/filled-result-deltas world eid result)
          [(out/all (out/sound sound pos 1.0 1.0))
           [:award eid stat 1]
           [:award eid (keyword "used" (name item)) 1]]))

(defn- bottle-deltas [world eid pos]
  (let [cur (edit/block-at world pos)]
    (when (= :water-cauldron (block/block-of cur))
      (concat (edit/change-deltas world [[pos (cauldron-lowered cur)]])
              (used-deltas world eid pos :glass-bottle water-bottle
                           :bottle/fill :custom/use-cauldron)))))

(defn- pour-bottle-deltas [world eid pos]
  (let [cur (edit/block-at world pos)]
    (when (contains? #{:cauldron :water-cauldron} (block/block-of cur))
      (when-let [st (cauldron-raised cur)]
        (concat (edit/change-deltas world [[pos st]])
                (used-deltas world eid pos :potion {:item :glass-bottle :count 1}
                             :bottle/empty :custom/use-cauldron))))))

(defn- scoop-deltas [world eid pos cur]
  (when-let [[filled sound] (cauldron-scooped cur)]
    (concat (edit/change-deltas world [[pos (block/state :cauldron)]])
            (used-deltas world eid pos :bucket {:item filled :count 1}
                         sound :custom/use-cauldron))))

(defn- fill-deltas [world eid pos item]
  (when-let [[st sound] (cauldron-filled world pos item)]
    (concat (edit/change-deltas world [[pos st]])
            (used-deltas world eid pos item {:item :bucket :count 1}
                         sound :custom/fill-cauldron))))

(defn cauldron-deltas [world eid pos item stack]
  (let [cur (edit/block-at world pos)]
    (cond
      (and (= :potion item) (water-bottle? stack)) (pour-bottle-deltas world eid pos)
      (= :bucket item) (scoop-deltas world eid pos cur)
      (= :glass-bottle item) (bottle-deltas world eid pos)
      :else (fill-deltas world eid pos item))))
