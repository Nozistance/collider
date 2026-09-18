(ns collider.game.systems.blocks.cauldron
  "Filling and emptying cauldrons."
  (:require [collider.data :as data]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private ^:table banners
  (delay (set (data/tag-values "item" "banners"))))

(def ^:private water-bottle
  {:item :potion
   :count 1
   :components
   {:potion-contents
    {:potion :water
     :custom-color nil
     :custom-effects []
     :custom-name nil}}})

(defn water-bottle? [stack]
  (and (= :potion (:item stack))
       (= :water (get-in stack [:components :potion-contents :potion]))))

(defn- cauldron-filled [world pos item]
  (let [above (edit/block-at world (mapv + pos [0 1 0]))
        under-water? (block/water? above)]
    (case item
      :water-bucket [(block/state :water-cauldron {:level :3}) :bucket/empty]
      :lava-bucket (when-not under-water? [(block/state :lava-cauldron) :bucket/empty-lava])
      :powder-snow-bucket (when-not under-water? [(block/state :powder-snow-cauldron {:level :3}) :bucket/empty-snow])
      nil)))

(defn- cauldron-scooped [cur]
  (let [n (block/block-of cur)
        full? (= 3 (block/prop-long cur :level))]
    (cond
      (= :lava-cauldron n)
      [:lava-bucket :bucket/fill-lava]
      (and (= :water-cauldron n) full?)
      [:water-bucket :bucket/fill]
      (and (= :powder-snow-cauldron n) full?)
      [:powder-snow-bucket :bucket/fill-snow])))

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
  "Returns the result item, the sound and the two stats a cauldron use
  gives the player."
  [world eid pos item result sound stat]
  (concat (items/filled-result-deltas world eid result)
          [(out/all (out/sound sound pos 1.0 1.0))
           [:award eid stat 1]
           [:award eid (keyword "used" (name item)) 1]]))

(defn- bottle-deltas [world eid pos]
  (let [cur (edit/block-at world pos)]
    (when (= :water-cauldron (block/block-of cur))
      (concat
        (edit/change-deltas world [[pos (cauldron-lowered cur)]])
        (used-deltas world eid pos :glass-bottle water-bottle
                     :bottle/fill :custom/use-cauldron)))))

(defn- pour-bottle-deltas [world eid pos]
  (let [cur (edit/block-at world pos)
        block (block/block-of cur)]
    (when (contains? #{:cauldron :water-cauldron} block)
      (when-let [st (cauldron-raised cur)]
        (concat
          (edit/change-deltas world [[pos st]])
          (used-deltas world eid pos :potion
                       {:item :glass-bottle :count 1}
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

(defn- dyed-shulker? [item]
  (and (not= :shulker-box item)
       (= :shulker-box (:type (get (data/blocks) item)))))

(defn- wash-deltas
  "Returns the deltas for a dyed shulker box or a patterned banner
  washed in a water cauldron. The cleaned item always joins the
  inventory, even when the hand is already full."
  [world eid pos cur stack cleaned stat]
  (when (= :water-cauldron (block/block-of cur))
    (concat
      (edit/change-deltas world [[pos (cauldron-lowered cur)]])
      (items/filled-result-deltas
        world eid (assoc cleaned :count 1) true)
      [[:award eid stat 1]])))

(defn- washed [world eid pos cur item stack]
  (let [layers (get-in stack [:components :banner-patterns])]
    (cond
      (dyed-shulker? item)
      (wash-deltas world eid pos cur stack
                   (assoc stack :item :shulker-box)
                   :custom/clean-shulker-box)
      (and (contains? @banners item) (seq layers))
      (wash-deltas world eid pos cur stack
                   (assoc-in stack [:components :banner-patterns]
                             (vec (butlast layers)))
                   :custom/clean-banner))))

(defn cauldron-deltas [world eid pos item stack]
  (let [cur (edit/block-at world pos)]
    (cond
      (and (= :potion item) (water-bottle? stack))
      (pour-bottle-deltas world eid pos)
      (= :bucket item) (scoop-deltas world eid pos cur)
      (= :glass-bottle item) (bottle-deltas world eid pos)
      :else (or (washed world eid pos cur item stack)
                (fill-deltas world eid pos item)))))
