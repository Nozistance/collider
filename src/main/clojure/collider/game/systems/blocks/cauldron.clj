(ns collider.game.systems.blocks.cauldron
  "Filling, emptying and washing in cauldrons."
  (:require [collider.data :as data]
            [collider.game.changes :as changes]
            [collider.game.inventory :as inventory]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.stack :as stack]
            [collider.world.block :as block]
            [collider.world.blocks.cauldron :as cauldron]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private ^:table banners
  (delay (set (data/tag-values "item" "banners"))))

(def ^:private ^:table undyeable
  (delay (set (data/tag-values "item" "cauldron_can_remove_dye"))))

(defn- cauldron-filled [world pos item]
  (let [above (changes/block-at world (dir/up pos))
        dry? (not (block/water? above))]
    (case item
      :water-bucket
      [(block/state :water-cauldron {:level :3}) :bucket/empty]
      :lava-bucket
      (if dry? [(block/state :lava-cauldron) :bucket/empty-lava] :consume)
      :powder-snow-bucket
      (if dry?
        [(block/state :powder-snow-cauldron {:level :3})
         :bucket/empty-snow]
        :consume)
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

(defn- cauldron-raised [cur]
  (if (= :cauldron (block/block-of cur))
    (block/state :water-cauldron)
    (let [lvl (block/prop-long cur :level)]
      (when (< lvl 3)
        (block/with-long cur :level (inc lvl))))))

(defn- hand-of [world eid]
  (get-in world [:entities eid :use-hand]))

(defn- used-deltas [world eid pos item result sound stat]
  (concat (inventory/filled-result-deltas world eid result false
                                          (hand-of world eid))
          [(out/all (out/block-sound sound pos 1.0 1.0))
           [:award eid stat 1]
           [:award eid (keyword "used" (name item)) 1]]))

(defn- bottle-deltas [world eid pos]
  (let [cur (changes/block-at world pos)]
    (when (= :water-cauldron (block/block-of cur))
      (concat
        (changes/change-deltas world [[pos (cauldron/lowered cur)]])
        (used-deltas world eid pos :glass-bottle stack/water-bottle
                     :bottle/fill :custom/use-cauldron)))))

(defn- pour-bottle-deltas [world eid pos]
  (let [cur (changes/block-at world pos)
        kind (block/block-of cur)]
    (when (contains? #{:cauldron :water-cauldron} kind)
      (when-let [st (cauldron-raised cur)]
        (concat
          (changes/change-deltas world [[pos st]])
          (used-deltas world eid pos :potion
                       {:item :glass-bottle :count 1}
                       :bottle/empty :custom/use-cauldron))))))

(defn- scoop-deltas [world eid pos cur]
  (when-let [[filled sound] (cauldron-scooped cur)]
    (concat
      (changes/change-deltas world [[pos (block/state :cauldron)]])
      (used-deltas world eid pos :bucket {:item filled :count 1}
                   sound :custom/use-cauldron))))

(defn- fill-deltas [world eid pos item]
  (when-let [filled (cauldron-filled world pos item)]
    (if (= :consume filled)
      []
      (let [[st sound] filled]
        (concat
          (changes/change-deltas world [[pos st]])
          (used-deltas world eid pos item {:item :bucket :count 1}
                       sound :custom/fill-cauldron))))))

(defn- dyed-shulker? [item]
  (and (not= :shulker-box item)
       (= :shulker-box (:type (get (data/blocks) item)))))

(defn- wash-deltas [world eid pos cur cleaned stat]
  (when (= :water-cauldron (block/block-of cur))
    (concat
      (changes/change-deltas world [[pos (cauldron/lowered cur)]])
      (inventory/filled-result-deltas
        world eid (assoc cleaned :count 1) true (hand-of world eid))
      [[:award eid stat 1]])))

(defn- undyed-deltas [world eid pos cur held]
  (when (and (= :water-cauldron (block/block-of cur))
             (contains? (:components held) :dyed-color))
    (concat
      (changes/change-deltas world [[pos (cauldron/lowered cur)]])
      [[:set-slot eid (player/use-slot world eid)
        (update held :components dissoc :dyed-color)]
       [:award eid :custom/clean-armor 1]])))

(defn- washed [world eid pos cur item held]
  (let [layers (get-in held [:components :banner-patterns])]
    (cond
      (contains? @undyeable item)
      (undyed-deltas world eid pos cur held)
      (dyed-shulker? item)
      (wash-deltas world eid pos cur (assoc held :item :shulker-box)
                   :custom/clean-shulker-box)
      (and (contains? @banners item) (seq layers))
      (let [path [:components :banner-patterns]]
        (wash-deltas world eid pos cur
                     (assoc-in held path (vec (butlast layers)))
                     :custom/clean-banner)))))

(defn cauldron-deltas
  "Returns the deltas of item in the hand of player eid used on the
  cauldron at pos."
  [world eid pos item held]
  (let [cur (changes/block-at world pos)]
    (cond
      (and (= :potion item) (stack/water-bottle? held))
      (pour-bottle-deltas world eid pos)
      (= :bucket item) (scoop-deltas world eid pos cur)
      (= :glass-bottle item) (bottle-deltas world eid pos)
      :else (or (washed world eid pos cur item held)
                (fill-deltas world eid pos item)))))
