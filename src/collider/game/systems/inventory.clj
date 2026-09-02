(ns collider.game.systems.inventory
  (:require [collider.data :as data]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn- restore-deltas [world events]
  (for [[tag eid] events
        :when (= :player-join tag)
        :let [inv (get-in world [:entities eid :inventory])]
        :when (seq inv)]
    (out/to eid (out/inventory (mapv inv (range 45))))))

(defn- echo-deltas [world events]
  (for [[tag eid slot] events
        :when (and (= :creative-slot tag)
                   (<= 0 (long slot) 44)
                   (get-in world [:entities eid]))]
    (out/to eid (out/set-slot slot (get-in world [:entities eid :inventory slot])))))

(defn- item-of [name]
  (when (contains? (get @data/registries "item") name) name))

(defn- pick-item
  "Item a middle click gives: the block at :pos, or the spawn egg of the entity."
  [world {:keys [pos entity]}]
  (cond
    pos (let [st (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos)]
          (when (pos? (long st)) (item-of (block/block-of (long st)))))
    entity (when-let [t (get-in world [:entities entity :type])]
             (item-of (keyword (str (name t) "-spawn-egg"))))))

(defn- hotbar-slot-with [inv item]
  (some (fn [slot] (when (= item (get-in inv [slot :item])) slot)) (range 36 45)))

(defn- pick-deltas [world [_ eid what]]
  (when-let [e (get-in world [:entities eid])]
    (when-let [item (pick-item world what)]
      (let [inv  (:inventory e)
            held (long (or (:held-slot e) 0))]
        (if-let [slot (hotbar-slot-with inv item)]
          (let [n (- (long slot) 36)]
            (when (not= n held)
              [[:merge-entity eid {:held-slot n}]
               (out/to eid (out/held-slot n))]))
          (let [slot (+ 36 held)
                stack {:item item :count 1}]
            [[:set-slot eid slot stack]
             (out/to eid (out/set-slot slot stack))]))))))

(defn- inventory-deltas [world events]
  (concat (restore-deltas world events)
          (echo-deltas world events)
          (mapcat #(when (= :pick (first %)) (pick-deltas world %)) events)))

(defn inventory [world events]
  [#(inventory-deltas world events)])
