(ns collider.game.systems.inventory
  (:require [collider.data :as data]
            [collider.game.menu :as menu]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn- restore-deltas
  "On join the client gets the inventory and the held slot of the profile
   (vanilla sends both at login)."
  [world events]
  (for [[tag eid] events
        :when (= :player-join tag)
        :let [e   (get-in world [:entities eid])
              inv (:inventory e)]
        msg (cond-> [(out/to eid (out/held-slot (long (or (:held-slot e) 0))))]
              (seq inv) (conj (out/to eid (out/inventory (mapv inv (range menu/slot-count)) (:carried e)))))]
    msg))

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

(defn- as-seen [s] (when s [(:item s) (long (:count s 1))]))

(defn- click-deltas
  "A click on the player's own inventory: the menu applies it; the slots the
   client predicted differently, and the carried stack, are set straight
   (vanilla broadcastChanges over the remote slots)."
  [world [_ eid {:keys [changed carried] :as m}]]
  (when-let [e (get-in world [:entities eid])]
    (let [before {:inventory (or (:inventory e) {}) :carried (:carried e) :quickcraft (:quickcraft e)}
          after  (menu/click before m)]
      (concat
       [[:merge-entity eid (select-keys after [:inventory :carried :quickcraft])]]
       (for [[slot seen] changed
             :let [ours (get (:inventory after) slot)]
             :when (not= (as-seen ours) (as-seen seen))]
         (out/to eid (out/set-slot slot ours)))
       (when (not= (as-seen (:carried after)) (as-seen carried))
         [(out/to eid (out/carried (:carried after)))])
       (map-indexed (fn [i stack] [:spawn-entity (items/dropped world eid stack true i)])
                    (:drops after))))))

(defn- inventory-deltas [world events]
  (concat (restore-deltas world events)
          (echo-deltas world events)
          (mapcat #(when (= :click (first %)) (click-deltas world %)) events)
          (mapcat #(when (= :pick (first %)) (pick-deltas world %)) events)))

(defn inventory [world events]
  [#(inventory-deltas world events)])
