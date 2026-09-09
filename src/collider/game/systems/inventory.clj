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

(def ^:private scan-order
  "Inventory slots in the order vanilla walks them: the hotbar, then the rest."
  (vec (concat (range 36 45) (range 9 36))))

(defn- slot-with [inv item]
  (some (fn [slot] (when (= item (get-in inv [slot :item])) slot)) scan-order))

(defn- free-slot [inv]
  (some (fn [slot] (when-not (get inv slot) slot)) scan-order))

(defn- suitable-hotbar
  "getSuitableHotbarSlot: the first empty hotbar slot from the held one round
   the hotbar, and the held one when every slot is taken."
  [inv ^long held]
  (or (some (fn [i] (let [n (mod (+ held (long i)) 9)]
                      (when-not (get inv (+ 36 n)) n)))
            (range 9))
      held))

(defn- select-deltas [eid ^long n]
  [[:merge-entity eid {:held-slot n}] (out/to eid (out/held-slot n))])

(defn- pick-deltas
  "tryPickItem: an item the player already has comes to the hand, otherwise
   creative gives a new stack. It takes a free hotbar slot and overwrites the
   held one only when the hotbar is full, and then what was there moves to a
   free slot."
  [world [_ eid what]]
  (when-let [e (get-in world [:entities eid])]
    (when-let [item (pick-item world what)]
      (let [inv  (:inventory e)
            held (long (or (:held-slot e) 0))]
        (if-let [slot (slot-with inv item)]
          (if (<= 36 (long slot) 44)
            (select-deltas eid (- (long slot) 36))
            (let [n (suitable-hotbar inv held)]
              (concat (select-deltas eid n)
                      [[:set-slot eid (+ 36 n) (get inv slot)]
                       [:set-slot eid slot (get inv (+ 36 n))]])))
          (let [n    (suitable-hotbar inv held)
                cur  (get inv (+ 36 n))
                free (when cur (free-slot inv))]
            (concat (select-deltas eid n)
                    (when free [[:set-slot eid free cur]])
                    [[:set-slot eid (+ 36 n) {:item item :count 1}]])))))))

(defn- click-deltas
  "A click on the player's own inventory: the menu applies it; what the
   client predicted for the slots and the cursor becomes its remote copy
   (vanilla setRemoteSlot/setRemoteCarried), the player system then sends
   only what differs."
  [world [_ eid {:keys [changed carried] :as m}]]
  (when-let [e (get-in world [:entities eid])]
    (let [before {:inventory (or (:inventory e) {}) :carried (:carried e) :quickcraft (:quickcraft e)}
          after  (menu/click before m)]
      (concat
       [[:merge-entity eid (select-keys after [:inventory :carried :quickcraft])]
        [:client-slots eid (or changed {}) carried]]
       (map-indexed (fn [i stack] [:spawn-entity (items/dropped world eid stack true i)])
                    (:drops after))))))

(defn- inventory-deltas [world events]
  (concat (restore-deltas world events)
          (mapcat #(when (= :click (first %)) (click-deltas world %)) events)
          (mapcat #(when (= :pick (first %)) (pick-deltas world %)) events)))

(defn inventory [world events]
  [#(inventory-deltas world events)])
