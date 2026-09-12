(ns collider.game.systems.inventory
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.block.menu :as menu]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn- restore-deltas [world events]
  (for [[tag eid] events
        :when (= :player-join tag)
        :let [e (get-in world [:entities eid])
              inv (:inventory e)]
        msg (cond-> [(out/to eid (out/held-slot (long (or (:held-slot e) 0))))]
                    (seq inv) (conj (out/to eid (out/inventory (mapv inv (range menu/slot-count)) (:carried e)))))]
    msg))

(defn- item-of [name]
  (when (contains? (get @data/registries "item") name) name))

(def ^:private cloned-kinds #{:banner :decorated-pot :shulker-box})

(defn- cloned-stack [world pos item]
  (let [e (be/at world pos)]
    (if (contains? cloned-kinds (:kind e))
      (be/to-stack item e)
      {:item item :count 1})))

(defn- pick-item [world {:keys [pos entity include-data]}]
  (cond
    pos (let [st (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos)]
          (when (pos? (long st))
            (when-let [item (item-of (block/block-of (long st)))]
              (if include-data
                (cloned-stack world pos item)
                {:item item :count 1}))))
    entity (when-let [t (get-in world [:entities entity :type])]
             (when-let [item (item-of (keyword (str (name t) "-spawn-egg")))]
               {:item item :count 1}))))

(def ^:private scan-order
  (vec (concat (range 36 45) (range 9 36))))

(defn- same-item? [a b]
  (and (some? a) (= (dissoc a :count) (dissoc b :count))))

(defn- slot-with [inv stack]
  (some (fn [slot] (when (same-item? (get inv slot) stack) slot)) scan-order))

(defn- free-slot [inv]
  (some (fn [slot] (when-not (get inv slot) slot)) scan-order))

(defn- suitable-hotbar [inv ^long held]
  (or (some (fn [i] (let [n (mod (+ held (long i)) 9)]
                      (when-not (get inv (+ 36 n)) n)))
            (range 9))
      held))

(defn- select-deltas [eid ^long n]
  [[:merge-entity eid {:held-slot n}] (out/to eid (out/held-slot n))])

(defn- pick-deltas [world [_ eid what]]
  (when-let [e (get-in world [:entities eid])]
    (when-let [stack (pick-item world what)]
      (let [inv (:inventory e)
            held (long (or (:held-slot e) 0))]
        (if-let [slot (slot-with inv stack)]
          (if (<= 36 (long slot) 44)
            (select-deltas eid (- (long slot) 36))
            (let [n (suitable-hotbar inv held)]
              (concat (select-deltas eid n)
                      [[:set-slot eid (+ 36 n) (get inv slot)]
                       [:set-slot eid slot (get inv (+ 36 n))]])))
          (let [n (suitable-hotbar inv held)
                cur (get inv (+ 36 n))
                free (when cur (free-slot inv))]
            (concat (select-deltas eid n)
                    (when free [[:set-slot eid free cur]])
                    [[:set-slot eid (+ 36 n) stack]])))))))

(defn- click-deltas [world [_ eid {:keys [changed carried] :as m}]]
  (when-let [e (get-in world [:entities eid])]
    (let [before {:inventory (or (:inventory e) {}) :carried (:carried e) :quickcraft (:quickcraft e)}
          after (menu/click before m)]
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
