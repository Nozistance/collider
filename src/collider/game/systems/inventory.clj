(ns collider.game.systems.inventory
  "Player inventory clicks and item picks."
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.block.crafting :as crafting]
            [collider.game.block.menu :as menu]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.game.state :as state]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.blocks.reach :as reach]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- join-msgs [world eid]
  (let [e (get-in world [:entities eid])
        inv (:inventory e)
        held (long (or (:held-slot e) 0))
        slots (mapv inv (range menu/slot-count))]
    (cond-> [(out/to eid (out/held-slot held))]
      (seq inv)
      (conj (out/to eid (out/inventory slots (:carried e)))))))

(defn- restore-deltas [world joins]
  (into [] (mapcat (fn [[tag eid]]
                     (when (= :player-join tag)
                       (join-msgs world eid))))
        joins))

(defn- item-of [name]
  (when (contains? (get (data/registries) "item") name) name))

(def ^:private own-kinds #{:banner :decorated-pot})

(defn- with-entity [s world pos include-data]
  (let [e (be/at world pos)]
    (if (or (contains? own-kinds (:kind e)) (and e include-data))
      (be/to-stack (:item s) e)
      s)))

(defn- with-props [s st include-data]
  (let [props (block/props-of st)
        pair (fn [k] [(block/prop-name k) (name (get props k))])
        v (into {} (map pair) (block/clone-props st include-data))]
    (cond-> s (seq v) (stack/put :block-state v))))

(defn- picked-block [world pos include-data]
  (let [st (chunk/chunks-get-block (:chunks world) pos)]
    (when-let [item (block/clone-of st)]
      (-> {:item item :count 1}
          (with-entity world pos include-data)
          (with-props st include-data)))))

(defn- picked-entity [world id]
  (when-let [t (get-in world [:entities id :type])]
    (let [egg (keyword (str (name t) "-spawn-egg"))]
      (when-let [item (item-of egg)]
        {:item item :count 1}))))

(defn- pick-item [world e {:keys [pos entity include-data]}]
  (cond
    (and pos (reach/in-reach? e pos))
    (picked-block world pos
                  (and (state/infinite-materials? e) include-data))
    entity (picked-entity world entity)))

(def ^:private scan-order
  (vec (concat (range 36 45) (range 9 36))))

(defn- same-item? [a b]
  (and (some? a) (= (dissoc a :count) (dissoc b :count))))

(defn- slot-with [inv stack]
  (some (fn [slot]
          (when (same-item? (get inv slot) stack) slot))
        scan-order))

(defn- free-slot [inv]
  (some (fn [slot] (when-not (get inv slot) slot)) scan-order))

(defn- enchanted? [s]
  (boolean (seq (stack/component s :enchantments))))

(defn- hotbar-where [inv ^long held pred]
  (some (fn [i] (let [n (mod (+ held (long i)) 9)]
                  (when (pred (get inv (+ 36 n))) n)))
        (range 9)))

(defn- suitable-hotbar [inv ^long held]
  (or (hotbar-where inv held nil?)
      (hotbar-where inv held (complement enchanted?))
      held))

(defn- select-deltas [eid ^long n]
  [[:merge-entity eid {:held-slot n}] (out/to eid (out/held-slot n))])

(defn- swap-into-hotbar [eid inv slot n]
  (let [n (long n)]
    (concat (select-deltas eid n)
            [[:set-slot eid (+ 36 n) (get inv slot)]
             [:set-slot eid slot (get inv (+ 36 n))]])))

(defn- stash-into-hotbar [eid inv stack n]
  (let [n (long n)
        cur (get inv (+ 36 n))
        free (when cur (free-slot inv))]
    (concat (select-deltas eid n)
            (when free [[:set-slot eid free cur]])
            [[:set-slot eid (+ 36 n) stack]])))

(defn- pick-deltas [world [_ eid what]]
  (when-let [e (get-in world [:entities eid])]
    (when-let [stack (pick-item world e what)]
      (let [inv (:inventory e)
            held (long (or (:held-slot e) 0))
            slot (slot-with inv stack)
            n (suitable-hotbar inv held)]
        (cond
          (and slot (<= 36 (long slot) 44))
          (select-deltas eid (- (long slot) 36))
          slot (swap-into-hotbar eid inv slot n)
          (state/infinite-materials? e)
          (stash-into-hotbar eid inv stack n)
          :else (select-deltas eid held))))))

(defn- own-start [world e]
  (let [ctx (crafting/context world e)]
    {:inventory  (or (:inventory e) {})
     :carried    (:carried e)
     :quickcraft (:quickcraft e)
     :held       (:held ctx)
     :creative?  (:infinite? ctx)
     :layout     (crafting/player-layout ctx)}))

(defn- equip-sound [pos s]
  (let [kind (data/equip-sound (:item s))]
    (out/all (out/sound kind pos 1.0 1.0 :players))))

(defn- equip-deltas [world eid after]
  (let [pos (get-in world [:entities eid :pos])]
    (map #(equip-sound pos %) (:equipped after))))

(defn- click-deltas [world [_ eid {:keys [changed carried] :as m}]]
  (when-let [e (get-in world [:entities eid])]
    (let [after (menu/click (own-start world e) m)
          own (select-keys after [:inventory :carried :quickcraft])]
      (concat
        [[:merge-entity eid own]
         [:client-slots eid (or changed {}) carried]]
        (containers/craft-deltas world eid after)
        (items/thrown-deltas world eid (:drops after))
        (equip-deltas world eid after)))))

(defn- own-click-deltas [world [tag eid packet]]
  (when (zero? (long (:container packet 0)))
    (click-deltas world [tag eid (dissoc packet :container)])))

(defn event-deltas
  "Returns the deltas of one inventory event of its player."
  [world [tag :as ev]]
  (vec (case tag
         :click (click-deltas world ev)
         :menu-click (own-click-deltas world ev)
         :pick (pick-deltas world ev)
         nil)))

(defn inventory
  "Returns what a joining player is told about his inventory."
  [world d]
  [#(restore-deltas world (state/joins d))])
