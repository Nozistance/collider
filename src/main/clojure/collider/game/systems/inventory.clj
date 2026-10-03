(ns collider.game.systems.inventory
  "Player inventory clicks and item picks."
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.block.crafting :as crafting]
            [collider.game.block.menu :as menu]
            [collider.game.deltas :as deltas]
            [collider.game.item :as item]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.slots :as slots]
            [collider.game.stack :as stack]
            [collider.game.player :as player]
            [collider.game.systems.containers :as containers]
            [collider.game.reach :as reach]
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

(defn- item-of [k]
  (when (contains? (get (data/registries) "item") k) k))

(def ^:private own-kinds #{:banner :decorated-pot})

(defn- with-data [s world e]
  (let [d (be/entity-data e (:tick world))]
    (stack/put s :block-entity-data d)))

(defn- with-entity [s world pos include-data]
  (let [e (be/at world pos)
        data? (and e include-data)
        s (if (or data? (contains? own-kinds (:kind e)))
            (be/to-stack (:item s) e)
            s)]
    (cond-> s data? (with-data world e))))

(defn- with-props [s st include-data]
  (let [props (block/props-of st)
        pair (fn [k] [(block/prop-name k) (name (get props k))])
        v (into {} (map pair) (block/clone-props st include-data))]
    (cond-> s (seq v) (stack/put :block-state v))))

(defn- picked-block [world pos include-data]
  (let [st (chunk/at (:chunks world) pos)]
    (when-let [item (block/clone-of st)]
      (-> {:item item :count 1}
          (with-entity world pos include-data)
          (with-props st include-data)))))

(defn- picked-egg [t]
  (when-let [item (item-of (keyword (str (name t) "-spawn-egg")))]
    {:item item :count 1}))

(defn- picked-entity [world id]
  (when-let [e (get-in world [:entities id])]
    (case (:type e)
      :painting {:item :painting :count 1}
      (:item-frame :glow-item-frame)
      (or (:stack e) {:item (:type e) :count 1})
      (picked-egg (:type e)))))

(defn- pick-item [world e {:keys [pos entity include-data]}]
  (cond
    (and pos (reach/in-reach? e pos))
    (picked-block world pos
                  (and (player/infinite-materials? e) include-data))
    entity (picked-entity world entity)))

(def ^:private ^:const hotbar-size 9)

(def ^:private scan-order
  (into (vec (range slots/hotbar (inc slots/hotbar-end))) slots/main))

(defn- slot-with [inv stack]
  (some (fn [slot]
          (when (stack/same-kind? (get inv slot) stack) slot))
        scan-order))

(defn- free-slot [inv]
  (some (fn [slot] (when-not (get inv slot) slot)) scan-order))

(defn- enchanted? [s]
  (boolean (seq (stack/component s :enchantments))))

(defn- hotbar-where [inv ^long held pred]
  (some (fn [i]
          (let [n (mod (+ held (long i)) hotbar-size)]
            (when (pred (get inv (+ slots/hotbar n))) n)))
        (range hotbar-size)))

(defn- suitable-hotbar [inv ^long held]
  (or (hotbar-where inv held nil?)
      (hotbar-where inv held (complement enchanted?))
      held))

(defn- select-deltas [eid ^long n]
  [[:merge-entity eid {:held-slot n}] (out/to eid (out/held-slot n))])

(defn- swap-into-hotbar [eid inv slot n]
  (let [n (long n)]
    (concat (select-deltas eid n)
            [[:set-slot eid (+ slots/hotbar n) (get inv slot)]
             [:set-slot eid slot (get inv (+ slots/hotbar n))]])))

(defn- stash-into-hotbar [eid inv stack n]
  (let [n (long n)
        cur (get inv (+ slots/hotbar n))
        free (when cur (free-slot inv))]
    (concat (select-deltas eid n)
            (when free [[:set-slot eid free cur]])
            [[:set-slot eid (+ slots/hotbar n) stack]])))

(defn- pick-deltas [world [_ eid what]]
  (when-let [e (get-in world [:entities eid])]
    (when-let [stack (pick-item world e what)]
      (let [inv (:inventory e)
            held (long (or (:held-slot e) 0))
            slot (slot-with inv stack)
            n (suitable-hotbar inv held)]
        (cond
          (and slot (<= slots/hotbar (long slot) slots/hotbar-end))
          (select-deltas eid (- (long slot) slots/hotbar))
          slot (swap-into-hotbar eid inv slot n)
          (player/infinite-materials? e)
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
        (containers/sound-deltas world eid after)
        (item/thrown-deltas world eid (:drops after))
        (equip-deltas world eid after)))))

(defn- resent-deltas [eid e]
  (let [inv (or (:inventory e) {})
        slots (mapv inv (range menu/slot-count))]
    [(out/to eid (out/inventory slots (:carried e)))]))

(defn- own-click-deltas
  [world [tag eid packet]]
  (let [e (get-in world [:entities eid])]
    (when (and e (nil? (:menu e))
               (zero? (long (:container packet 0))))
      (if (game-mode/spectator? e)
        (resent-deltas eid e)
        (click-deltas world [tag eid (dissoc packet :container)])))))

(defn- bundle-select-deltas [world [_ eid slot i]]
  (let [e (get-in world [:entities eid])]
    (when (and e (nil? (:menu e)))
      (let [after (menu/select-bundle (own-start world e) slot i)]
        [[:merge-entity eid (select-keys after [:inventory])]]))))

(defn event-deltas
  "Returns the deltas of one inventory event of its player."
  [world [tag :as ev]]
  (vec (case tag
         (:click :menu-click) (own-click-deltas world ev)
         :bundle-select (bundle-select-deltas world ev)
         :pick (pick-deltas world ev)
         nil)))

(defn inventory
  "Returns what a joining player is told about its inventory."
  {:wake {:deltas #{:player-placed}}}
  [world d]
  (deltas/of-vec (restore-deltas world (player/joins d))))
