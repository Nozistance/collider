(ns collider.game.systems.containers
  "Container menus for chests, barrels, lecterns and benches."
  (:require [collider.game.block.screen :as screen]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.inventory :as inventory]
            [collider.game.item :as item]
            [collider.game.mode :as game-mode]
            [collider.game.block.anvil :as anvil]
            [collider.game.block.container :as container]
            [collider.game.block.crafting :as crafting]
            [collider.game.block.enchanting :as enchanting]
            [collider.game.block.menu :as menu]
            [collider.game.out :as out]
            [collider.game.apply :as apply]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.random :as random]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(defn- flat [contents inv ^long n]
  (into (into {} (keep-indexed (fn [i s] (when s [i s]))) contents)
        (map (fn [[k v]] [(+ n (long k)) v]))
        inv))

(defn- split-flat [m ^long n]
  (let [tail (fn [[k v]]
               (when (>= (long k) n) [(- (long k) n) v]))]
    [(mapv #(get m %) (range n))
     (into {} (keep tail) m)]))

(defn- opened [world eid e m id]
  (let [contents (container/items world eid m)
        slots (screen/view m contents (:inventory e))]
    {:menu  (assoc m :id id :state-id 1
                     :remote (screen/remote-slots slots)
                     :remote-data (container/data-values world m)
                     :remote-carried (screen/remote-of (:carried e)))
     :slots slots}))

(defn- open-screen-deltas [world eid m id slots carried]
  (let [screen (:screen m (:type m))
        one (fn [i v] (out/to eid (out/container-data id i v)))]
    (concat
      [(out/to eid (out/open-screen id screen (:title m)))
       (out/to eid (out/container-content id 1 slots carried))]
      (when (and (container/bench? m) (contains? m :selected))
        [(one 0 (:selected m))])
      (when (container/lectern? m)
        [(one 0 (container/page world m))])
      (map-indexed one (container/data-values world m)))))

(def ^:private open-stats
  {:crafting-table :custom/interact-with-crafting-table
   :stonecutter    :custom/interact-with-stonecutter
   :loom           :custom/interact-with-loom
   :furnace        :custom/interact-with-furnace
   :blast-furnace  :custom/interact-with-blast-furnace
   :smoker         :custom/interact-with-smoker
   :brewing-stand  :custom/interact-with-brewingstand
   :anvil          :custom/interact-with-anvil
   :grindstone     :custom/interact-with-grindstone
   :smithing-table :custom/interact-with-smithing-table})

(def ^:private block-stats
  {:chest                    :custom/open-chest
   :copper-chest             :custom/open-chest
   :weathering-copper-chest  :custom/open-chest
   :trapped-chest            :custom/trigger-trapped-chest
   :barrel                   :custom/open-barrel
   :shulker-box              :custom/open-shulker-box
   :ender-chest              :custom/open-enderchest})

(defn- open-stat [world pos m]
  (or (open-stats (:type m))
      (block-stats (block/type-of
                     (container/state-at (:chunks world) pos)))))

(defn- open-menu-deltas [world eid e m]
  (let [m (container/for-player m e)
        prev (screen/close-deltas world eid e true)
        e' (cond-> e prev (assoc :carried nil :menu nil))
        id (inc (mod (long (:container-counter e 0)) 100))
        {:keys [menu slots]} (opened world eid e' m id)]
    (concat
      prev
      [[:merge-entity eid {:menu menu :container-counter id}]]
      (open-screen-deltas world eid m id slots (:carried e')))))

(defn open-deltas
  "Returns the deltas of player eid opening the menu of block pos."
  [world eid pos]
  (if-let [m (container/menu-at world pos)]
    (let [e (get-in world [:entities eid])
          stat (open-stat world pos m)]
      (concat
        (open-menu-deltas world eid e m)
        (when stat [[:award eid stat 1]])
        (screen/opener-deltas world e m 1)))
    []))

(defn spectator-open-deltas
  "Returns the deltas of a spectator using a block with a menu.
  The menu opens with no stat and no opener. Returns nil when the
  block has none."
  [world eid pos]
  (when-let [m (container/provider-at world pos)]
    (open-menu-deltas world eid (get-in world [:entities eid]) m)))

(defn- with-client [menu changed carried]
  (let [put (fn [r [s v]] (assoc r (long s) (screen/remote-of v)))
        remote (reduce put (:remote menu) changed)]
    (assoc menu :remote remote
                :remote-carried (screen/remote-of carried))))

(defn- slot-changes [before after]
  (let [moved (fn [slot]
                (when (not= (get after slot) (get before slot))
                  [slot (get after slot)]))
        ks (into (set (keys before)) (keys after))]
    (into {} (keep moved) ks)))

(defn- stale-result [m items items']
  (if (and (container/crafting? m)
           (not= (container/inputs m items)
                 (container/inputs m items')))
    (assoc-in m [:remote 0] ::stale)
    m))

(defn- click-start [e m items ctx]
  {:inventory  (flat items (or (:inventory e) {})
                     (container/slot-count m))
   :carried    (:carried e)
   :quickcraft (:quickcraft e)
   :held       (:held ctx)
   :creative?  (:infinite? ctx)
   :layout     (container/layout m ctx)})

(defn- menu-after [m m0 items items' packet]
  (-> (cond-> m0 (container/bench? m0) (assoc :contents items'))
      (with-client (:changed packet) (:carried packet))
      (stale-result items items')))

(defn- menu-context [world e m]
  (cond-> (crafting/context world e)
    (container/enchanting? m)
    (assoc :shelves (enchanting/shelves world (:pos m)))))

(defn- clicked [world eid e m packet]
  (let [items (container/items world eid m)
        ctx (menu-context world e m)
        after (menu/click (click-start e m items ctx) packet)
        n (container/slot-count m)
        [items0 inv'] (split-flat (:inventory after) n)
        [m0 items'] (container/settled m items0 ctx)
        m' (menu-after m m0 items items' packet)
        sent (long (:state-id packet))
        resync? (not= sent (long (:state-id m 1)))
        slots (screen/view m items' inv')
        carried (:carried after)
        synced (screen/sync-deltas eid m' slots carried resync?)]
    (assoc synced :after after :inventory inv' :items items')))

(defn craft-deltas
  "Returns the stats and spills of player eid crafting."
  [world eid after]
  (concat
    (for [[item n] (:crafted after)]
      [:award eid (keyword "crafted" (name item)) n])
    (for [s (:spills after)]
      [:spawn-entity (item/dropped world eid s)])))

(def ^:private bundle-sounds
  {:insert      :item.bundle.insert
   :insert-fail :item.bundle.insert-fail
   :remove-one  :item.bundle.remove-one})

(defn- bundle-sound [world eid pos i kind]
  (let [r (random/of-key (:tick world) eid :bundle i)
        fail? (= :insert-fail kind)
        snd (out/sound (bundle-sounds kind) pos (if fail? 1.0 0.8)
                       (if fail? 1.0 (+ 0.8 (* 0.4 r))) :players)]
    (out/except eid snd)))

(defn sound-deltas
  "Returns the sounds a click of player eid made bundles play."
  [world eid after]
  (let [pos (get-in world [:entities eid :pos])]
    (map-indexed #(bundle-sound world eid pos %1 %2)
                 (:sounds after))))

(defn- value-deltas [world eid m menu]
  (let [old (container/data-values world m)
        id (:id menu)
        one (fn [i v]
              (when (not= (nth old i nil) v)
                (out/to eid (out/container-data id i v))))]
    (keep-indexed one (container/data-values world menu))))

(defn- take-deltas [world e m takes]
  (when (pos? (long takes))
    (case (:type m)
      :anvil (container/anvil-take-deltas
               world m (player/infinite-materials? e))
      (keep identity [(container/take-sound m)]))))

(defn- click-merge-deltas [eid e after inventory menu]
  (let [changes (slot-changes (or (:inventory e) {}) inventory)
        carried (:carried after)
        fields {:inventory  inventory :carried carried
                :quickcraft (:quickcraft after) :menu menu}]
    [[:merge-entity eid fields]
     [:client-slots eid changes carried]]))

(defn- selected-deltas [eid m menu]
  (when (not= (:selected m) (:selected menu))
    (let [p (out/container-data (:id menu) 0 (:selected menu))]
      [(out/to eid p)])))

(defn- click-result-deltas [world eid e m click]
  (let [{:keys [after inventory items deltas menu]} click]
    (concat
      (click-merge-deltas eid e after inventory menu)
      (container/store-deltas world eid m items)
      deltas
      (selected-deltas eid m menu)
      (value-deltas world eid m menu)
      (take-deltas world e m (long (:takes after 0)))
      (craft-deltas world eid after)
      (sound-deltas world eid after)
      (item/thrown-deltas world eid (:drops after)))))

(defn- all-data-deltas [world eid e m]
  (let [st (bit-and (inc (long (:state-id m 1))) 32767)
        items (container/items world eid m)
        slots (screen/view m items (:inventory e))
        data (container/data-values world m)
        {:keys [deltas menu]}
        (screen/resync-deltas eid m slots (:carried e) st)
        one (fn [i v] (out/to eid (out/container-data (:id m) i v)))
        menu (assoc menu :remote-data data)]
    (concat deltas (map-indexed one data)
            [[:merge-entity eid {:menu menu}]])))

(defn- click-deltas [world [_ eid packet]]
  (when-let [e (get-in world [:entities eid])]
    (let [m (:menu e)
          want (:container packet)
          same? (and m (= (long want) (long (:id m))))]
      (cond
        (not same?) nil
        (game-mode/spectator? e) (all-data-deltas world eid e m)
        (container/lectern? m) nil
        (not (screen/valid? world m))
        (screen/close-deltas world eid e true)
        :else
        (let [c (clicked world eid e m packet)]
          (click-result-deltas world eid e m c))))))

(defn- page-button-deltas [world eid m ^long want]
  (when-let [ds (container/page-deltas world m want)]
    (let [page (container/next-page world m want)
          p (out/container-data (:id m) 0 page)]
      (concat ds [(out/to eid p)]))))

(defn- take-book-deltas
  [world eid e m]
  (when-let [book (container/book-of world m)]
    (let [inv (or (:inventory e) {})
          [changes left] (inventory/add-stack inv book)
          state-id (inc (long (:state-id m 1)))]
      (concat
        (container/remove-book-deltas world (:pos m))
        [(out/to eid (out/container-slot (:id m) state-id 0 nil))]
        (when (not= 0 (container/page world m))
          [(out/to eid (out/container-data (:id m) 0 0))])
        (for [[slot s] changes] [:set-slot eid slot s])
        (when left [[:spawn-entity (item/dropped world eid left)]])
        (screen/close-deltas world eid e true)))))

(defn- lectern-button-deltas [world eid e m id]
  (let [id (long id)
        page (container/page world m)]
    (cond
      (>= id 100) (page-button-deltas world eid m (- id 100))
      (= 1 id) (page-button-deltas world eid m (dec page))
      (= 2 id) (page-button-deltas world eid m (inc page))
      (= 3 id) (take-book-deltas world eid e m)
      :else nil)))

(defn- bench-button-deltas [eid e m id]
  (let [m' (container/button m (long id))]
    (when (not= (:selected m') (:selected m))
      (let [items (container/derived m' (:contents m'))
            slots (screen/view m' items (:inventory e))
            m'' (assoc m' :contents items)
            carried (:carried e)
            synced (screen/sync-deltas eid m'' slots carried false)
            menu (:menu synced)
            p (out/container-data (:id menu) 0 (:selected menu))]
        (concat
          [[:merge-entity eid {:menu menu}]]
          (:deltas synced)
          [(out/to eid p)])))))

(defn- enchant-sound [world pos]
  (let [r (random/of-key (:tick world) pos :enchant)
        at (mapv #(+ 0.5 (double %)) pos)]
    (out/all (out/sound :block.enchantment-table.use at 1.0
                        (+ 0.9 (* 0.1 r))))))

(defn- levels-paid [e ^long cost]
  (let [n (- (long (:xp-level e 0)) cost)]
    (cond-> {:xp-level (max n 0) :xp-sent nil}
      (neg? n) (assoc :xp-progress 0.0 :xp-total 0))))

(defn- enchant-deltas [world eid e m enchanted lapis cost]
  (let [seed (enchanting/next-seed
               (random/of-key (:tick world) eid :enchantment-seed))
        items [enchanted lapis]
        shelves (enchanting/shelves world (:pos m))
        m' (merge (assoc m :contents items :seed seed)
                  (enchanting/offers seed shelves enchanted))
        slots (screen/view m' items (:inventory e))
        synced (screen/sync-deltas eid m' slots (:carried e) false)
        {:keys [deltas menu]} synced
        paid (assoc (levels-paid e cost) :enchantment-seed seed)]
    (concat
      [[:merge-entity eid (assoc paid :menu menu)]]
      deltas
      (value-deltas world eid m menu)
      [[:award eid :custom/enchant-item 1]
       (enchant-sound world (:pos m))])))

(defn- paid-lapis [e lapis ^long n]
  (if (player/infinite-materials? e)
    lapis
    (let [left (- (long (:count lapis 0)) n)]
      (when (pos? left) (assoc lapis :count left)))))

(defn- enchant-button-deltas [world eid e m id]
  (let [[item lapis] (:contents m)
        id (long id)
        n (inc id)
        cost (long (nth (:costs m) id 0))
        free? (player/infinite-materials? e)
        level (long (:xp-level e 0))]
    (when (and (< -1 id 3)
               (or free? (>= (long (:count lapis 0)) n))
               (pos? cost) item
               (or free? (and (>= level n) (>= level cost))))
      (when-let [s (enchanting/enchanted (:seed m) id cost item)]
        (enchant-deltas world eid e m s (paid-lapis e lapis n) n)))))

(defn- renamed-deltas [world eid e m]
  (let [ctx (crafting/context world e)
        [m' items] (container/settled m (:contents m) ctx)
        slots (screen/view m' items (:inventory e))
        m'' (assoc m' :contents items)
        synced (screen/sync-deltas eid m'' slots (:carried e) false)
        menu (:menu synced)]
    (concat [[:merge-entity eid {:menu menu}]]
            (:deltas synced)
            (value-deltas world eid m menu))))

(defn- rename-deltas [world [_ eid text]]
  (when-let [e (get-in world [:entities eid])]
    (let [m (:menu e)
          nm (anvil/valid-name text)]
      (when (and m (= :anvil (:type m)) (screen/valid? world m)
                 nm (not= nm (:name m)))
        (renamed-deltas world eid e (assoc m :name nm))))))

(defn- button-deltas [world [_ eid container id]]
  (when-let [e (get-in world [:entities eid])]
    (let [m (:menu e)]
      (when (and m (= (long container) (long (:id m)))
                 (not (game-mode/spectator? e)))
        (cond
          (container/lectern? m)
          (when (screen/valid? world m)
            (lectern-button-deltas world eid e m (long id)))
          (container/enchanting? m)
          (when (screen/valid? world m)
            (enchant-button-deltas world eid e m (long id)))
          (container/bench? m)
          (bench-button-deltas eid e m (long id)))))))

(defn- inventory-close-deltas [world eid e]
  (let [inv (or (:inventory e) {})
        stacks (cons (:carried e) (map inv (rest screen/own-grid)))
        ctx (crafting/context world e)
        kept (apply dissoc inv screen/own-grid)
        [inv' drops] (screen/put-back ctx kept stacks)]
    (concat
      (when (:carried e) [[:merge-entity eid {:carried nil}]])
      (for [[slot s] (screen/changed-slots inv inv')]
        [:set-slot eid slot s])
      (for [s drops] [:spawn-entity (item/dropped world eid s)]))))

(defn- quit-deltas [world]
  (mapcat (fn [e]
            (let [eid (:eid e)
                  w (assoc-in world [:entities eid] e)]
              (delta/authored (screen/left-behind-deltas w eid e)
                              eid :player)))
          (get-in world [:input :quits])))

(defn- close-event-deltas [world [_ eid _]]
  (when-let [e (get-in world [:entities eid])]
    (if (:menu e)
      (screen/close-deltas world eid e false)
      (inventory-close-deltas world eid e))))

(defn- selected-bundle-deltas [world eid e m slot i]
  (let [items (container/items world eid m)
        ctx (crafting/context world e)
        start (click-start e m items ctx)
        after (menu/select-bundle start slot i)
        n (container/slot-count m)
        [items' inv] (split-flat (:inventory after) n)
        m' (cond-> m (container/bench? m) (assoc :contents items'))]
    (cons [:merge-entity eid {:inventory inv :menu m'}]
          (container/store-deltas world eid m items'))))

(defn- bundle-select-deltas [world [_ eid slot i]]
  (let [e (get-in world [:entities eid])
        m (:menu e)]
    (when (and m (not (container/lectern? m)) (screen/valid? world m))
      (selected-bundle-deltas world eid e m slot i))))

(defn- event-deltas [world [tag :as ev]]
  (case tag
    :menu-click (click-deltas world ev)
    :menu-close (close-event-deltas world ev)
    :menu-button (button-deltas world ev)
    :rename-item (rename-deltas world ev)
    :bundle-select (bundle-select-deltas world ev)
    nil))

(defn- containers-deltas [world events]
  (concat
    (container/animate-deltas world)
    (quit-deltas world)
    (apply/fold-events world events event-deltas)))

(defn containers
  "Returns the deltas of the menus and containers of the tick."
  {:wake {:events #{:menu-click :menu-close :menu-button
                    :rename-item :bundle-select}
          :keys [:shulker-anim [:input :quits]]}}
  [world d]
  (let [events (:input d)]
    (deltas/of-vec (containers-deltas world events))))
