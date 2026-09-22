(ns collider.game.systems.containers
  "Container menus for chests, barrels, lecterns and benches."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.anvil :as anvil]
            [collider.game.block.container :as container]
            [collider.game.block.crafting :as crafting]
            [collider.game.block.menu :as menu]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private player-view (vec (concat (range 9 36) (range 36 45))))

(defn- view [m contents inv]
  (if (container/player-slots? m)
    (into (vec contents) (map inv) player-view)
    (vec contents)))

(defn- flat [contents inv ^long n]
  (into (into {} (keep-indexed (fn [i s] (when s [i s]))) contents)
        (map (fn [[k v]] [(+ n (long k)) v]))
        inv))

(defn- split-flat [m ^long n]
  (let [tail (fn [[k v]]
               (when (>= (long k) n) [(- (long k) n) v]))]
    [(mapv #(get m %) (range n))
     (into {} (keep tail) m)]))

(defn- remote-of [stack]
  (when stack
    (cond-> {:item (:item stack) :count (long (:count stack 1))}
      (:components stack) (assoc :components (:components stack))
      (:components? stack) (assoc :components? true))))

(defn- hashed [r]
  (when r
    (assoc (dissoc r :components)
      :components? (boolean (or (:components r) (:components? r))))))

(defn- remote-match?
  "Returns true when the client already has stack in that slot. What
  it told us of its own components is only whether it has any, so a
  slot matches every stack that agrees on that much."
  [remote stack]
  (let [r (remote-of stack)]
    (if (:components? remote) (= remote (hashed r)) (= remote r))))

(defn- count-deltas [world m ^long step]
  (let [at (fn [pos]
             (let [before (container/viewers world pos)
                   after (+ before step)]
               (container/count-deltas world pos before after)))]
    (mapcat at (container/positions m))))

(defn- barrel-toggle [world pos ^long step]
  (let [st (container/state-at (:chunks world) pos)
        before (container/viewers world pos)
        after (+ before step)
        open? (pos? after)]
    (when (and (= :barrel (block/type-of st))
               (or (and (zero? before) open?) (zero? after)))
      [[:set-blocks
        [[pos (container/barrel-open-state st open?)]]]])))

(defn- barrel-deltas [world m ^long step]
  (mapcat #(barrel-toggle world % step)
          (container/positions m)))

(defn- bench-valid? [world m]
  (let [st (container/state-at (:chunks world) (:pos m))]
    (= (:type m) (block/type-of st))))

(defn- valid?
  "Returns true when the block a menu belongs to is still there."
  [world m]
  (cond
    (container/lectern? m)
    (and (= :lectern (:kind (be/at world (:pos m))))
         (container/book? (container/book-of world m)))
    (container/bench? m) (bench-valid? world m)
    (= :ender (:kind m)) true
    :else
    (every? (fn [pos]
              (contains? be/menu-kinds (:kind (be/at world pos))))
            (:cells m))))

(defn- put-back [ctx inv stacks]
  (reduce (fn [[inv drops] s]
            (let [[inv' left] (crafting/place-back ctx inv s)]
              [inv' (cond-> drops left (conj left))]))
          [inv []]
          (remove nil? stacks)))

(defn- changed-slots [before after]
  (let [moved (fn [k]
                (when (not= (get before k) (get after k))
                  [k (get after k)]))
        ks (into (set (keys before)) (keys after))]
    (sort-by key (into {} (keep moved) ks))))

(defn- bench-inputs [world eid m]
  (when (container/bench? m)
    (container/inputs m (container/items world eid m))))

(defn- closed-inventory [world eid e m]
  (let [inv (or (:inventory e) {})
        ctx (crafting/context world e)
        stacks (cons (:carried e) (bench-inputs world eid m))
        [inv' drops] (put-back ctx inv stacks)]
    [(changed-slots inv inv') drops]))

(defn- close-out-deltas [eid m carried notify?]
  (concat
    (when carried [(out/to eid (out/carried nil))])
    (when notify? [(out/to eid (out/container-close (:id m)))])))

(defn- close-deltas [world eid e notify?]
  (when-let [m (:menu e)]
    (let [[changes drops] (closed-inventory world eid e m)]
      (concat
        [[:merge-entity eid {:menu nil :carried nil}]]
        (for [[slot s] changes] [:set-slot eid slot s])
        (for [s drops] [:spawn-entity (items/dropped world eid s)])
        (close-out-deltas eid m (:carried e) notify?)
        (count-deltas world m -1)
        (barrel-deltas world m -1)))))

(defn- remote-slots [slots]
  (into {} (map-indexed (fn [i s] [i (remote-of s)])) slots))

(defn- opened [world eid e m id]
  (let [contents (container/items world eid m)
        slots (view m contents (:inventory e))]
    {:menu  (assoc m :id id :state-id 1
                     :remote (remote-slots slots)
                     :remote-data (container/data-values world m)
                     :remote-carried (remote-of (:carried e)))
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
  (let [prev (close-deltas world eid e true)
        e' (cond-> e prev (assoc :carried nil :menu nil))
        id (inc (mod (long (:container-counter e 0)) 100))
        {:keys [menu slots]} (opened world eid e' m id)]
    (concat
      prev
      [[:merge-entity eid {:menu menu :container-counter id}]]
      (open-screen-deltas world eid m id slots (:carried e')))))

(defn open-deltas [world eid pos]
  (if-let [m (container/menu-at world pos)]
    (concat
      (open-menu-deltas world eid (get-in world [:entities eid]) m)
      (when-let [stat (open-stat world pos m)] [[:award eid stat 1]])
      (count-deltas world m 1)
      (barrel-deltas world m 1))
    []))

(defn- synced [menu ^long st slots carried]
  (assoc menu :state-id st
              :remote (remote-slots slots)
              :remote-carried (remote-of carried)))

(defn- slot-diff-deltas [eid menu slots ^long base]
  (let [remote (:remote menu)
        stale (fn [i s]
                (when-not (remote-match? (get remote i) s)
                  [i s]))
        send (fn [[ds ^long st] [i s]]
               (let [p (out/container-slot (:id menu) (inc st) i s)]
                 [(conj ds (out/to eid p)) (inc st)]))]
    (reduce send [[] base] (keep-indexed stale slots))))

(defn- resync-deltas [eid menu slots carried st]
  (let [p (out/container-content (:id menu) st slots carried)]
    {:deltas [(out/to eid p)]
     :menu   (synced menu st slots carried)}))

(defn- sync-deltas [eid menu slots carried resync?]
  (let [base (long (:state-id menu 1))]
    (if resync?
      (resync-deltas eid menu slots carried (inc base))
      (let [[ds st] (slot-diff-deltas eid menu slots base)
            old (:remote-carried menu)
            same? (remote-match? old carried)]
        {:deltas (cond-> ds
                   (not same?)
                   (conj (out/to eid (out/carried carried))))
         :menu   (synced menu st slots carried)}))))

(defn- with-client [menu changed carried]
  (let [put (fn [r [s v]] (assoc r (long s) (remote-of v)))
        remote (reduce put (:remote menu) changed)]
    (assoc menu :remote remote
                :remote-carried (remote-of carried))))

(defn- slot-changes [before after]
  (let [moved (fn [slot]
                (when (not= (get after slot) (get before slot))
                  [slot (get after slot)]))
        ks (into (set (keys before)) (keys after))]
    (into {} (keep moved) ks)))

(defn- stale-result
  "Returns the menu with its result slot marked unseen.
  The mark is set only when the grid changed."
  [m items items']
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
   :layout     (container/layout m ctx)})

(defn- menu-after [m m0 items items' packet]
  (-> (cond-> m0 (container/bench? m0) (assoc :contents items'))
      (with-client (:changed packet) (:carried packet))
      (stale-result items items')))

(defn- clicked [world eid e m packet]
  (let [items (container/items world eid m)
        ctx (crafting/context world e)
        after (menu/click (click-start e m items ctx) packet)
        n (container/slot-count m)
        [items0 inv'] (split-flat (:inventory after) n)
        [m0 items'] (container/settled m items0 ctx)
        m' (menu-after m m0 items items' packet)
        sent (long (:state-id packet))
        resync? (not= sent (long (:state-id m 1)))
        slots (view m items' inv')
        synced (sync-deltas eid m' slots (:carried after) resync?)]
    (assoc synced :after after :inventory inv' :items items')))

(defn craft-deltas [world eid after]
  (concat
    (for [[item n] (:crafted after)]
      [:award eid (keyword "crafted" (name item)) n])
    (for [s (:spills after)]
      [:spawn-entity (items/dropped world eid s)])))

(defn- value-deltas [world eid m menu]
  (let [old (container/data-values world m)
        id (:id menu)
        one (fn [i v]
              (when (not= (nth old i nil) v)
                (out/to eid (out/container-data id i v))))]
    (keep-indexed one (container/data-values world menu))))

(defn- crafted-award [eid m ^long takes]
  (when-let [s (nth (:contents m) (long (:result m)) nil)]
    [[:award eid (keyword "crafted" (name (:item s)))
      (* takes (long (:count s 1)))]]))

(defn- take-deltas [world eid e m takes]
  (when (pos? (long takes))
    (case (:type m)
      :anvil (container/anvil-take-deltas
               world m (state/infinite-materials? e))
      :smithing-table
      (concat (crafted-award eid m (long takes))
              [(container/take-sound m)])
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

(defn- drop-deltas [world eid after]
  (let [one (fn [i s]
              [:spawn-entity
               (items/dropped world eid s true i)])]
    (map-indexed one (:drops after))))

(defn- click-result-deltas [world eid e m click]
  (let [{:keys [after inventory items deltas menu]} click]
    (concat
      (click-merge-deltas eid e after inventory menu)
      (container/store-deltas world eid m items)
      deltas
      (selected-deltas eid m menu)
      (value-deltas world eid m menu)
      (take-deltas world eid e m (long (:takes after 0)))
      (craft-deltas world eid after)
      (drop-deltas world eid after))))

(defn- click-deltas [world [_ eid packet]]
  (when-let [e (get-in world [:entities eid])]
    (let [m (:menu e)
          want (:container packet)
          same? (and m (= (long want) (long (:id m))))]
      (cond
        (not same?) nil
        (container/lectern? m) nil
        (not (valid? world m)) (close-deltas world eid e true)
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
    (let [[changes left] (items/add-stack (or (:inventory e) {}) book)
          state-id (inc (long (:state-id m 1)))]
      (concat
        (container/remove-book-deltas world (:pos m))
        [(out/to eid (out/container-slot (:id m) state-id 0 nil))]
        (when (not= 0 (container/page world m))
          [(out/to eid (out/container-data (:id m) 0 0))])
        (for [[slot s] changes] [:set-slot eid slot s])
        (when left [[:spawn-entity (items/dropped world eid left)]])
        (close-deltas world eid e true)))))

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
            slots (view m' items (:inventory e))
            m'' (assoc m' :contents items)
            synced (sync-deltas eid m'' slots (:carried e) false)
            menu (:menu synced)
            p (out/container-data (:id menu) 0 (:selected menu))]
        (concat
          [[:merge-entity eid {:menu menu}]]
          (:deltas synced)
          [(out/to eid p)])))))

(defn- renamed-deltas [world eid e m]
  (let [ctx (crafting/context world e)
        [m' items] (container/settled m (:contents m) ctx)
        slots (view m' items (:inventory e))
        m'' (assoc m' :contents items)
        synced (sync-deltas eid m'' slots (:carried e) false)
        menu (:menu synced)]
    (concat [[:merge-entity eid {:menu menu}]]
            (:deltas synced)
            (value-deltas world eid m menu))))

(defn- rename-deltas [world [_ eid text]]
  (when-let [e (get-in world [:entities eid])]
    (let [m (:menu e)
          nm (anvil/valid-name text)]
      (when (and m (= :anvil (:type m)) (valid? world m)
                 nm (not= nm (:name m)))
        (renamed-deltas world eid e (assoc m :name nm))))))

(defn- button-deltas [world [_ eid container id]]
  (when-let [e (get-in world [:entities eid])]
    (let [m (:menu e)]
      (when (and m (= (long container) (long (:id m))))
        (cond
          (container/lectern? m)
          (when (valid? world m)
            (lectern-button-deltas world eid e m (long id)))
          (container/bench? m)
          (bench-button-deltas eid e m (long id)))))))

(def ^:private own-grid [0 1 2 3 4])

(defn- inventory-close-deltas [world eid e]
  (let [inv (or (:inventory e) {})
        stacks (cons (:carried e) (map inv (rest own-grid)))
        ctx (crafting/context world e)
        kept (apply dissoc inv own-grid)
        [inv' drops] (put-back ctx kept stacks)]
    (concat
      (when (:carried e) [[:merge-entity eid {:carried nil}]])
      (for [[slot s] (changed-slots inv inv')] [:set-slot eid slot s])
      (for [s drops] [:spawn-entity (items/dropped world eid s)]))))

(defn- held-stacks [world eid e]
  (let [grid (map (or (:inventory e) {}) (rest own-grid))
        m (:menu e)]
    (remove nil?
            (if m
              (concat grid [(:carried e)] (bench-inputs world eid m))
              (cons (:carried e) grid)))))

(defn- left-behind-deltas [world eid e]
  (let [m (:menu e)]
    (concat
      (for [s (held-stacks world eid e)]
        [:spawn-entity (items/dropped world eid s)])
      (when m (count-deltas world m -1))
      (when m (barrel-deltas world m -1)))))

(defn removed-deltas [world eid e]
  (concat
    (when (or (:menu e) (:carried e))
      [[:merge-entity eid {:menu nil :carried nil}]])
    (for [slot own-grid :when (get-in e [:inventory slot])]
      [:set-slot eid slot nil])
    (left-behind-deltas world eid e)))

(defn- quit-deltas [world]
  (mapcat (fn [e]
            (let [eid (:eid e)
                  w (assoc-in world [:entities eid] e)]
              (left-behind-deltas w eid e)))
          (:quits world)))

(defn- close-event-deltas [world [_ eid _]]
  (when-let [e (get-in world [:entities eid])]
    (if (:menu e)
      (close-deltas world eid e false)
      (inventory-close-deltas world eid e))))

(defn- data-deltas
  "Diffs the data values of a menu against what the client was told."
  [eid m values]
  (let [old (:remote-data m)]
    [(keep-indexed (fn [i v]
                     (when (not= (nth old i nil) v)
                       (out/to eid (out/container-data (:id m) i v))))
                   values)
     (cond-> m values (assoc :remote-data (vec values)))]))

(defn- broadcast-deltas [world eid e]
  (let [m (:menu e)
        slots (view m (container/items world eid m) (:inventory e))
        synced (sync-deltas eid m slots (:carried e) false)
        deltas (:deltas synced)
        values (container/data-values world m)
        [ds menu] (data-deltas eid (:menu synced) values)]
    (when (or (seq deltas) (seq ds))
      (concat [[:merge-entity eid {:menu menu}]] deltas ds))))

(defn- broadcasting? [world e]
  (let [m (:menu e)]
    (and m (= :block (:kind m)) (valid? world m))))

(defn broadcast
  "Sends every viewer the slots and data of their menu that moved.
  This happens once per player tick."
  [world _d]
  [#(mapcat (fn [[eid e]]
              (when (broadcasting? world e)
                (broadcast-deltas world (long eid) e)))
            (:entities world))])

(defn- event-deltas [world [tag :as ev]]
  (case tag
    :menu-click (click-deltas world ev)
    :menu-close (close-event-deltas world ev)
    :menu-button (button-deltas world ev)
    :rename-item (rename-deltas world ev)
    nil))

(defn- containers-deltas [world events]
  (concat
    (container/animate-deltas world)
    (quit-deltas world)
    (state/fold-events world events event-deltas)
    (container/recheck-deltas world)))

(defn containers [world d]
  (let [events (:input d)]
    [#(containers-deltas world events)]))
