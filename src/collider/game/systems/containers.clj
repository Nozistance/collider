(ns collider.game.systems.containers
  "Container menus for chests, barrels, lecterns and crafting benches."
  (:require [collider.game.block.blockentity :as be]
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
  [(mapv #(get m %) (range n))
   (into {} (keep (fn [[k v]] (when (>= (long k) n) [(- (long k) n) v]))) m)])

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
  (mapcat (fn [pos]
            (let [before (container/viewers world pos)]
              (container/count-deltas world pos before (+ before step))))
          (container/positions m)))

(defn- barrel-deltas [world m ^long step]
  (mapcat (fn [pos]
            (let [st (container/state-at (:chunks world) pos)
                  before (container/viewers world pos)
                  after (+ before step)]
              (when (and (= :barrel (block/type-of st))
                         (or (and (zero? before) (pos? after)) (zero? after)))
                [[:set-blocks [[pos (container/barrel-open-state st (pos? after))]]]])))
          (container/positions m)))

(defn- valid?
  "Returns true when the block a menu belongs to is still there."
  [world m]
  (cond
    (container/lectern? m)
    (and (= :lectern (:kind (be/at world (:pos m))))
         (container/book? (container/book-of world m)))
    (container/bench? m)
    (= (:type m) (block/type-of (container/state-at (:chunks world) (:pos m))))
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
  (sort-by key
           (into {}
                 (keep (fn [k]
                         (when (not= (get before k) (get after k))
                           [k (get after k)])))
                 (into (set (keys before)) (keys after)))))

(defn- bench-inputs [world eid m]
  (when (container/bench? m)
    (container/inputs m (container/items world eid m))))

(defn- closed-inventory [world eid e m]
  (let [inv (or (:inventory e) {})
        [inv' drops] (put-back (crafting/context world e) inv
                               (cons (:carried e)
                                     (bench-inputs world eid m)))]
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

(defn- opened [world eid e m id]
  (let [contents (container/items world eid m)
        slots (view m contents (:inventory e))]
    {:menu  (assoc m :id id :state-id 1
                     :remote (into {} (map-indexed (fn [i s] [i (remote-of s)])) slots)
                     :remote-data (container/data-values world m)
                     :remote-carried (remote-of (:carried e)))
     :slots slots}))

(defn- open-screen-deltas [world eid m id slots carried]
  (concat
    [(out/to eid (out/open-screen id (:screen m (:type m))
                                  (:title m)))
     (out/to eid (out/container-content id 1 slots carried))]
    (when (and (container/bench? m) (contains? m :selected))
      [(out/to eid (out/container-data id 0 (:selected m)))])
    (when (container/lectern? m)
      [(out/to eid (out/container-data id 0 (container/page world m)))])
    (map-indexed (fn [i v] (out/to eid (out/container-data id i v)))
                 (container/data-values world m))))

(def ^:private open-stats
  {:crafting-table :custom/interact-with-crafting-table
   :stonecutter    :custom/interact-with-stonecutter
   :loom           :custom/interact-with-loom
   :furnace        :custom/interact-with-furnace
   :blast-furnace  :custom/interact-with-blast-furnace
   :smoker         :custom/interact-with-smoker
   :brewing-stand  :custom/interact-with-brewingstand})

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
      (when-let [stat (open-stats (:type m))] [[:award eid stat 1]])
      (count-deltas world m 1)
      (barrel-deltas world m 1))
    []))

(defn- synced [menu ^long st slots carried]
  (assoc menu :state-id st
              :remote (into {} (map-indexed (fn [i s] [i (remote-of s)])) slots)
              :remote-carried (remote-of carried)))

(defn- slot-diff-deltas [eid menu slots ^long base]
  (let [remote (:remote menu)
        diff (keep-indexed (fn [i s]
                             (when-not (remote-match? (get remote i) s)
                               [i s]))
                           slots)]
    (reduce (fn [[ds ^long st] [i s]]
              [(conj ds (out/to eid (out/container-slot (:id menu) (inc st) i s))) (inc st)])
            [[] base] diff)))

(defn- sync-deltas [eid menu slots carried resync?]
  (let [base (long (:state-id menu 1))]
    (if resync?
      {:deltas [(out/to eid (out/container-content (:id menu) (inc base) slots carried))]
       :menu   (synced menu (inc base) slots carried)}
      (let [[ds st] (slot-diff-deltas eid menu slots base)]
        {:deltas (cond-> ds
                   (not (remote-match?
                          (:remote-carried menu) carried))
                   (conj (out/to eid (out/carried carried))))
         :menu   (synced menu st slots carried)}))))

(defn- with-client [menu changed carried]
  (assoc menu
    :remote (reduce (fn [r [s v]] (assoc r (long s) (remote-of v))) (:remote menu) changed)
    :remote-carried (remote-of carried)))

(defn- slot-changes [before after]
  (into {}
        (keep (fn [slot] (when (not= (get after slot) (get before slot))
                           [slot (get after slot)])))
        (into (set (keys before)) (keys after))))

(defn- stale-result
  "Returns the menu with its result slot marked unseen when the
  grid changed."
  [m items items']
  (if (and (container/crafting? m)
           (not= (container/inputs m items)
                 (container/inputs m items')))
    (assoc-in m [:remote 0] ::stale)
    m))

(defn- click-start [world e m items]
  {:inventory  (flat items (or (:inventory e) {})
                     (container/slot-count m))
   :carried    (:carried e)
   :quickcraft (:quickcraft e)
   :layout     (container/layout m (crafting/context world e))})

(defn- menu-after [m m0 items items' packet]
  (-> (cond-> m0 (container/bench? m0) (assoc :contents items'))
      (with-client (:changed packet) (:carried packet))
      (stale-result items items')))

(defn- clicked [world eid e m packet]
  (let [items (container/items world eid m)
        after (menu/click (click-start world e m items) packet)
        [items0 inv'] (split-flat (:inventory after)
                                  (container/slot-count m))
        [m0 items'] (container/settled m items0)
        m' (menu-after m m0 items items' packet)
        resync? (not= (long (:state-id packet)) (long (:state-id m 1)))]
    (assoc (sync-deltas eid m' (view m items' inv') (:carried after) resync?)
      :after after :inventory inv' :items items')))

(defn craft-deltas [world eid after]
  (concat
    (for [[item n] (:crafted after)]
      [:award eid (keyword "crafted" (name item)) n])
    (for [s (:spills after)]
      [:spawn-entity (items/dropped world eid s)])))

(defn- click-result-deltas [world eid e m {:keys [after inventory items deltas menu]}]
  (concat
    [[:merge-entity eid {:inventory  inventory :carried (:carried after)
                         :quickcraft (:quickcraft after) :menu menu}]
     [:client-slots eid (slot-changes (or (:inventory e) {}) inventory) (:carried after)]]
    (container/store-deltas world eid m items)
    deltas
    (when (not= (:selected m) (:selected menu))
      [(out/to eid (out/container-data (:id menu) 0 (:selected menu)))])
    (when (pos? (long (:takes after 0)))
      (keep identity [(container/take-sound m)]))
    (craft-deltas world eid after)
    (map-indexed (fn [i stack] [:spawn-entity (items/dropped world eid stack true i)])
                 (:drops after))))

(defn- click-deltas [world [_ eid packet]]
  (when-let [e (get-in world [:entities eid])]
    (let [m (:menu e)]
      (cond
        (or (nil? m) (not= (long (:container packet)) (long (:id m)))) nil
        (container/lectern? m) nil
        (not (valid? world m)) (close-deltas world eid e true)
        :else (click-result-deltas world eid e m (clicked world eid e m packet))))))

(defn- page-button-deltas [world eid m ^long want]
  (when-let [ds (container/page-deltas world m want)]
    (concat ds [(out/to eid (out/container-data (:id m) 0
                                                (container/next-page world m want)))])))

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
            {:keys [deltas menu]} (sync-deltas eid (assoc m' :contents items)
                                               slots (:carried e) false)]
        (concat
          [[:merge-entity eid {:menu menu}]]
          deltas
          [(out/to eid (out/container-data (:id menu) 0 (:selected menu)))])))))

(defn- button-deltas [world [_ eid container id]]
  (when-let [e (get-in world [:entities eid])]
    (let [m (:menu e)]
      (when (and m (= (long container) (long (:id m))))
        (cond
          (container/lectern? m) (when (valid? world m)
                                   (lectern-button-deltas world eid e m (long id)))
          (container/bench? m) (bench-button-deltas eid e m (long id)))))))

(def ^:private own-grid [0 1 2 3 4])

(defn- inventory-close-deltas [world eid e]
  (let [inv (or (:inventory e) {})
        stacks (cons (:carried e) (map inv (rest own-grid)))
        [inv' drops] (put-back (crafting/context world e)
                               (apply dissoc inv own-grid) stacks)]
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
            (let [eid (:eid e)]
              (left-behind-deltas (assoc-in world [:entities eid] e)
                                  eid e)))
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
        [ds menu] (data-deltas eid (:menu synced)
                               (container/data-values world m))]
    (when (or (seq deltas) (seq ds))
      (concat [[:merge-entity eid {:menu menu}]] deltas ds))))

(defn- broadcasting? [world e]
  (let [m (:menu e)]
    (and m (= :block (:kind m)) (valid? world m))))

(defn broadcast
  "Sends every viewer the slots and data of their menu that moved, as
  AbstractContainerMenu.broadcastChanges does each player tick."
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
