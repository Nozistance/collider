(ns collider.game.systems.containers
  (:require [collider.game.blockentity :as be]
            [collider.game.container :as container]
            [collider.game.menu :as menu]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private player-view (vec (concat (range 9 36) (range 36 45))))

(defn- view
  "The menu slots as the client sees them. LecternMenu has no player slots."
  [m contents inv]
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
    {:item (:item stack) :count (long (:count stack 1))
     :components? (boolean (or (:components stack) (:components? stack)))}))

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

(defn- valid? [world m]
  (cond
    (container/lectern? m)
    ;; the bookAccess Container: the block entity is there and still has a book
    (and (= :lectern (:kind (be/at world (:pos m))))
         (container/book? (container/book-of world m)))
    (container/bench? m)
    (= (:type m) (block/type-of (container/state-at (:chunks world) (:pos m))))
    (= :ender (:kind m)) true
    :else
    (every? (fn [pos] (contains? be/container-kinds (:kind (be/at world pos)))) (:cells m))))

(defn- back-into [inv stack]
  (when stack (items/add-stack inv stack)))

(defn- give-back
  "Inventory.placeItemBackInInventory for a run of stacks, one after another."
  [inv stacks]
  (reduce (fn [[inv changes drops] stack]
            (let [[chg left] (items/add-stack inv stack)]
              [(reduce (fn [i [slot v]] (assoc i slot v)) inv chg)
               (into changes chg)
               (cond-> drops left (conj left))]))
          [inv [] []] (remove nil? stacks)))

(defn- close-deltas [world eid e notify?]
  (when-let [m (:menu e)]
    (let [carried (:carried e)
          ;; ContainerLevelAccess.create(...).execute always runs: the inputs
          ;; come back even if the bench itself is gone.
          back (when (container/bench? m)
                 (container/inputs m (container/items world eid m)))
          [inv0 kept drops] (give-back (or (:inventory e) {}) back)
          [changes left] (if carried (back-into inv0 carried) [nil nil])
          changes (concat kept changes)]
      (concat
       [[:merge-entity eid {:menu nil :carried nil}]]
       (for [[slot s] changes] [:set-slot eid slot s])
       (for [s drops] [:spawn-entity (items/dropped world eid s)])
       (when left [[:spawn-entity (items/dropped world eid left)]])
       (when carried [(out/to eid (out/carried nil))])
       (when notify? [(out/to eid (out/container-close (:id m)))])
       (count-deltas world m -1)
       (barrel-deltas world m -1)))))

(defn- opened [world eid e m id]
  (let [contents (container/items world eid m)
        slots (view m contents (:inventory e))]
    {:menu (assoc m :id id :state-id 1
                  :remote (into {} (map-indexed (fn [i s] [i (remote-of s)])) slots)
                  :remote-carried (remote-of (:carried e)))
     :slots slots}))

(defn open-deltas [world eid pos]
  (if-let [m (container/menu-at world pos)]
    (let [e (get-in world [:entities eid])
          prev (close-deltas world eid e true)
          e' (cond-> e prev (assoc :carried nil :menu nil))
          id (inc (mod (long (:container-counter e 0)) 100))
          {:keys [menu slots]} (opened world eid e' m id)]
      (concat
       prev
       [[:merge-entity eid {:menu menu :container-counter id}]
        (out/to eid (out/open-screen id (:type m) (:title m)))
        (out/to eid (out/container-content id 1 slots (:carried e')))]
       ;; sendInitialData: every data slot goes out after the contents
       (when (container/bench? m)
         [(out/to eid (out/container-data id 0 (:selected m)))])
       (when (container/lectern? m)
         [(out/to eid (out/container-data id 0 (container/page world m)))])
       (count-deltas world m 1)
       (barrel-deltas world m 1)))
    []))

(defn- sync-deltas [world eid menu slots carried resync?]
  (let [remote (:remote menu)
        base (long (:state-id menu 1))]
    (if resync?
      {:deltas [(out/to eid (out/container-content (:id menu) (inc base) slots carried))]
       :menu (assoc menu :state-id (inc base)
                    :remote (into {} (map-indexed (fn [i s] [i (remote-of s)])) slots)
                    :remote-carried (remote-of carried))}
      (let [diff (keep-indexed (fn [i s] (when (not= (get remote i) (remote-of s)) [i s])) slots)
            [ds st] (reduce (fn [[ds ^long st] [i s]]
                              [(conj ds (out/to eid (out/container-slot (:id menu) (inc st) i s)))
                               (inc st)])
                            [[] base] diff)]
        {:deltas (cond-> ds
                   (not= (:remote-carried menu) (remote-of carried))
                   (conj (out/to eid (out/carried carried))))
         :menu (assoc menu :state-id st
                      :remote (into {} (map-indexed (fn [i s] [i (remote-of s)])) slots)
                      :remote-carried (remote-of carried))}))))

(defn- with-client [menu changed carried]
  (assoc menu
         :remote (reduce (fn [r [s v]] (assoc r (long s) (remote-of v))) (:remote menu) changed)
         :remote-carried (remote-of carried)))

(defn- slot-changes
  "What the click did to the player's own slots. While a menu is open its own
   sync tells the client about them, so the player tracker must not say it
   again: in vanilla the inventory menu is not the open one and is silent."
  [before after]
  (into {}
        (keep (fn [slot] (when (not= (get after slot) (get before slot))
                           [slot (get after slot)])))
        (into (set (keys before)) (keys after))))

(defn- click-deltas [world [_ eid packet]]
  (when-let [e (get-in world [:entities eid])]
    (let [m (:menu e)]
      (cond
        (or (nil? m) (not= (long (:container packet)) (long (:id m)))) nil
        ;; LecternScreen is a book view with no slots: it never clicks.
        (container/lectern? m) nil
        (not (valid? world m)) (close-deltas world eid e true)
        :else
        (let [n (container/slot-count m)
              contents (container/items world eid m)
              before {:inventory (flat contents (or (:inventory e) {}) n)
                      :carried (:carried e)
                      :quickcraft (:quickcraft e)
                      :layout (container/layout m)}
              after (menu/click before packet)
              [items0 inv'] (split-flat (:inventory after) n)
              [m0 items'] (container/settled m items0)
              slots (view m items' inv')
              resync? (not= (long (:state-id packet)) (long (:state-id m 1)))
              m' (with-client (cond-> m0 (container/bench? m0) (assoc :contents items'))
                              (:changed packet) (:carried packet))
              {:keys [deltas menu]} (sync-deltas world eid m' slots (:carried after) resync?)]
          (concat
           [[:merge-entity eid {:inventory inv' :carried (:carried after)
                                :quickcraft (:quickcraft after) :menu menu}]
            [:client-slots eid (slot-changes (or (:inventory e) {}) inv') (:carried after)]]
           (container/store-deltas world eid m items')
           deltas
           (when (not= (:selected m) (:selected menu))
             [(out/to eid (out/container-data (:id menu) 0 (:selected menu)))])
           (when (pos? (long (:takes after 0))) [(container/take-sound m)])
           (map-indexed (fn [i stack] [:spawn-entity (items/dropped world eid stack true i)])
                        (:drops after))))))))

(defn- page-button-deltas
  "LecternMenu.setData: the clamped page goes back to the client right away,
   the way broadcastChanges does."
  [world eid m ^long want]
  (when-let [ds (container/page-deltas world m want)]
    (concat ds [(out/to eid (out/container-data (:id m) 0
                                                (container/next-page world m want)))])))

(defn- take-book-deltas
  "LecternMenu button 3: the book goes to the player, the lectern loses it and
   the menu stops being valid."
  [world eid e m]
  (when-let [book (container/book-of world m)]
    (let [[changes left] (items/add-stack (or (:inventory e) {}) book)
          state-id (inc (long (:state-id m 1)))]
      (concat
       (container/remove-book-deltas world (:pos m))
       ;; broadcastChanges before ServerPlayer.tick finds the menu invalid
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
      ;; Player.mayBuild is always true for the creative player we serve
      (= 3 id) (take-book-deltas world eid e m)
      :else nil)))

(defn- bench-button-deltas [world eid e m id]
  (let [m' (container/button m (long id))]
    (when (not= (:selected m') (:selected m))
      (let [items (container/derived m' (:contents m'))
            slots (view m' items (:inventory e))
            {:keys [deltas menu]} (sync-deltas world eid (assoc m' :contents items)
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
          (container/bench? m) (bench-button-deltas world eid e m (long id)))))))

(defn- close-event-deltas [world [_ eid _]]
  (when-let [e (get-in world [:entities eid])]
    (close-deltas world eid e false)))

(defn- containers-deltas [world events]
  (concat
   (container/animate-deltas world)
   (mapcat (fn [[tag :as ev]]
             (case tag
               :menu-click (click-deltas world ev)
               :menu-close (close-event-deltas world ev)
               :menu-button (button-deltas world ev)
               nil))
           events)
   (container/recheck-deltas world)))

(defn containers [world events]
  [#(containers-deltas world events)])
