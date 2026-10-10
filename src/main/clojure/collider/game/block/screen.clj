(ns collider.game.block.screen
  "The open menu of a player, its sync with the client and its
  close."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.container :as container]
            [collider.game.block.crafting :as crafting]
            [collider.game.block.lectern :as lectern]
            [collider.game.block.lid :as lid]
            [collider.game.item :as item]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.reach :as reach]
            [collider.world.block :as block]
            [collider.world.blocks.chest :as chest]))

(set! *warn-on-reflection* true)

(def ^:private player-view (vec (concat (range 9 36) (range 36 45))))

(defn view
  "Returns the slots menu m shows. The slots of inventory inv follow
  its own when m shows them."
  [m contents inv]
  (if (container/player-slots? m)
    (into (vec contents) (map inv) player-view)
    (vec contents)))

(defn remote-of
  "Returns stack as the client of the menu knows it."
  [stack]
  (when stack
    (cond-> {:item (:item stack) :count (long (:count stack 1))}
      (:components stack) (assoc :components (:components stack))
      (:components? stack) (assoc :components? true))))

(defn- hashed [r]
  (when r
    (assoc (dissoc r :components)
      :components? (boolean (or (:components r) (:components? r))))))

(defn- remote-match? [remote stack]
  (let [r (remote-of stack)]
    (if (:components? remote) (= remote (hashed r)) (= remote r))))

(defn- bench-valid? [world m]
  (let [st (chest/state-at (:chunks world) (:pos m))]
    (= (:type m) (block/type-of st))))

(defn- ender-valid? [world m]
  (let [st (chest/state-at (:chunks world) (:pos m))]
    (= :ender-chest (block/type-of st))))

(defn valid?
  "Returns true when the blocks of menu m still hold it."
  [world m]
  (cond
    (container/lectern? m)
    (and (= :lectern (:kind (be/at world (:pos m))))
         (lectern/book? (lectern/book-of world m)))
    (container/bench? m) (bench-valid? world m)
    (= :ender (:kind m)) (ender-valid? world m)
    :else
    (every? (fn [pos]
              (contains? be/menu-kinds (:kind (be/at world pos))))
            (:cells m))))

(defn put-back
  "Returns [inv drops] after the stacks go back into inventory inv.
  The stacks that find no room are the drops."
  [ctx inv stacks]
  (reduce (fn [[inv drops] s]
            (let [[inv' left] (crafting/place-back ctx inv s)]
              [inv' (cond-> drops left (conj left))]))
          [inv []]
          (remove nil? stacks)))

(defn changed-slots
  "Returns the slots that differ from before to after, in order."
  [before after]
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

(defn opener-deltas
  "Returns the deltas that count player e in or out, by step, of
  the openers of the blocks of menu m."
  [world e m step]
  (when-not (game-mode/spectator? e)
    (mapcat #(lid/opener-deltas world % step)
            (lid/positions m))))

(defn close-deltas
  "Returns the deltas that close the menu of player eid.
  The client hears of the close when notify? is true."
  [world eid e notify?]
  (when-let [m (:menu e)]
    (let [[changes drops] (closed-inventory world eid e m)]
      (concat
        [[:merge-entity eid {:menu nil :carried nil}]]
        (for [[slot s] changes] [:set-slot eid slot s])
        (for [s drops] [:spawn-entity (item/dropped world eid s)])
        (close-out-deltas eid m (:carried e) notify?)
        (opener-deltas world e m -1)))))

(defn remote-slots
  "Returns the slots as the client of the menu knows them."
  [slots]
  (into {} (map-indexed (fn [i s] [i (remote-of s)])) slots))

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

(defn resync-deltas
  "Returns the deltas and the menu that send player eid all the
  slots of menu at state id st."
  [eid menu slots carried st]
  (let [p (out/container-content (:id menu) st slots carried)]
    {:deltas [(out/to eid p)]
     :menu   (synced menu st slots carried)}))

(defn sync-deltas
  "Returns the deltas and the menu that send player eid the slots
  of menu it does not know. With resync? it gets them all."
  [eid menu slots carried resync?]
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

(def own-grid
  "The slots of the crafting grid of the inventory, result first."
  [0 1 2 3 4])

(defn- held-stacks [world eid e]
  (let [grid (map (or (:inventory e) {}) (rest own-grid))
        m (:menu e)]
    (remove nil?
            (if m
              (concat grid [(:carried e)] (bench-inputs world eid m))
              (cons (:carried e) grid)))))

(defn left-behind-deltas
  "Returns the deltas of the stacks player e leaves behind as its
  menu goes. The stacks drop, and the openers of the menu lose e."
  [world eid e]
  (let [m (:menu e)]
    (concat
      (for [s (held-stacks world eid e)]
        [:spawn-entity (item/dropped world eid s)])
      (when m (opener-deltas world e m -1)))))

(defn removed-deltas
  "Returns the deltas that close the menu of player e as it leaves."
  [world eid e]
  (concat
    (when (or (:menu e) (:carried e))
      [[:merge-entity eid {:menu nil :carried nil}]])
    (for [slot own-grid :when (get-in e [:inventory slot])]
      [:set-slot eid slot nil])
    (left-behind-deltas world eid e)))

(defn- data-deltas [eid m values]
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

(defn- near?
  "Returns true when player e is near enough to keep menu m open. It
  must be within its reach and 4 blocks of each block of m."
  [e m]
  (every? #(reach/in-edit-range? e %)
          (or (seq (:cells m)) [(:pos m)])))

(defn menu-deltas
  "Returns the deltas of the menu of player eid in its turn. The
  slots and data that moved go to the player, and a menu no longer
  valid closes."
  [world eid e]
  (when-let [m (:menu e)]
    (let [ok? (valid? world m)]
      (concat
        (when (and ok? (= :block (:kind m)))
          (broadcast-deltas world (long eid) e))
        (when-not (and ok? (near? e m))
          (close-deltas world eid e true))))))

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
        [(one 0 (lectern/page world m))])
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
                     (chest/state-at (:chunks world) pos)))))

(defn- open-menu-deltas [world eid e m]
  (let [m (container/for-player m e)
        prev (close-deltas world eid e true)
        e' (cond-> e prev (assoc :carried nil :menu nil))
        id (inc (mod (long (:container-counter e 0)) 100))
        {:keys [menu slots]} (opened world eid e' m id)]
    (concat
      prev
      [[:merge-entity eid {:menu menu :container-counter id}]]
      (open-screen-deltas world eid m id slots (:carried e')))))

(defn open-deltas
  [world eid pos]
  (if-let [m (container/menu-at world pos)]
    (let [e (get-in world [:entities eid])
          stat (open-stat world pos m)]
      (concat
        (open-menu-deltas world eid e m)
        (when stat [[:award eid stat 1]])
        (opener-deltas world e m 1)))
    []))

(defn spectator-open-deltas
  "Returns the deltas of a spectator using a block with a menu.
  The menu opens with no stat and no opener. Returns nil when the
  block has none."
  [world eid pos]
  (when-let [m (container/provider-at world pos)]
    (open-menu-deltas world eid (get-in world [:entities eid]) m)))
