(ns collider.game.block.screen
  "The open menu of a player, its sync with the client and its
  close."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.container :as container]
            [collider.game.block.crafting :as crafting]
            [collider.game.item :as item]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.reach :as reach]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private player-view (vec (concat (range 9 36) (range 36 45))))

(defn view
  "Returns the slots menu m shows: its contents, then the slots
  of inventory inv when m shows them."
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
  (let [st (container/state-at (:chunks world) (:pos m))]
    (= (:type m) (block/type-of st))))

(defn- ender-valid? [world m]
  (let [st (container/state-at (:chunks world) (:pos m))]
    (= :ender-chest (block/type-of st))))

(defn valid?
  "Returns true when the blocks of menu m still hold it."
  [world m]
  (cond
    (container/lectern? m)
    (and (= :lectern (:kind (be/at world (:pos m))))
         (container/book? (container/book-of world m)))
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
    (mapcat #(container/opener-deltas world % step)
            (container/positions m))))

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
  menu goes: they drop, and the openers of the menu lose it."
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
  "Returns true when player e is near enough to keep menu m open:
  within its reach and 4 blocks of each of its blocks, as
  Container.stillValidBlockEntity:95."
  [e m]
  (every? #(reach/in-edit-range? e %)
          (or (seq (:cells m)) [(:pos m)])))

(defn menu-deltas
  "Returns the deltas of the menu of player eid in its turn
  (ServerPlayer.tick:619-623): the slots and data that moved go to
  it, then a menu no longer valid closes."
  [world eid e]
  (when-let [m (:menu e)]
    (let [ok? (valid? world m)]
      (concat
        (when (and ok? (= :block (:kind m)))
          (broadcast-deltas world (long eid) e))
        (when-not (and ok? (near? e m))
          (close-deltas world eid e true))))))
