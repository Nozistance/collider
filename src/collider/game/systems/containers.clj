(ns collider.game.systems.containers
  (:require [collider.game.blockentity :as be]
            [collider.game.container :as container]
            [collider.game.menu :as menu]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private player-view (vec (concat (range 9 36) (range 36 45))))

(defn- view [contents inv]
  (into (vec contents) (map inv) player-view))

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
  (or (= :ender (:kind m))
      (every? (fn [pos] (contains? be/container-kinds (:kind (be/at world pos)))) (:cells m))))

(defn- back-into [inv stack]
  (when stack (items/add-stack inv stack)))

(defn- close-deltas [world eid e notify?]
  (when-let [m (:menu e)]
    (let [carried (:carried e)
          [changes left] (if carried (back-into (:inventory e) carried) [nil nil])]
      (concat
       [[:merge-entity eid {:menu nil :carried nil}]]
       (for [[slot s] changes] [:set-slot eid slot s])
       (when left [[:spawn-entity (items/dropped world eid left)]])
       (when carried [(out/to eid (out/carried nil))])
       (when notify? [(out/to eid (out/container-close (:id m)))])
       (count-deltas world m -1)
       (barrel-deltas world m -1)))))

(defn- opened [world eid e m id]
  (let [contents (container/items world eid m)
        slots (view contents (:inventory e))]
    {:menu (assoc m :id id :state-id 1
                  :remote (into {} (map-indexed (fn [i s] [i (remote-of s)])) slots)
                  :remote-carried (remote-of (:carried e)))
     :slots slots}))

(defn open-deltas [world eid pos]
  (if-let [m (container/menu-at (:chunks world) pos)]
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

(defn- click-deltas [world [_ eid packet]]
  (when-let [e (get-in world [:entities eid])]
    (let [m (:menu e)]
      (cond
        (or (nil? m) (not= (long (:container packet)) (long (:id m)))) nil
        (not (valid? world m)) (close-deltas world eid e true)
        :else
        (let [n (* 9 (long (:rows m)))
              contents (container/items world eid m)
              before {:inventory (flat contents (or (:inventory e) {}) n)
                      :carried (:carried e)
                      :quickcraft (:quickcraft e)
                      :layout (container/layout m)}
              after (menu/click before packet)
              [items' inv'] (split-flat (:inventory after) n)
              slots (view items' inv')
              resync? (not= (long (:state-id packet)) (long (:state-id m 1)))
              m' (with-client m (:changed packet) (:carried packet))
              {:keys [deltas menu]} (sync-deltas world eid m' slots (:carried after) resync?)]
          (concat
           [[:merge-entity eid {:inventory inv' :carried (:carried after)
                                :quickcraft (:quickcraft after) :menu menu}]]
           (container/store-deltas world eid m items')
           deltas
           (map-indexed (fn [i stack] [:spawn-entity (items/dropped world eid stack true i)])
                        (:drops after))))))))

(defn- close-event-deltas [world [_ eid _]]
  (when-let [e (get-in world [:entities eid])]
    (close-deltas world eid e false)))

(defn- containers-deltas [world events]
  (mapcat (fn [[tag :as ev]]
            (case tag
              :menu-click (click-deltas world ev)
              :menu-close (close-event-deltas world ev)
              nil))
          events))

(defn containers [world events]
  [#(containers-deltas world events)])
