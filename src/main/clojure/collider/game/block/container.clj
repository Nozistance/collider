(ns collider.game.block.container
  "What container and bench menus show, hold and store."
  (:require [collider.game.block.anvil :as anvil]
            [collider.game.block.blockentity :as be]
            [collider.game.block.brewing :as brewing]
            [collider.game.block.crafting :as crafting]
            [collider.game.block.enchanting :as enchanting]
            [collider.game.block.furnace :as furnace]
            [collider.game.block.grindstone :as grindstone]
            [collider.game.block.lectern :as lectern]
            [collider.game.block.lid :as lid]
            [collider.game.block.loom :as loom]
            [collider.game.block.menu :as menu]
            [collider.game.block.smithing :as smithing]
            [collider.game.block.stonecutter :as stonecutter]
            [collider.game.bundle :as bundle]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.chest :as chest]
            [collider.world.blocks.lectern :as block-lectern]))

(set! *warn-on-reflection* true)

(def ^:private size 27)

(def ^:private bench-types
  #{:stonecutter :loom :crafting-table :anvil :grindstone
    :smithing-table :enchantment-table})

(def container-types
  (into (conj chest/types :barrel :ender-chest :shulker-box)
        bench-types))

(def menu-types
  (conj (into container-types be/furnace-kinds) :brewing-stand))

(defn placed-state
  "Returns the state a chest or barrel st takes as it is placed at
  pos."
  [chunks pos st face sneaking? yaw pitch]
  (let [t (block/type-of st)]
    (cond
      (contains? chest/types t)
      (chest/placed chunks pos st face sneaking?)
      (= :barrel t) (chest/barrel-placed st yaw pitch)
      :else st)))

(defn- blocked? [chunks pos]
  (block/conductor? (chest/state-at chunks (mapv + pos [0 1 0]))))

(defn- double-chest-menu [pos p2 ^long st]
  {:kind  :block :rows 6 :type :generic-9x6
   :title {:translate "container.chestDouble"}
   :cells (if (= :right (:type (block/props-of st)))
            [pos p2]
            [p2 pos])})

(defn- single-chest-menu [pos]
  {:kind  :block :rows 3 :type :generic-9x3
   :title {:translate "container.chest"}
   :cells [pos]})

(defn- chest-menu [chunks pos ^long st]
  (when-not (blocked? chunks pos)
    (if-let [p2 (chest/partner chunks pos st)]
      (when-not (blocked? chunks p2)
        (double-chest-menu pos p2 st))
      (single-chest-menu pos))))

(defn- barrel-menu [_ _ pos _]
  {:kind :block :rows 3 :type :generic-9x3
   :title {:translate "container.barrel"} :cells [pos]})

(defn- shulker-menu [_ _ pos _]
  {:kind :block :rows 3 :type :shulker-box
   :title {:translate "container.shulkerBox"} :cells [pos]})

(defn- lidded-shulker-menu [world chunks pos st]
  (when (lid/can-open? world pos st)
    (shulker-menu world chunks pos st)))

(defn- ender-menu [_ chunks pos _]
  (when-not (blocked? chunks pos)
    {:kind :ender :rows 3 :type :generic-9x3
     :title {:translate "container.enderchest"}
     :cells [] :pos pos}))

(defn- stonecutter-menu [_ _ pos _]
  {:kind :bench :type :stonecutter :size 2 :result 1
   :title {:translate "container.stonecutter"}
   :cells [] :pos pos :selected 0 :contents [nil nil]})

(defn- crafting-menu [_ _ pos _]
  {:kind :bench :type :crafting-table :screen :crafting
   :size 10 :result 0 :cells [] :pos pos
   :title {:translate "container.crafting"}
   :contents (vec (repeat 10 nil))})

(defn- lectern-menu [_ _ pos st]
  (when (block-lectern/has-book? st)
    {:kind :lectern :type :lectern
     :title {:translate "container.lectern"}
     :cells [] :pos pos}))

(defn- brewing-menu [_ _ pos _]
  {:kind  :block :type :brewing-stand :slots 5
   :title {:translate "container.brewing"}
   :cells [pos]})

(defn- loom-menu [_ _ pos _]
  {:kind :bench :type :loom :size 4 :result 3
   :title {:translate "container.loom"} :cells [] :pos pos
   :selected 0 :patterns [] :contents [nil nil nil nil]})

(defn- anvil-menu [_ _ pos _]
  {:kind :bench :type :anvil :size 3 :result 2
   :title {:translate "container.repair"} :cells [] :pos pos
   :cost 0 :name nil :repair-count 0 :renaming? false
   :contents [nil nil nil]})

(defn- grindstone-menu [_ _ pos _]
  {:kind :bench :type :grindstone :size 3 :result 2
   :title {:translate "container.grindstone_title"}
   :cells [] :pos pos :contents [nil nil nil]})

(defn- smithing-menu [_ _ pos _]
  {:kind :bench :type :smithing-table :screen :smithing
   :size 4 :result 3 :cells [] :pos pos
   :title {:translate "container.upgrade"}
   :recipe-error? false :contents [nil nil nil nil]})

(defn- enchanting-menu [_ _ pos _]
  (merge {:kind :bench :type :enchantment-table :screen :enchantment
          :size 2 :cells [] :pos pos :seed 0
          :title {:translate "container.enchant"}
          :contents [nil nil]}
         enchanting/blank))

(def ^:private menu-builders
  {:barrel         barrel-menu
   :shulker-box    lidded-shulker-menu
   :ender-chest    ender-menu
   :stonecutter    stonecutter-menu
   :crafting-table crafting-menu
   :lectern        lectern-menu
   :brewing-stand  brewing-menu
   :loom           loom-menu
   :anvil          anvil-menu
   :grindstone     grindstone-menu
   :smithing-table smithing-menu
   :enchantment-table enchanting-menu})

(def ^:private furnace-titles
  {:furnace       "container.furnace"
   :blast-furnace "container.blast_furnace"
   :smoker        "container.smoker"})

(defn- furnace-menu [t pos]
  {:kind  :block :type t :slots 3 :cells [pos]
   :title {:translate (furnace-titles t)}})

(def ^:private builders
  (into menu-builders
        (map (fn [t] [t (fn [_ _ pos _] (furnace-menu t pos))]))
        be/furnace-kinds))

(defn menu-at
  [world pos]
  (let [chunks (:chunks world)
        st (chest/state-at chunks pos) t (block/type-of st)]
    (if (contains? chest/types t)
      (chest-menu chunks pos st)
      (when-let [f (builders t)] (f world chunks pos st)))))

(defn provider-at
  "Returns the menu a spectator opens at pos.
  An ender chest has none. A shulker box opens whatever blocks
  its lid."
  [world pos]
  (let [st (chest/state-at (:chunks world) pos)]
    (case (block/type-of st)
      :ender-chest nil
      :shulker-box (shulker-menu world nil pos st)
      (menu-at world pos))))

(defn bench? [m] (= :bench (:kind m)))

(defn lectern? [m] (= :lectern (:kind m)))

(defn crafting? [m] (= :crafting-table (:type m)))

(defn furnace? [m] (contains? be/furnace-kinds (:type m)))

(defn brewing? [m] (= :brewing-stand (:type m)))

(defn- menu-entity [world m] (be/at world (first (:cells m))))

(defn enchanting? [m] (= :enchantment-table (:type m)))

(defn for-player
  [m e]
  (cond-> m
    (enchanting? m) (assoc :seed (long (:enchantment-seed e 0)))))

(defn- bench-values [m]
  (case (:type m)
    :anvil [(long (:cost m 0))]
    :smithing-table [(if (:recipe-error? m) 1 0)]
    :enchantment-table
    (-> (:costs m) (conj (:seed m))
        (into (:clues m)) (into (:levels m)))
    nil))

(defn data-values
  "Returns the synced numbers of a bench, furnace or brewing stand."
  [world m]
  (cond
    (bench? m) (bench-values m)
    (furnace? m) (let [e (menu-entity world m)]
                   [(:lit-remaining e 0) (:lit-total e 0)
                    (:cook e 0) (:cook-total e 0)])
    (brewing? m) (let [e (menu-entity world m)]
                   [(:brew e 0) (:fuel e 0)])))

(defn player-slots?
  "Returns true when menu m shows the player inventory."
  [m]
  (not (lectern? m)))

(defn slot-count
  "Returns the number of own slots of menu m."
  ^long [m]
  (cond
    (lectern? m) 1
    (bench? m) (long (:size m))
    (:slots m) (long (:slots m))
    :else (* 9 (long (:rows m)))))

(defn- padded
  ([items] (padded items size))
  ([items ^long n] (vec (take n (concat items (repeat nil))))))

(defn- cell-size ^long [m] (long (:slots m size)))

(defn- cell-items
  ([world pos] (cell-items world pos size))
  ([world pos ^long n] (padded (:items (be/at world pos)) n)))

(defn- bench-items [m]
  (vec (take (slot-count m) (concat (:contents m) (repeat nil)))))

(defn- ender-items [world eid]
  (padded (get-in world [:entities eid :ender-items])))

(defn items
  "Returns the stacks in the own slots of menu m of player eid."
  [world eid m]
  (cond
    (lectern? m) [(lectern/book-of world m)]
    (bench? m) (bench-items m)
    (= :ender (:kind m)) (ender-items world eid)
    :else (into [] (mapcat #(cell-items world % (cell-size m)))
                (:cells m))))

(defn inputs
  "Returns the stacks of items outside the result slot of menu m."
  [m items]
  (keep-indexed (fn [i s] (when (not= i (:result m)) s)) items))

(defn- count-of ^long [s] (if s (long (:count s 1)) 0))

(defn- taken [old items]
  (let [before (nth (:items old) 2)
        n (- (count-of before) (count-of (nth items 2)))]
    (when (pos? n) [(:item before) n])))

(defn- furnace-paid [world eid pos old]
  (let [t (:tick world)
        at (get-in world [:entities eid :pos])
        roll #(random/of-key t pos :furnace-xp %)]
    (furnace/award-deltas old at roll [:furnace eid])))

(defn- furnace-store [world eid m items]
  (let [pos (first (:cells m))
        old (be/at world pos)
        got (taken old items)
        e (-> (furnace/input-changed old (nth items 0))
              (assoc-in [:items 1] (nth items 1))
              (assoc-in [:items 2] (nth items 2))
              (cond-> got (assoc :used {})))]
    (concat (when (not= old e) [[:set-block-entity pos e]])
            (when got (furnace-paid world eid pos old)))))

(defn- cell-store [world items n i pos]
  (let [old (be/at world pos)
        n (long n) i (long i)
        part (padded (subvec (vec items) (* i n) (* (inc i) n)) n)]
    (when (not= (padded (:items old) n) part)
      [:set-block-entity pos (assoc old :items part)])))

(defn store-deltas
  "Returns the deltas that store items back into the blocks of menu
  m of player eid."
  [world eid m items]
  (cond
    (lectern? m) nil
    (bench? m) nil
    (= :ender (:kind m))
    [[:merge-entity eid {:ender-items (vec items)}]]
    (furnace? m) (furnace-store world eid m items)
    :else (keep-indexed #(cell-store world items (cell-size m) %1 %2)
                        (:cells m))))

(defn- may-place? [_ stack]
  (bundle/fits-inside? (:item stack)))

(defn- typed-layout [m ctx]
  (case (:type m)
    :stonecutter (stonecutter/layout m)
    :loom (loom/layout m)
    :anvil (anvil/layout m ctx)
    :grindstone (grindstone/layout)
    :smithing-table (smithing/layout)
    :enchantment-table (enchanting/layout)
    :crafting-table (crafting/table-layout ctx)
    :shulker-box
    (menu/container-layout (long (:rows m)) may-place?)
    (menu/container-layout (long (:rows m)))))

(def ^:private plain-ctx {:held 0})

(defn layout
  ([m] (layout m plain-ctx))
  ([m ctx]
   (cond
     (lectern? m) (lectern/layout)
     (furnace? m) (furnace/layout m)
     (brewing? m) (brewing/layout)
     :else (typed-layout m ctx))))

(defn derived
  "Returns items with the result slot of bench menu m filled in."
  ([m items] (derived m items plain-ctx))
  ([m items ctx]
   (if-not (bench? m)
     items
     (let [sparse (keep-indexed (fn [i s] (when s [i s])) items)
           inv ((:derive (layout m ctx)) (into {} sparse))]
       (mapv #(get inv %) (range (slot-count m)))))))

(defn- slots-changed
  ([m items] (slots-changed m items plain-ctx))
  ([m items ctx]
   (case (:type m)
     :enchantment-table (enchanting/changed m items ctx)
     :stonecutter (stonecutter/changed m items)
     :loom (loom/changed m items)
     :anvil (anvil/changed m items ctx)
     :smithing-table (smithing/changed m items)
     m)))

(defn settled
  "Returns bench menu m and its items after its slots change."
  ([m items] (settled m items plain-ctx))
  ([m items ctx]
   (if (or (not (bench? m)) (crafting? m))
     [m items]
     (let [m' (slots-changed m items ctx)]
       [m' (derived m' items ctx)]))))

(defn- option-count ^long [m]
  (case (:type m)
    :stonecutter (count (stonecutter/cuts (first (:contents m))))
    :loom (count (:patterns m))
    0))

(defn button
  "Returns menu m with option id selected when it offers one."
  [m ^long id]
  (let [n (option-count m)]
    (if (and (not= id (long (:selected m))) (< -1 id n))
      (assoc m :selected id)
      m)))

(def ^:private take-sounds
  {:stonecutter :ui.stonecutter.take-result
   :loom        :ui.loom.take-result})

(def ^:private take-events
  {:grindstone     :sound-grindstone-used
   :smithing-table :sound-smithing-table-used})

(defn take-sound
  "Returns the effect of a result taken from bench menu m, or nil."
  [m]
  (if-let [event (take-events (:type m))]
    (out/all (out/level-event event (:pos m) 0))
    (when-let [kind (take-sounds (:type m))]
      (out/all (out/block-sound kind (:pos m) 1.0 1.0)))))
