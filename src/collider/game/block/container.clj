(ns collider.game.block.container
  "Containers and benches, their opening, contents, lids and viewers."
  (:require [collider.data :as data]
            [collider.game.game-mode :as game-mode]
            [collider.game.block.blockentity :as be]
            [collider.game.entity :as entity]
            [collider.game.block.menu :as menu]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.game.block.crafting :as crafting]
            [collider.game.block.brewing :as brewing]
            [collider.game.block.furnace :as furnace]
            [collider.game.block.workbench :as workbench]
            [collider.game.block.anvil :as anvil]
            [collider.game.block.grindstone :as grindstone]
            [collider.game.block.smithing :as smithing]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.blocks.chest :as chest]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.lectern :as lectern])
  (:import (java.util List)))

(set! *warn-on-reflection* true)

(def size 27)

(def chest-types chest/types)

(def bench-types
  #{:stonecutter :loom :crafting-table :anvil :grindstone
    :smithing-table})

(def container-types
  (into (conj chest/types :barrel :ender-chest :shulker-box)
        bench-types))

(def menu-types
  "Blocks whose use opens a menu, containers and furnaces alike."
  (conj (into container-types be/furnace-kinds) :brewing-stand))

(def state-at chest/state-at)

(def connected-direction chest/connected-direction)

(def copper-types chest/copper-types)

(defn placed-state [chunks pos st face sneaking? yaw pitch]
  (let [t (block/type-of st)]
    (cond
      (contains? chest/types t)
      (chest/placed chunks pos st face sneaking?)
      (= :barrel t) (chest/barrel-placed st yaw pitch)
      :else st)))

(defn blocked? [chunks pos]
  (block/full-cube? (chest/state-at chunks (mapv + pos [0 1 0]))))

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

(def ^:private axis-index {:x 0 :y 1 :z 2})

(def ^:private positive? #{:up :south :east})

(defn- half-free?
  "Returns true when nothing blocks the lid on that side."
  [chunks pos facing]
  (let [st (state-at chunks (mapv + pos (dir/offset facing)))
        ax (long (axis-index (dir/axis facing)))
        far? (contains? positive? facing)]
    (not-any? (fn [box]
                (let [lo (double (nth box ax))
                      hi (double (nth box (+ ax 3)))]
                  (if far? (< lo 8.0) (> hi 8.0))))
              (block/collision-boxes st))))

(defn animation [world pos]
  (get (:shulker-anim world) pos))

(defn can-open? [world pos ^long st]
  (or (some? (animation world pos))
      (half-free? (:chunks world) pos (:facing (block/props-of st)))))

(defn- barrel-menu [_ _ pos _]
  {:kind :block :rows 3 :type :generic-9x3
   :title {:translate "container.barrel"} :cells [pos]})

(defn- shulker-menu [_ _ pos _]
  {:kind :block :rows 3 :type :shulker-box
   :title {:translate "container.shulkerBox"} :cells [pos]})

(defn- lidded-shulker-menu [world chunks pos st]
  (when (can-open? world pos st)
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
  (when (lectern/has-book? st)
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
   :smithing-table smithing-menu})

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

(defn menu-at [world pos]
  (let [chunks (:chunks world)
        st (state-at chunks pos) t (block/type-of st)]
    (if (contains? chest-types t)
      (chest-menu chunks pos st)
      (when-let [f (builders t)] (f world chunks pos st)))))

(defn provider-at
  "BlockState.getMenuProvider: the menu a spectator opens at pos.
  An ender chest has none; a shulker box opens whatever blocks
  its lid."
  [world pos]
  (let [st (state-at (:chunks world) pos)]
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

(defn- bench-values [m]
  (case (:type m)
    :anvil [(long (:cost m 0))]
    :smithing-table [(if (:recipe-error? m) 1 0)]
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

(defn player-slots? [m]
  (not (lectern? m)))

(defn slot-count ^long [m]
  (cond
    (lectern? m) 1
    (bench? m) (long (:size m))
    (:slots m) (long (:slots m))
    :else (* 9 (long (:rows m)))))

(def ^:private ^:table lectern-books
  (delay (set (data/tag-values "item" "lectern_books"))))

(defn book-items [] @lectern-books)

(defn book? [stack]
  (contains? (book-items) (:item stack)))

(defn page-count ^long [stack]
  (let [c (:components stack)
        written (:written-book-content c)
        writable (:writable-book-content c)]
    (cond
      written (count (:pages written))
      writable (count writable)
      :else 0)))

(defn clamp-page ^long [^long page ^long pages]
  (if (< page 0) 0 (min page (dec pages))))

(defn- padded
  ([items] (padded items size))
  ([items ^long n] (vec (take n (concat items (repeat nil))))))

(defn- cell-size ^long [m] (long (:slots m size)))

(defn cell-items
  ([world pos] (cell-items world pos size))
  ([world pos ^long n] (padded (:items (be/at world pos)) n)))

(defn book-of [world m] (:book (be/at world (:pos m))))

(defn page ^long [world m] (long (:page (be/at world (:pos m)) 0)))

(defn- bench-items [m]
  (vec (take (slot-count m) (concat (:contents m) (repeat nil)))))

(defn- ender-items [world eid]
  (padded (get-in world [:entities eid :ender-items])))

(defn items [world eid m]
  (cond
    (lectern? m) [(book-of world m)]
    (bench? m) (bench-items m)
    (= :ender (:kind m)) (ender-items world eid)
    :else (into [] (mapcat #(cell-items world % (cell-size m)))
                (:cells m))))

(defn inputs [m items]
  (keep-indexed (fn [i s] (when (not= i (:result m)) s)) items))

(defn- count-of ^long [s] (if s (long (:count s 1)) 0))

(defn- taken
  "Returns [item n] of what a click removed from the result slot."
  [old items]
  (let [before (nth (:items old) 2)
        n (- (count-of before) (count-of (nth items 2)))]
    (when (pos? n) [(:item before) n])))

(defn- furnace-store [world m items]
  (let [pos (first (:cells m))
        old (be/at world pos)
        got (taken old items)
        e (-> (furnace/input-changed old (nth items 0))
              (assoc-in [:items 1] (nth items 1))
              (assoc-in [:items 2] (nth items 2))
              (cond-> got (assoc :used {})))]
    (when (not= old e) [[:set-block-entity pos e]])))

(defn- cell-store [world items n i pos]
  (let [old (be/at world pos)
        n (long n) i (long i)
        part (padded (subvec (vec items) (* i n) (* (inc i) n)) n)]
    (when (not= (padded (:items old) n) part)
      [:set-block-entity pos (assoc old :items part)])))

(defn store-deltas [world eid m items]
  (cond
    (lectern? m) nil
    (bench? m) nil
    (= :ender (:kind m))
    [[:merge-entity eid {:ender-items (vec items)}]]
    (furnace? m) (furnace-store world m items)
    :else (keep-indexed #(cell-store world items (cell-size m) %1 %2)
                        (:cells m))))

(defn- centre [[x y z]]
  [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)])

(defn place-book-deltas [world pos ^long st stack]
  (let [e (or (be/at world pos) (be/fresh :lectern nil))
        e' (assoc e :book (assoc stack :count 1) :page 0)]
    [[:set-block-entity pos e']
     [:set-blocks [[pos (lectern/reset-state st true)]]]
     (out/all (out/sound :item.book.put (centre pos) 1.0 1.0))]))

(defn remove-book-deltas [world pos]
  (let [st (state-at (:chunks world) pos)
        e (be/at world pos)]
    [[:set-block-entity pos (assoc e :book nil :page 0)]
     [:set-blocks [[pos (lectern/reset-state st false)]]]]))

(defn next-page ^long [world m ^long want]
  (clamp-page want (page-count (book-of world m))))

(defn page-deltas [world m ^long want]
  (let [pos (:pos m)
        st (state-at (:chunks world) pos)
        e (be/at world pos)
        p (clamp-page want (page-count (:book e)))]
    (when (not= p (long (:page e 0)))
      (let [at (+ (long (:tick world)) lectern/impulse-ticks -1)
            ids [(chunk/block-pos->id pos)]]
        [[:set-block-entity pos (assoc e :page p)]
         [:set-blocks [[pos (lectern/powered-state st true)]]]
         [:schedule-ticks {at ids}]
         (out/all (out/level-event out/sound-page-turn pos 0))]))))

(defn dropped-book [world pos]
  (let [e (be/at world pos)
        st (state-at (:chunks world) pos)]
    (when (and (lectern/has-book? st) (:book e))
      (let [[x y z] pos
            [dx _ dz] (dir/offset (lectern/facing st))
            p [(+ (double x) 0.5 (* 0.25 (double dx)))
               (double (inc (long y)))
               (+ (double z) 0.5 (* 0.25 (double dz)))]
            v (entity/pop-velocity [(:tick world) pos :lectern])]
        [(entity/item p v (:book e))]))))

(defn positions [m]
  (cond
    (lectern? m) []
    (bench? m) []
    (= :ender (:kind m)) [(:pos m)]
    :else (:cells m)))

(defn covers? [m pos]
  (boolean (some #(= pos %) (positions m))))

(defn viewers
  "ContainerOpenersCounter: the players with a menu of the container
  at pos open, spectators left out."
  [world pos]
  (let [sees? (fn [[_ e]]
                (and (:menu e) (covers? (:menu e) pos)
                     (not (game-mode/spectator? e))))]
    (count (filter sees? (:entities world)))))

(defn- pitch [world pos salt]
  (random/hinge-pitch [(:tick world) pos salt]))

(def ^:private copper-hinge
  {:weathered-copper-chest       :block.copper-chest-weathered
   :waxed-weathered-copper-chest :block.copper-chest-weathered
   :oxidized-copper-chest        :block.copper-chest-oxidized
   :waxed-oxidized-copper-chest  :block.copper-chest-oxidized})

(defn- hinge-base [^long st]
  (if (contains? copper-types (block/type-of st))
    (get copper-hinge (block/block-of st) :block.copper-chest)
    :block.chest))

(defn- chest-sound-name [^long st open?]
  (let [base (hinge-base st)]
    (keyword (str (name base) (if open? ".open" ".close")))))

(defn- chest-sound-pos [pos ^long st]
  (let [[x y z] pos
        c [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]]
    (if (= :right (:type (block/props-of st)))
      (let [[dx _ dz] (dir/offset (connected-direction st))]
        [(+ (c 0) (* 0.5 (double dx)))
         (c 1)
         (+ (c 2) (* 0.5 (double dz)))])
      c)))

(defn- chest-sound [world pos ^long st open?]
  (when (not= :left (:type (block/props-of st)))
    (let [nm (chest-sound-name st open?)
          p (chest-sound-pos pos st)]
      [(out/all (out/sound nm p 0.5 (pitch world pos :lid)))])))

(defn- barrel-sound [world pos ^long st open?]
  (let [[x y z] pos
        [dx dy dz] (dir/offset (:facing (block/props-of st)))
        p [(+ (double x) 0.5 (* 0.5 (double dx)))
           (+ (double y) 0.5 (* 0.5 (double dy)))
           (+ (double z) 0.5 (* 0.5 (double dz)))]
        nm (if open? :block.barrel.open :block.barrel.close)]
    [(out/all (out/sound nm p 0.5 (pitch world pos :lid)))]))

(defn- shulker-sound [world pos open?]
  (let [[x y z] pos
        p [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]
        nm (if open?
             :block.shulker-box.open
             :block.shulker-box.close)]
    [(out/all (out/sound nm p 0.5 (pitch world pos :lid)))]))

(defn- ender-sound [world [x y z :as pos] open?]
  [(out/all
     (out/sound
       (if open? :block.ender-chest.open :block.ender-chest.close)
       [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]
       0.5 (pitch world pos :lid)))])

(def ^:private ^:const recheck-delay 5)

(def ^:private open-step (float 0.1))

(defn- trigger-deltas [pos ^long after]
  (cond
    (zero? after) [[:shulker-anim pos {:status :closing}]]
    (= 1 after) [[:shulker-anim pos {:status :opening}]]
    :else nil))

(defn- shulker-step [status up down]
  (case status
    :opening (if (>= (float up) (float 1.0))
               {:status :opened :progress (float 1.0)}
               {:status :opening :progress up})
    :closing (when (> (float down) (float 0.0))
               {:status :closing :progress down})))

(defn animate-deltas [world]
  (mapcat (fn [[pos {:keys [status progress]}]]
            (let [p (float progress)
                  up (float (+ p open-step))
                  down (float (- p open-step))
                  st (state-at (:chunks world) pos)]
              (if (not= :shulker-box (block/type-of st))
                [[:shulker-anim pos nil]]
                (case status
                  (:opening :closing)
                  [[:shulker-anim pos (shulker-step status up down)]]
                  nil))))
          (:shulker-anim world)))

(defn- edge-sound [world pos st t open?]
  (cond
    (contains? chest-types t) (chest-sound world pos st open?)
    (= :barrel t) (barrel-sound world pos st open?)
    (= :shulker-box t) (shulker-sound world pos open?)
    (= :ender-chest t) (ender-sound world pos open?)))

(defn- recheck-delta [world pos t before after]
  (when (and (not= :barrel t) (not= :shulker-box t)
             (zero? before) (pos? after))
    [[:container-recheck pos
      (+ (dec (long (:tick world))) recheck-delay)]]))

(defn count-deltas [world pos ^long before ^long after]
  (let [st (state-at (:chunks world) pos)
        t (block/type-of st)]
    (when (contains? container-types t)
      (concat
        (when (and (zero? before) (pos? after))
          (edge-sound world pos st t true))
        (when (and (pos? before) (zero? after))
          (edge-sound world pos st t false))
        (when (not= :barrel t)
          [(out/all (out/block-event pos 1 (min 255 after)))])
        (when (= :shulker-box t) (trigger-deltas pos after))
        (recheck-delta world pos t before after)))))

(defn- due-recheck [world ^long t [pos at]]
  (when (<= (long at) t)
    (let [st (state-at (:chunks world) pos)
          type (block/type-of st)
          open? (contains? container-types type)
          n (if open? (viewers world pos) 0)
          next-at (when (pos? n) (+ t recheck-delay))
          event (when (not= :barrel type)
                  [(out/all (out/block-event pos 1 (min 255 n)))])]
      (cons [:container-recheck pos next-at] event))))

(defn recheck-deltas [world]
  (let [t (long (:tick world))]
    (mapcat #(due-recheck world t %) (:container-rechecks world))))

(defn barrel-open-state [^long st open?]
  (let [open (if open? :true :false)
        props (assoc (block/props-of st) :open open)]
    (block/state (block/block-of st) props)))

(defn fits-inside? [item]
  (not= :shulker-box (:type (get (data/blocks) item))))

(defn- may-place? [_ stack]
  (fits-inside? (:item stack)))

(defn- shrink [inv slot]
  (let [n (dec (long (:count (get inv slot) 1)))]
    (if (pos? n) (update inv slot assoc :count n) (dissoc inv slot))))

(defn- span [v ^long from ^long to reverse?]
  (map v (if reverse?
           (range (dec to) (dec from) -1)
           (range from to))))

(defn- cut-quick [v inv slot]
  (let [i (long (.indexOf ^List v slot))]
    (cond
      (= 1 i) (span v 2 38 true)
      (= 0 i) (span v 2 38 false)
      (workbench/cuts-input? (get inv slot)) (span v 0 1 false)
      (< 1 i 29) (span v 29 38 false)
      :else (span v 2 29 false))))

(defn- loom-quick [v inv slot]
  (let [i (long (.indexOf ^List v slot))
        stack (get inv slot)]
    (cond
      (= 3 i) (span v 4 40 true)
      (< i 3) (span v 4 40 false)
      (workbench/banner? stack) (span v 0 1 false)
      (workbench/dye? stack) (span v 1 2 false)
      (workbench/pattern-item? stack) (span v 2 3 false)
      (< 3 i 31) (span v 31 40 false)
      :else (span v 4 31 false))))

(defn- cut-derive [inv ^long selected]
  (if-let [r (workbench/cut-result (get inv 0) selected)]
    (assoc inv 1 r)
    (dissoc inv 1)))

(defn- cut-layout [m]
  (let [place? (fn [slot _] (not= 1 (long slot)))
        base (menu/slots-layout 2 place?)
        v (:visible base)
        selected (long (:selected m))]
    (assoc base
      :result 1
      :no-gather #{1}
      :stat {:slot 1 :by :taken}
      :quick (fn [inv slot] (cut-quick v inv slot))
      :on-take (fn [inv] (shrink inv 0))
      :derive (fn [inv] (cut-derive inv selected)))))

(defn- loom-place? [slot stack]
  (case (long slot)
    0 (workbench/banner? stack)
    1 (workbench/dye? stack)
    2 (workbench/pattern-item? stack)
    3 false))

(defn- loom-derive [inv pattern]
  (let [banner (get inv 0)
        dye (get inv 1)]
    (if-let [r (workbench/loom-result banner dye pattern)]
      (assoc inv 3 r)
      (dissoc inv 3))))

(defn- loom-layout [m]
  (let [base (menu/slots-layout 4 loom-place?)
        v (:visible base)
        patterns (vec (:patterns m))
        selected (long (:selected m))
        pattern (when (< -1 selected (count patterns))
                  (nth patterns selected))]
    (assoc base
      :result 3
      :quick (fn [inv slot] (loom-quick v inv slot))
      :on-take (fn [inv] (-> inv (shrink 0) (shrink 1)))
      :derive (fn [inv] (loom-derive inv pattern)))))

(defn- combiner-quick [v ^long result slot]
  (let [i (long (.indexOf ^List v slot))
        total (count v)]
    (cond
      (= i result) (span v (inc result) total true)
      (< i result) (span v (inc result) total false)
      :else (span v 0 result false))))

(defn- anvil-take [m inv]
  (let [n (long (:repair-count m 0))
        left (- (long (:count (get inv 1) 1)) n)
        inv (cond
              (zero? n) (cond-> inv (not (:renaming? m)) (dissoc 1))
              (pos? left) (update inv 1 assoc :count left)
              :else (dissoc inv 1))]
    (dissoc inv 0)))

(defn- anvil-result [m inv creative?]
  (:result (anvil/work (get inv 0) (get inv 1) (:name m) creative?)))

(defn- anvil-layout [m ctx]
  (let [place? (fn [slot _] (not= 2 (long slot)))
        base (menu/slots-layout 3 place?)
        v (:visible base)
        creative? (boolean (:infinite? ctx))]
    (assoc base
      :result 2
      :quick (fn [_ slot] (combiner-quick v 2 slot))
      :on-take (fn [inv] (anvil-take m inv))
      :derive (fn [inv]
                (if-let [r (anvil-result m inv creative?)]
                  (assoc inv 2 r)
                  (dissoc inv 2))))))

(defn- grindstone-place? [slot stack]
  (case (long slot)
    (0 1) (grindstone/accepts? stack)
    2 false))

(defn- grind-result [inv]
  (grindstone/result (get inv 0) (get inv 1)))

(defn- grindstone-layout []
  (let [base (menu/slots-layout 3 grindstone-place?)
        v (:visible base)]
    (assoc base
      :result 2
      :quick (fn [_ slot] (combiner-quick v 2 slot))
      :on-take (fn [inv] (dissoc inv 0 1))
      :derive (fn [inv]
                (if-let [r (grind-result inv)]
                  (assoc inv 2 r)
                  (dissoc inv 2))))))

(defn- smithing-place? [slot stack]
  (case (long slot)
    0 (smithing/template? stack)
    1 (smithing/base? stack)
    2 (smithing/addition? stack)
    3 false))

(defn- smithing-quick [v inv slot]
  (let [i (long (.indexOf ^List v slot))
        total (count v)]
    (cond
      (= 3 i) (span v 4 total true)
      (< i 3) (span v 4 total false)
      :else {:try  (span v 0 3 false)
             :else (if (< i 31)
                     (span v 31 total false)
                     (span v 4 31 false))})))

(defn- smithing-result [inv]
  (smithing/result (get inv 0) (get inv 1) (get inv 2)))

(defn- smithing-layout []
  (let [base (menu/slots-layout 4 smithing-place?)
        v (:visible base)]
    (assoc base
      :result 3
      :no-gather #{3}
      :stat {:slot 3 :by :taken}
      :quick (fn [inv slot] (smithing-quick v inv slot))
      :on-take (fn [inv] (-> inv (shrink 0) (shrink 1) (shrink 2)))
      :derive (fn [inv]
                (if-let [r (smithing-result inv)]
                  (assoc inv 3 r)
                  (dissoc inv 3))))))

(defn- lectern-layout []
  {:count 1 :visible [0]
   :place (fn [_ _] false)
   :swap  (fn ^long [^long _] 0)
   :quick (fn [_ _] nil)})

(defn- fuel? [stack]
  (or (pos? (furnace/burn-duration stack))
      (= :bucket (:item stack))))

(defn- furnace-place? [slot stack]
  (case (long slot) 0 true 1 (fuel? stack) 2 false))

(defn- furnace-max [slot stack]
  (when (and (= 1 (long slot)) (= :bucket (:item stack))) 1))

(defn- furnace-quick [v kind inv slot]
  (let [i (long (.indexOf ^List v slot))
        stack (get inv slot)]
    (cond
      (= 2 i) (span v 3 39 true)
      (< i 2) (span v 3 39 false)
      (furnace/recipe kind stack) (span v 0 1 false)
      (pos? (furnace/burn-duration stack)) (span v 1 2 false)
      (< 2 i 30) (span v 30 39 false)
      :else (span v 3 30 false))))

(defn- furnace-layout [m]
  (let [base (menu/slots-layout 3 furnace-place?)
        v (:visible base)
        kind (:type m)]
    (assoc base
      :max furnace-max
      :stat {:slot 2 :by :removed}
      :quick (fn [inv slot] (furnace-quick v kind inv slot)))))

(defn- brewing-place? [slot stack]
  (case (long slot)
    (0 1 2) (contains? brewing/bottles (:item stack))
    3 (brewing/ingredient? stack)
    4 (brewing/fuel? stack)))

(defn- brewing-max [slot _]
  (when (< (long slot) 3) 1))

(defn- brewing-quick [v inv slot]
  (let [i (long (.indexOf ^List v slot))
        stack (get inv slot)]
    (cond
      (< i 5) (span v 5 41 true)
      (brewing/fuel? stack)
      {:try  (span v 4 5 false)
       :else (when (brewing/ingredient? stack) (span v 3 4 false))}
      (brewing/ingredient? stack) (span v 3 4 false)
      (contains? brewing/bottles (:item stack)) (span v 0 3 false)
      (< 4 i 32) (span v 32 41 false)
      :else (span v 5 32 false))))

(defn- brewing-layout []
  (let [base (menu/slots-layout 5 brewing-place?)
        v (:visible base)]
    (assoc base
      :max brewing-max
      :quick (fn [inv slot] (brewing-quick v inv slot)))))

(defn- typed-layout [m ctx]
  (case (:type m)
    :stonecutter (cut-layout m)
    :loom (loom-layout m)
    :anvil (anvil-layout m ctx)
    :grindstone (grindstone-layout)
    :smithing-table (smithing-layout)
    :crafting-table (crafting/table-layout ctx)
    :shulker-box
    (menu/container-layout (long (:rows m)) may-place?)
    (menu/container-layout (long (:rows m)))))

(defn layout
  ([m] (layout m {:held 0}))
  ([m ctx]
   (cond
     (lectern? m) (lectern-layout)
     (furnace? m) (furnace-layout m)
     (brewing? m) (brewing-layout)
     :else (typed-layout m ctx))))

(def ^:private plain-ctx {:held 0})

(defn derived
  ([m items] (derived m items plain-ctx))
  ([m items ctx]
   (if-not (bench? m)
     items
     (let [sparse (keep-indexed (fn [i s] (when s [i s])) items)
           inv ((:derive (layout m ctx)) (into {} sparse))]
       (mapv #(get inv %) (range (slot-count m)))))))

(defn- anvil-changed [m items ctx]
  (let [w (anvil/work (nth items 0) (nth items 1) (:name m)
                      (boolean (:infinite? ctx)))]
    (assoc m :cost (:cost w) :repair-count (:repair-count w)
             :renaming? (:renaming? w))))

(defn- smithing-changed [m items]
  (let [ins (take 3 items)
        bad? (and (every? some? ins)
                  (nil? (apply smithing/result ins)))]
    (assoc m :recipe-error? (boolean bad?))))

(defn slots-changed
  ([m items] (slots-changed m items plain-ctx))
  ([m items ctx]
   (case (:type m)
     :stonecutter (workbench/cut-changed m items)
     :loom (workbench/loom-changed m items)
     :anvil (anvil-changed m items ctx)
     :smithing-table (smithing-changed m items)
     m)))

(defn settled
  ([m items] (settled m items plain-ctx))
  ([m items ctx]
   (if (or (not (bench? m)) (crafting? m))
     [m items]
     (let [m' (slots-changed m items ctx)]
       [m' (derived m' items ctx)]))))

(defn- option-count ^long [m]
  (case (:type m)
    :stonecutter (count (workbench/cuts (first (:contents m))))
    :loom (count (:patterns m))
    0))

(defn button [m ^long id]
  (let [n (option-count m)]
    (if (and (not= id (long (:selected m))) (< -1 id n))
      (assoc m :selected id)
      m)))

(def ^:private take-sounds
  {:stonecutter :ui.stonecutter.take-result
   :loom        :ui.loom.take-result})

(def ^:private take-events
  {:grindstone     out/sound-grindstone-used
   :smithing-table out/sound-smithing-table-used})

(defn take-sound [m]
  (if-let [event (take-events (:type m))]
    (out/all (out/level-event event (:pos m) 0))
    (when-let [kind (take-sounds (:type m))]
      (let [[x y z] (:pos m)
            p [(+ (double x) 0.5) (+ (double y) 0.5)
               (+ (double z) 0.5)]]
        (out/all (out/sound kind p 1.0 1.0))))))

(def ^:private ^:const wear-chance 0.12)

(defn- worn-state [^long st]
  (when-let [next (anvil/next-stage (block/block-of st))]
    (block/state next (block/props-of st))))

(defn- wear-deltas [pos ^long st]
  (if-let [next (worn-state st)]
    [[:set-blocks [[pos next]]]
     (out/all (out/level-event out/sound-anvil-used pos 0))]
    [[:set-blocks [[pos (block/state :air)]]]
     (out/all (out/level-event out/sound-anvil-broken pos 0))]))

(defn anvil-take-deltas
  "Returns the wear and the sound of a result leaving an anvil."
  [world m creative?]
  (let [pos (:pos m)
        st (state-at (:chunks world) pos)
        roll (random/of-key (:tick world) pos :anvil)]
    (if (and (not creative?) (< roll wear-chance))
      (wear-deltas pos st)
      [(out/all (out/level-event out/sound-anvil-used pos 0))])))
