(ns collider.game.container
  (:require [collider.data :as data]
            [collider.game.blockentity :as be]
            [collider.game.menu :as menu]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.game.workbench :as workbench]
            [collider.world.block :as block]
            [collider.world.chest :as chest])
  (:import (java.util List)))

(set! *warn-on-reflection* true)

(def size 27)
(def chest-types chest/types)
(def bench-types #{:stonecutter :loom})
(def container-types
  (into (conj chest/types :barrel :ender-chest :shulker-box) bench-types))

(def state-at chest/state-at)
(def connected-direction chest/connected-direction)
(def copper-types chest/copper-types)

(defn placed-state [chunks pos st face sneaking? yaw pitch]
  (let [t (block/type-of st)]
    (cond
      (contains? chest/types t) (chest/placed chunks pos st face sneaking?)
      (= :barrel t) (chest/barrel-placed st yaw pitch)
      :else st)))

(defn blocked? [chunks pos]
  (block/full-cube? (chest/state-at chunks (mapv + pos [0 1 0]))))

(defn- chest-menu [chunks pos ^long st]
  (when-not (blocked? chunks pos)
    (if-let [p2 (chest/partner chunks pos st)]
      (when-not (blocked? chunks p2)
        {:kind :block :rows 6 :type :generic-9x6 :title {:translate "container.chestDouble"}
         :cells (if (= :right (:type (block/props-of st))) [pos p2] [p2 pos])})
      {:kind :block :rows 3 :type :generic-9x3 :title {:translate "container.chest"} :cells [pos]})))

(def ^:private dir-offset
  {:down [0 -1 0] :up [0 1 0] :north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})

(def ^:private axis-of {:down 1 :up 1 :north 2 :south 2 :west 0 :east 0})
(def ^:private positive? #{:up :south :east})

(defn- half-free?
  "Shulker.getProgressDeltaAabb(1, facing, 0, 0.5) over the neighbour cell:
   the half of it next to the box must hold no collision box."
  [chunks pos facing]
  (let [st (state-at chunks (mapv + pos (dir-offset facing)))
        ax (long (axis-of facing))
        far? (contains? positive? facing)]
    (not-any? (fn [box]
                (let [lo (double (nth box ax)) hi (double (nth box (+ ax 3)))]
                  (if far? (< lo 8.0) (> hi 8.0))))
              (block/collision-boxes st))))

(defn animation [world pos]
  (get (:shulker-anim world) pos))

(defn can-open?
  "ShulkerBoxBlock.canOpen: an already moving or open lid always opens."
  [world pos ^long st]
  (or (some? (animation world pos))
      (half-free? (:chunks world) pos (:facing (block/props-of st)))))

(defn menu-at [world pos]
  (let [chunks (:chunks world)
        st (state-at chunks pos) t (block/type-of st)]
    (cond
      (contains? chest-types t) (chest-menu chunks pos st)
      (= :barrel t) {:kind :block :rows 3 :type :generic-9x3
                     :title {:translate "container.barrel"} :cells [pos]}
      (= :shulker-box t) (when (can-open? world pos st)
                           {:kind :block :rows 3 :type :shulker-box
                            :title {:translate "container.shulkerBox"} :cells [pos]})
      (= :ender-chest t) (when-not (blocked? chunks pos)
                           {:kind :ender :rows 3 :type :generic-9x3
                            :title {:translate "container.enderchest"} :cells [] :pos pos})
      (= :stonecutter t) {:kind :bench :type :stonecutter :size 2 :result 1
                          :title {:translate "container.stonecutter"}
                          :cells [] :pos pos :selected 0 :contents [nil nil]}
      (= :loom t) {:kind :bench :type :loom :size 4 :result 3
                   :title {:translate "container.loom"}
                   :cells [] :pos pos :selected 0 :patterns [] :contents [nil nil nil nil]})))

(defn bench? [m] (= :bench (:kind m)))

(defn slot-count ^long [m]
  (if (bench? m) (long (:size m)) (* 9 (long (:rows m)))))

(defn- padded [items] (vec (take size (concat items (repeat nil)))))

(defn cell-items [world pos] (padded (:items (be/at world pos))))

(defn items [world eid m]
  (cond
    (bench? m) (vec (take (slot-count m) (concat (:contents m) (repeat nil))))
    (= :ender (:kind m)) (padded (get-in world [:entities eid :ender-items]))
    :else (into [] (mapcat #(cell-items world %)) (:cells m))))

(defn inputs
  "AbstractContainerMenu.clearContainer on removed: the result is discarded, the
   inputs go back to the player."
  [m items]
  (keep-indexed (fn [i s] (when (not= i (:result m)) s)) items))

(defn store-deltas
  "Where the contents of an open menu live. A bench keeps its own on the menu,
   so it stores nothing of its own here."
  [world eid m items]
  (cond
    (bench? m) nil
    (= :ender (:kind m)) [[:merge-entity eid {:ender-items (vec items)}]]
    :else
    (keep-indexed (fn [i pos]
                    (let [old (be/at world pos)
                          part (vec (subvec (vec items) (* i size) (* (inc (long i)) size)))]
                      (when (not= (padded (:items old)) part)
                        [:set-block-entity pos (assoc old :items part)])))
                  (:cells m))))

(defn positions [m]
  (cond (bench? m) [] (= :ender (:kind m)) [(:pos m)] :else (:cells m)))

(defn covers? [m pos]
  (boolean (some #(= pos %) (positions m))))

(defn viewers [world pos]
  (count (filter (fn [[_ e]] (and (:menu e) (covers? (:menu e) pos))) (:entities world))))

(defn- pitch [world pos salt]
  (+ 0.9 (* 0.1 (random/of-key [(:tick world) pos salt]))))

(def ^:private copper-hinge
  {:weathered-copper-chest :block.copper-chest-weathered
   :waxed-weathered-copper-chest :block.copper-chest-weathered
   :oxidized-copper-chest :block.copper-chest-oxidized
   :waxed-oxidized-copper-chest :block.copper-chest-oxidized})

(defn- chest-sound-name [^long st open?]
  (let [t (block/type-of st)
        base (if (contains? copper-types t)
               (get copper-hinge (block/block-of st) :block.copper-chest)
               :block.chest)]
    (keyword (str (name base) (if open? ".open" ".close")))))

(defn- chest-sound-pos [pos ^long st]
  (let [[x y z] pos
        c [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]]
    (if (= :right (:type (block/props-of st)))
      (let [[dx _ dz] (chest/offset (connected-direction st))]
        [(+ (c 0) (* 0.5 (double dx))) (c 1) (+ (c 2) (* 0.5 (double dz)))])
      c)))

(defn- chest-sound [world pos ^long st open?]
  (when (not= :left (:type (block/props-of st)))
    [(out/all (out/sound (chest-sound-name st open?) (chest-sound-pos pos st)
                         0.5 (pitch world pos :lid)))]))

(defn- barrel-sound [world pos ^long st open?]
  (let [[x y z] pos
        [dx dy dz] (chest/offset (:facing (block/props-of st)))]
    [(out/all (out/sound (if open? :block.barrel.open :block.barrel.close)
                         [(+ (double x) 0.5 (* 0.5 (double dx)))
                          (+ (double y) 0.5 (* 0.5 (double dy)))
                          (+ (double z) 0.5 (* 0.5 (double dz)))]
                         0.5 (pitch world pos :lid)))]))

(defn- shulker-sound [world pos open?]
  (let [[x y z] pos]
    [(out/all (out/sound (if open? :block.shulker-box.open :block.shulker-box.close)
                         [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]
                         0.5 (pitch world pos :lid)))]))

(defn- ender-sound [world pos open?]
  (let [[x y z] pos]
    [(out/all (out/sound (if open? :block.ender-chest.open :block.ender-chest.close)
                         [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]
                         0.5 (pitch world pos :lid)))]))

(def ^:private ^:const recheck-delay 5)
(def ^:private step (float 0.1))

(defn- trigger-deltas
  "ShulkerBoxBlockEntity.triggerEvent: opener count 1 starts opening, 0 closing."
  [pos ^long after]
  (cond
    (zero? after) [[:shulker-anim pos {:status :closing}]]
    (= 1 after) [[:shulker-anim pos {:status :opening}]]
    :else nil))

(defn animate-deltas
  "ShulkerBoxBlockEntity.updateAnimation: 0.1 of the lid per tick."
  [world]
  (mapcat (fn [[pos {:keys [status progress]}]]
            (let [p (float progress)
                  up (float (+ p step)) down (float (- p step))]
              (if (not= :shulker-box (block/type-of (state-at (:chunks world) pos)))
                [[:shulker-anim pos nil]]
                (case status
                  :opening [[:shulker-anim pos (if (>= up (float 1.0))
                                                 {:status :opened :progress (float 1.0)}
                                                 {:status :opening :progress up})]]
                  :closing [[:shulker-anim pos (when (> down (float 0.0))
                                                 {:status :closing :progress down})]]
                  nil))))
          (:shulker-anim world)))

(defn count-deltas [world pos ^long before ^long after]
  (let [st (state-at (:chunks world) pos)
        t (block/type-of st)
        edge (fn [open?] (cond
                           (contains? chest-types t) (chest-sound world pos st open?)
                           (= :barrel t) (barrel-sound world pos st open?)
                           (= :shulker-box t) (shulker-sound world pos open?)
                           (= :ender-chest t) (ender-sound world pos open?)))]
    (concat
     (when (and (zero? before) (pos? after)) (edge true))
     (when (and (pos? before) (zero? after)) (edge false))
     (when (not= :barrel t) [(out/all (out/block-event pos 1 (min 255 after)))])
     (when (= :shulker-box t) (trigger-deltas pos after))
     ;; ShulkerBoxBlockEntity keeps its own openCount and never schedules a
     ;; recheck: only ContainerOpenersCounter does.
     (when (and (not= :barrel t) (not= :shulker-box t) (zero? before) (pos? after))
       [[:container-recheck pos (+ (dec (long (:tick world))) recheck-delay)]]))))

(defn recheck-deltas [world]
  (let [t (long (:tick world))]
    (mapcat (fn [[pos at]]
              (when (<= (long at) t)
                (let [st (state-at (:chunks world) pos)
                      n (if (contains? container-types (block/type-of st)) (viewers world pos) 0)]
                  (cons [:container-recheck pos (when (pos? n) (+ t recheck-delay))]
                        (when (not= :barrel (block/type-of st))
                          [(out/all (out/block-event pos 1 (min 255 n)))])))))
            (:container-rechecks world))))

(defn barrel-open-state [^long st open?]
  (block/state (block/block-of st) (assoc (block/props-of st) :open (if open? :true :false))))

(defn fits-inside?
  "Item.canFitInsideContainerItems: BlockItem of a ShulkerBoxBlock cannot."
  [item]
  (not= :shulker-box (:type (get @data/blocks item))))

(defn- may-place? [_ stack]
  (fits-inside? (:item stack)))

(defn- shrink [inv slot]
  (let [n (dec (long (:count (get inv slot) 1)))]
    (if (pos? n) (update inv slot assoc :count n) (dissoc inv slot))))

(defn- span [v ^long from ^long to reverse?]
  (map v (if reverse? (range (dec to) (dec from) -1) (range from to))))

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

(defn- cut-layout [m]
  (let [base (menu/slots-layout 2 (fn [slot _] (not= 1 (long slot))))
        v (:visible base)
        selected (long (:selected m))]
    (assoc base
           :result 1
           :quick (fn [inv slot] (cut-quick v inv slot))
           :on-take (fn [inv] (shrink inv 0))
           :derive (fn [inv]
                     (if-let [r (workbench/cut-result (get inv 0) selected)]
                       (assoc inv 1 r)
                       (dissoc inv 1))))))

(defn- loom-place? [slot stack]
  (case (long slot)
    0 (workbench/banner? stack)
    1 (workbench/dye? stack)
    2 (workbench/pattern-item? stack)
    3 false))

(defn- loom-layout [m]
  (let [base (menu/slots-layout 4 loom-place?)
        v (:visible base)
        patterns (vec (:patterns m))
        selected (long (:selected m))
        pattern (when (< -1 selected (count patterns)) (nth patterns selected))]
    (assoc base
           :result 3
           :quick (fn [inv slot] (loom-quick v inv slot))
           :on-take (fn [inv] (-> inv (shrink 0) (shrink 1)))
           :derive (fn [inv]
                     (if-let [r (workbench/loom-result (get inv 0) (get inv 1) pattern)]
                       (assoc inv 3 r)
                       (dissoc inv 3))))))

(defn layout [m]
  (case (:type m)
    :stonecutter (cut-layout m)
    :loom (loom-layout m)
    :shulker-box (menu/container-layout (long (:rows m)) may-place?)
    (menu/container-layout (long (:rows m)))))

(defn derived
  "The result slot rebuilt from the inputs: setupResultSlot."
  [m items]
  (if-not (bench? m)
    items
    (let [inv ((:derive (layout m))
               (into {} (keep-indexed (fn [i s] (when s [i s]))) items))]
      (mapv #(get inv %) (range (slot-count m))))))

(defn slots-changed
  "The menu's own reaction to its inputs changing: StonecutterMenu.slotsChanged
   and LoomMenu.slotsChanged."
  [m items]
  (case (:type m)
    :stonecutter (workbench/cut-changed m items)
    :loom (workbench/loom-changed m items)
    m))

(defn settled
  "A bench after its slots moved: slotsChanged and then the result slot again."
  [m items]
  (if-not (bench? m)
    [m items]
    (let [m' (slots-changed m items)]
      [m' (derived m' items)])))

(defn button
  "AbstractContainerMenu.clickMenuButton for the two benches: the button picks
   an entry of the list the menu currently offers."
  [m ^long id]
  (let [n (case (:type m)
            :stonecutter (count (workbench/cuts (first (:contents m))))
            :loom (count (:patterns m))
            0)]
    (if (and (not= id (long (:selected m))) (< -1 id n))
      (assoc m :selected id)
      m)))

(def ^:private take-sounds
  {:stonecutter :ui.stonecutter.take-result
   :loom :ui.loom.take-result})

(defn take-sound [m]
  (let [[x y z] (:pos m)]
    (out/all (out/sound (take-sounds (:type m))
                        [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]
                        1.0 1.0))))
