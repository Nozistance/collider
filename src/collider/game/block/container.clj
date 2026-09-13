(ns collider.game.block.container
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.entity :as entity]
            [collider.game.block.menu :as menu]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.game.block.workbench :as workbench]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.blocks.chest :as chest]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.lectern :as lectern])
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
        {:kind  :block :rows 6 :type :generic-9x6 :title {:translate "container.chestDouble"}
         :cells (if (= :right (:type (block/props-of st))) [pos p2] [p2 pos])})
      {:kind :block :rows 3 :type :generic-9x3 :title {:translate "container.chest"} :cells [pos]})))

(def ^:private axis-index {:x 0 :y 1 :z 2})
(def ^:private positive? #{:up :south :east})

(defn- half-free?
  [chunks pos facing]
  (let [st (state-at chunks (mapv + pos (dir/offset facing)))
        ax (long (axis-index (dir/axis facing)))
        far? (contains? positive? facing)]
    (not-any? (fn [box]
                (let [lo (double (nth box ax)) hi (double (nth box (+ ax 3)))]
                  (if far? (< lo 8.0) (> hi 8.0))))
              (block/collision-boxes st))))

(defn animation [world pos]
  (get (:shulker-anim world) pos))

(defn can-open?
  [world pos ^long st]
  (or (some? (animation world pos))
      (half-free? (:chunks world) pos (:facing (block/props-of st)))))

(def ^:private menu-builders
  {:barrel      (fn [_ _ pos _] {:kind  :block :rows 3 :type :generic-9x3
                                 :title {:translate "container.barrel"} :cells [pos]})
   :shulker-box (fn [world _ pos st] (when (can-open? world pos st)
                                       {:kind  :block :rows 3 :type :shulker-box
                                        :title {:translate "container.shulkerBox"} :cells [pos]}))
   :ender-chest (fn [_ chunks pos _] (when-not (blocked? chunks pos)
                                       {:kind  :ender :rows 3 :type :generic-9x3
                                        :title {:translate "container.enderchest"} :cells [] :pos pos}))
   :stonecutter (fn [_ _ pos _] {:kind  :bench :type :stonecutter :size 2 :result 1
                                 :title {:translate "container.stonecutter"}
                                 :cells [] :pos pos :selected 0 :contents [nil nil]})
   :lectern     (fn [_ _ pos st] (when (lectern/has-book? st)
                                   {:kind  :lectern :type :lectern
                                    :title {:translate "container.lectern"}
                                    :cells [] :pos pos}))
   :loom        (fn [_ _ pos _] {:kind  :bench :type :loom :size 4 :result 3
                                 :title {:translate "container.loom"}
                                 :cells [] :pos pos :selected 0 :patterns [] :contents [nil nil nil nil]})})

(defn menu-at [world pos]
  (let [chunks (:chunks world)
        st (state-at chunks pos) t (block/type-of st)]
    (if (contains? chest-types t)
      (chest-menu chunks pos st)
      (when-let [f (menu-builders t)] (f world chunks pos st)))))

(defn bench? [m] (= :bench (:kind m)))
(defn lectern? [m] (= :lectern (:kind m)))

(defn player-slots?
  [m]
  (not (lectern? m)))

(defn slot-count ^long [m]
  (cond
    (lectern? m) 1
    (bench? m) (long (:size m))
    :else (* 9 (long (:rows m)))))

(def book-items (set (data/tag-values "item" "lectern_books")))

(defn book?
  [stack]
  (contains? book-items (:item stack)))

(defn page-count ^long [stack]
  (let [c (:components stack)]
    (cond
      (:written-book-content c) (count (:pages (:written-book-content c)))
      (:writable-book-content c) (count (:writable-book-content c))
      :else 0)))

(defn clamp-page ^long [^long page ^long pages]
  (if (< page 0) 0 (min page (dec pages))))

(defn- padded [items] (vec (take size (concat items (repeat nil)))))

(defn cell-items [world pos] (padded (:items (be/at world pos))))

(defn book-of [world m] (:book (be/at world (:pos m))))

(defn page ^long [world m] (long (:page (be/at world (:pos m)) 0)))

(defn items [world eid m]
  (cond
    (lectern? m) [(book-of world m)]
    (bench? m) (vec (take (slot-count m) (concat (:contents m) (repeat nil))))
    (= :ender (:kind m)) (padded (get-in world [:entities eid :ender-items]))
    :else (into [] (mapcat #(cell-items world %)) (:cells m))))

(defn inputs
  [m items]
  (keep-indexed (fn [i s] (when (not= i (:result m)) s)) items))

(defn store-deltas
  [world eid m items]
  (cond
    (lectern? m) nil
    (bench? m) nil
    (= :ender (:kind m)) [[:merge-entity eid {:ender-items (vec items)}]]
    :else
    (keep-indexed (fn [i pos]
                    (let [old (be/at world pos)
                          part (vec (subvec (vec items) (* i size) (* (inc (long i)) size)))]
                      (when (not= (padded (:items old)) part)
                        [:set-block-entity pos (assoc old :items part)])))
                  (:cells m))))

(defn- centre [[x y z]]
  [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)])

(defn place-book-deltas
  [world pos ^long st stack]
  (let [e (or (be/at world pos) (be/fresh :lectern nil))]
    [[:set-block-entity pos (assoc e :book (assoc stack :count 1) :page 0)]
     [:set-blocks [[pos (lectern/reset-state st true)]]]
     (out/all (out/sound :item.book.put (centre pos) 1.0 1.0))]))

(defn remove-book-deltas
  [world pos]
  (let [st (state-at (:chunks world) pos)
        e (be/at world pos)]
    [[:set-block-entity pos (assoc e :book nil :page 0)]
     [:set-blocks [[pos (lectern/reset-state st false)]]]]))

(defn next-page ^long [world m ^long want]
  (clamp-page want (page-count (book-of world m))))

(defn page-deltas
  [world m ^long want]
  (let [pos (:pos m)
        st (state-at (:chunks world) pos)
        e (be/at world pos)
        p (clamp-page want (page-count (:book e)))]
    (when (not= p (long (:page e 0)))
      [[:set-block-entity pos (assoc e :page p)]
       [:set-blocks [[pos (lectern/powered-state st true)]]]
       [:schedule-ticks {(+ (long (:tick world)) lectern/impulse-ticks -1)
                         [(chunk/block-pos->id pos)]}]
       (out/all (out/level-event out/sound-page-turn pos 0))])))

(defn dropped-book
  [world pos]
  (let [e (be/at world pos)
        st (state-at (:chunks world) pos)]
    (when (and (lectern/has-book? st) (:book e))
      (let [[x y z] pos
            [dx _ dz] (dir/offset (lectern/facing st))]
        [(entity/item [(+ (double x) 0.5 (* 0.25 (double dx)))
                       (double (inc (long y)))
                       (+ (double z) 0.5 (* 0.25 (double dz)))]
                      (entity/pop-velocity [(:tick world) pos :lectern])
                      (:book e))]))))

(defn positions [m]
  (cond (lectern? m) [] (bench? m) [] (= :ender (:kind m)) [(:pos m)] :else (:cells m)))

(defn covers? [m pos]
  (boolean (some #(= pos %) (positions m))))

(defn viewers [world pos]
  (count (filter (fn [[_ e]] (and (:menu e) (covers? (:menu e) pos))) (:entities world))))

(defn- pitch [world pos salt] (random/hinge-pitch [(:tick world) pos salt]))

(def ^:private copper-hinge
  {:weathered-copper-chest       :block.copper-chest-weathered
   :waxed-weathered-copper-chest :block.copper-chest-weathered
   :oxidized-copper-chest        :block.copper-chest-oxidized
   :waxed-oxidized-copper-chest  :block.copper-chest-oxidized})

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
      (let [[dx _ dz] (dir/offset (connected-direction st))]
        [(+ (c 0) (* 0.5 (double dx))) (c 1) (+ (c 2) (* 0.5 (double dz)))])
      c)))

(defn- chest-sound [world pos ^long st open?]
  (when (not= :left (:type (block/props-of st)))
    [(out/all (out/sound (chest-sound-name st open?) (chest-sound-pos pos st)
                         0.5 (pitch world pos :lid)))]))

(defn- barrel-sound [world pos ^long st open?]
  (let [[x y z] pos
        [dx dy dz] (dir/offset (:facing (block/props-of st)))]
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
(def ^:private open-step (float 0.1))

(defn- trigger-deltas
  [pos ^long after]
  (cond
    (zero? after) [[:shulker-anim pos {:status :closing}]]
    (= 1 after) [[:shulker-anim pos {:status :opening}]]
    :else nil))

(defn animate-deltas
  [world]
  (mapcat (fn [[pos {:keys [status progress]}]]
            (let [p (float progress)
                  up (float (+ p open-step)) down (float (- p open-step))]
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
  [item]
  (not= :shulker-box (:type (get data/blocks item))))

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

(defn- lectern-layout []
  {:count 1 :visible [0]
   :place (fn [_ _] false)
   :swap  (fn ^long [^long _] 0)
   :quick (fn [_ _] nil)})

(defn layout [m]
  (if (lectern? m)
    (lectern-layout)
    (case (:type m)
      :stonecutter (cut-layout m)
      :loom (loom-layout m)
      :shulker-box (menu/container-layout (long (:rows m)) may-place?)
      (menu/container-layout (long (:rows m))))))

(defn derived
  [m items]
  (if-not (bench? m)
    items
    (let [inv ((:derive (layout m))
               (into {} (keep-indexed (fn [i s] (when s [i s]))) items))]
      (mapv #(get inv %) (range (slot-count m))))))

(defn slots-changed
  [m items]
  (case (:type m)
    :stonecutter (workbench/cut-changed m items)
    :loom (workbench/loom-changed m items)
    m))

(defn settled
  [m items]
  (if-not (bench? m)
    [m items]
    (let [m' (slots-changed m items)]
      [m' (derived m' items)])))

(defn button
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
   :loom        :ui.loom.take-result})

(defn take-sound [m]
  (let [[x y z] (:pos m)]
    (out/all (out/sound (take-sounds (:type m))
                        [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]
                        1.0 1.0))))
