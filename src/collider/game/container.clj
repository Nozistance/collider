(ns collider.game.container
  (:require [collider.game.blockentity :as be]
            [collider.game.menu :as menu]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chest :as chest]))

(set! *warn-on-reflection* true)

(def size 27)
(def chest-types chest/types)
(def container-types (conj chest/types :barrel :ender-chest))

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

(defn menu-at [chunks pos]
  (let [st (state-at chunks pos) t (block/type-of st)]
    (cond
      (contains? chest-types t) (chest-menu chunks pos st)
      (= :barrel t) {:kind :block :rows 3 :type :generic-9x3
                     :title {:translate "container.barrel"} :cells [pos]}
      (= :ender-chest t) (when-not (blocked? chunks pos)
                           {:kind :ender :rows 3 :type :generic-9x3
                            :title {:translate "container.enderchest"} :cells [] :pos pos}))))

(defn- padded [items] (vec (take size (concat items (repeat nil)))))

(defn cell-items [world pos] (padded (:items (be/at world pos))))

(defn items [world eid m]
  (if (= :ender (:kind m))
    (padded (get-in world [:entities eid :ender-items]))
    (into [] (mapcat #(cell-items world %)) (:cells m))))

(defn store-deltas [world eid m items]
  (if (= :ender (:kind m))
    [[:merge-entity eid {:ender-items (vec items)}]]
    (keep-indexed (fn [i pos]
                    (let [old (be/at world pos)
                          part (vec (subvec (vec items) (* i size) (* (inc (long i)) size)))]
                      (when (not= (padded (:items old)) part)
                        [:set-block-entity pos (assoc old :items part)])))
                  (:cells m))))

(defn positions [m]
  (if (= :ender (:kind m)) [(:pos m)] (:cells m)))

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

(defn- ender-sound [world pos open?]
  (let [[x y z] pos]
    [(out/all (out/sound (if open? :block.ender-chest.open :block.ender-chest.close)
                         [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]
                         0.5 (pitch world pos :lid)))]))

(def ^:private ^:const recheck-delay 5)

(defn count-deltas [world pos ^long before ^long after]
  (let [st (state-at (:chunks world) pos)
        t (block/type-of st)
        edge (fn [open?] (cond
                           (contains? chest-types t) (chest-sound world pos st open?)
                           (= :barrel t) (barrel-sound world pos st open?)
                           (= :ender-chest t) (ender-sound world pos open?)))]
    (concat
     (when (and (zero? before) (pos? after)) (edge true))
     (when (and (pos? before) (zero? after)) (edge false))
     (when (not= :barrel t) [(out/all (out/block-event pos 1 (min 255 after)))])
     (when (and (not= :barrel t) (zero? before) (pos? after))
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

(defn layout [m] (menu/container-layout (long (:rows m))))
