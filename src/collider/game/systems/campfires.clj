(ns collider.game.systems.campfires
  "Campfires cooking the food laid on them."
  (:require [collider.game.block.furnace :as furnace]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const slots 4)

(def ^:private ^:const cool-speed 2)

(defn recipe
  "Returns the campfire recipe that cooks the item, if any."
  [item]
  (furnace/recipe :campfire {:item item :count 1}))

(defn- free-slot [items]
  (first (keep-indexed (fn [i s] (when (nil? s) i)) items)))

(defn place-food
  "Returns the campfire with one piece of the item on its first
  free slot, or nil when it takes nothing."
  [e item]
  (when-let [i (free-slot (:items e))]
    (when-let [r (recipe item)]
      (-> e
          (assoc-in [:items i] {:item item :count 1})
          (assoc-in [:cook i] 0)
          (assoc-in [:cook-total i] (:time r))))))

(defn- result [stack]
  (or (:out (recipe (:item stack))) stack))

(defn- ripe? [e ^long i]
  (>= (long (nth (:cook e) i))
      (long (nth (:cook-total e) i))))

(defn- step [[e drops] ^long i]
  (if (nil? (nth (:items e) i))
    [e drops]
    (let [e' (update-in e [:cook i] inc)]
      (if (ripe? e' i)
        [(assoc-in e' [:items i] nil)
         (conj drops [i (result (nth (:items e') i))])]
        [e' drops]))))

(defn- cool-slot [e ^long i]
  (let [c (long (nth (:cook e) i))]
    (if (pos? c)
      (assoc-in e [:cook i]
                (min (max 0 (- c cool-speed))
                     (long (nth (:cook-total e) i))))
      e)))

(defn- drop-deltas [world pos [i stack]]
  (let [salt [:campfire i]]
    (map (fn [part]
           [:spawn-entity (items/popped world pos part salt)])
         (items/split-drop world pos stack salt))))

(defn- cook-deltas [world pos e]
  (let [[e' drops] (reduce step [e []] (range slots))]
    (concat (when (not= e e') [[:set-block-entity pos e']])
            (mapcat (partial drop-deltas world pos) drops)
            (when (seq drops)
              [(out/all (out/block-entity pos))]))))

(defn- cool-deltas [pos e]
  (let [e' (reduce cool-slot e (range slots))]
    (when (not= e e') [[:set-block-entity pos e']])))

(defn- lit? [world pos]
  (let [st (chunk/chunks-get-block (:chunks world) pos)]
    (= :true (:lit (block/props-of st)))))

(defn- campfire-deltas [world [pos e]]
  (if (lit? world pos)
    (cook-deltas world pos e)
    (cool-deltas pos e)))

(defn- campfires [world]
  (let [active (state/active-chunks world)]
    (for [[cid entries] (:block-entities world)
          :when (contains? active cid)
          [pos e] entries
          :when (= :campfire (:kind e))]
      [pos e])))

(defn campfire-cooking [world _d]
  [#(mapcat (partial campfire-deltas world) (campfires world))])
