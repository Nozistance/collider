(ns collider.game.systems.campfires
  "Campfires cooking the food laid on them."
  (:require [collider.game.block.campfire :as campfire]
            [collider.game.item :as item]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const slots 4)

(def ^:private ^:const cool-speed 2)

(defn- result [stack]
  (or (:out (campfire/recipe (:item stack))) stack))

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
           [:spawn-entity (item/popped world pos part salt)])
         (item/split-drop world pos stack salt))))

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
  (let [st (chunk/at (:chunks world) pos)]
    (= :true (:lit (block/props-of st)))))

(defn tick-deltas
  "Returns the deltas of one tick of the campfire e at pos."
  [world [pos e]]
  (if (lit? world pos)
    (cook-deltas world pos e)
    (cool-deltas pos e)))
