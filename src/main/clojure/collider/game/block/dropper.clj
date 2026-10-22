(ns collider.game.block.dropper
  "A dropper throws one item, or pushes it into a facing container."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.dispenser :as dispenser]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- taken [s]
  (let [n (dec (long (stack/size s)))]
    (when (pos? n) (assoc s :count n))))

(defn- fits? [cur s]
  (and cur (stack/same-kind? cur s)
       (< (long (stack/size cur)) (long (stack/max-size s)))))

(defn- slots [kind face n]
  (if (#{:furnace :blast-furnace :smoker} kind)
    (case face :up [0] :down [] [1])
    (range n)))

(defn- place [items s allowed]
  (or (some (fn [i]
              (when (fits? (nth items i) s)
                (update items i update :count inc)))
            allowed)
      (some (fn [i]
              (when (nil? (nth items i)) (assoc items i s)))
            allowed)))

(defn- cooled [e empty?]
  (if (and empty? (= :hopper (:kind e))
           (<= (long (:cooldown e -1)) 8))
    (assoc e :cooldown 8)
    e))

(defn- held [world there]
  (let [box (be/at world there)]
    (when (:items box) box)))

(defn- moved [pos e items i stack there box into empty?]
  (let [src (assoc e :items (assoc items i (taken stack)))
        dst (cooled (assoc box :items into) empty?)]
    [[:set-block-entity pos src]
     [:set-block-entity there dst]]))

(defn- poured [world pos facing e items i stack]
  (let [there (mapv + pos (dir/offset facing))
        box (held world there)]
    (when box
      (let [cur (vec (:items box))
            face (dir/opposite facing)
            one (assoc stack :count 1)
            into (place cur one (slots (:kind box) face (count cur)))]
        (if into
          (moved pos e items i stack there box into
                 (not-any? some? cur))
          [])))))

(defn fired
  "Returns the deltas of the dropper at pos throwing once.
  A container in front keeps the item when it has no room."
  [world pos]
  (let [found (be/at world pos)
        e (or found (be/fresh :dropper nil))
        items (vec (:items e))
        facing (block/facing-of (chunk/at (:chunks world) pos))
        i (dispenser/picked items (:tick world) pos)
        ds (if (neg? i)
             [(out/all (out/level-event :sound-dispenser-fail pos 0))]
             (or (poured world pos facing e items i (nth items i))
                 (dispenser/thrown world pos facing e items i)))]
    (if found ds (into [[:set-block-entity pos e]] ds))))
