(ns collider.game.systems.blocks
  (:require [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.game.systems.blocks.bed :as bed]
            [collider.game.systems.blocks.bucket :as bucket]
            [collider.game.systems.blocks.dig :as dig]
            [collider.game.systems.blocks.door :as door]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.place :as place]
            [collider.game.systems.blocks.reach :as reach]
            [collider.game.systems.blocks.tools :as tools]
            [collider.game.systems.blocks.use :as use]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn- clicked-scaffolding? [{:keys [world pos item use-item?]}]
  (and (= :scaffolding item) (not use-item?) (= :scaffolding (block/type-of (edit/block-at world pos)))))

(defn- when-use [f] (fn [c] (when (:use-item? c) (f c))))
(defn- when-hand [f] (fn [c] (when-not (:use-item? c) (f c))))
(defn- item-is [k] (comp #{k} :item))
(defn- tool-is [d] (fn [c] (d (:item c))))
(defn- on-args [f] (fn [{:keys [world args]}] (f world args)))

(def ^:private item-actions
  [[clicked-scaffolding? (fn [{:keys [world eid pos face]}] (place/scaffold-place-deltas world eid pos face))]
   [(comp nil? :item) (constantly nil)]
   [:pour (when-use (fn [{:keys [world eid at pour]}] (bucket/add world eid at pour)))]
   [(item-is :flint-and-steel) (when-hand (on-args tools/flint-deltas))]
   [(item-is :bucket) (when-use (fn [{:keys [world eid at]}] (bucket/scoop-deltas world eid at)))]
   [(item-is :lily-pad) (when-use (fn [{:keys [world eid at]}] (bucket/lily-deltas world eid at)))]
   [(item-is :potion) (when-hand (fn [{:keys [world eid pos face]}] (tools/mud-deltas world eid pos face)))]
   [(item-is :bone-meal) (when-hand (on-args tools/bonemeal-deltas))]
   [(tool-is tools/hoes) (when-hand (on-args tools/till-deltas))]
   [(item-is :honeycomb) (when-hand (on-args tools/wax-deltas))]
   [(tool-is tools/axes) (when-hand (on-args (fn [w args] (or (tools/axe-deltas w args) (place/solid-place-deltas w args)))))]
   [(tool-is tools/shovels) (when-hand (on-args tools/flatten-deltas))]
   [(comp mobs/egg-type :item) (when-hand (on-args tools/spawn-egg-deltas))]
   [(fn [{:keys [item use-item?]}] (and use-item? (tools/armor-slot-of item)))
    (fn [{:keys [world eid item]}] (tools/equip-armor-deltas world eid item (tools/armor-slot-of item)))]
   [(constantly true) (on-args place/solid-place-deltas)]])

(defn- item-deltas [ctx]
  (when-let [[_ f] (first (filter (fn [[pred _]] (pred ctx)) item-actions))]
    (f ctx)))

(defn- place-deltas [world [eid pos face item cursor] origin]
  (let [item (or item (sense/held-of (get-in world [:entities eid])))
        at (merge (get-in world [:entities eid]) origin)
        world (assoc-in world [:entities eid] at)
        use-item? (= 255 (bit-and (long face) 0xFF))
        used (when (and (not use-item?) (not (and item (:sneaking? at))))
               (use/deltas world eid pos face item cursor))]
    (cond
      (door/opens? world eid pos item use-item?) (door/toggle-deltas world eid pos (edit/block-at world pos))
      (bed/uses-bed? world eid pos item use-item?) (bed/sleep-deltas world eid pos)
      used used
      :else (item-deltas {:world world :eid eid :pos pos :face face :item item :at at
                          :args  [eid pos face item cursor] :use-item? use-item?
                          :pour  (liquid/bucket->state item)}))))

(defn- sequence-of [tag args]
  (case tag
    :dig (when (#{0 1 2} (long (first args))) (nth args 3 nil))
    :place (nth args 4 nil)
    nil))

(defn- acted-at [world eid origin pos]
  (reach/in-reach? (merge (get-in world [:entities eid]) origin) pos))

(defn- use-ack-deltas [world events origins]
  (mapcat (fn [[i [tag eid pos face]]]
            (when-let [off (and (= :place tag) (dir/face-offset (bit-and (long face) 0xFF)))]
              (when (and (chunk/in-range? (nth pos 1))
                         (acted-at world eid (get origins i) pos))
                (let [pos' (mapv + pos off)]
                  (cond-> [(edit/own-change world eid pos)]
                          (chunk/in-range? (nth pos' 1)) (conj (edit/own-change world eid pos')))))))
          (map-indexed vector events)))

(defn ack-deltas [world events]
  (let [latest (reduce (fn [m [tag eid & args]]
                         (if-let [sq (sequence-of tag args)]
                           (update m eid (fnil max -1) (long sq))
                           m))
                       {} events)]
    (concat (map (fn [[eid sq]] (out/to eid (out/block-ack sq))) latest)
            (use-ack-deltas world events (:use-origins world)))))

(defn- with-edits [world deltas]
  (let [changes (into [] (mapcat (fn [[tag recs]] (when (= tag :set-blocks) recs))) deltas)]
    (if (empty? changes)
      world
      (update world :chunks chunk/chunks-set-blocks gen/flat-chunk changes))))

(defn- block-edits-deltas [world events]
  (let [[_ edits] (reduce (fn [[w acc] [i [tag & args]]]
                            (let [ds (case tag
                                       :dig (dig/dig-deltas w args)
                                       :place (place-deltas w args (get-in world [:use-origins i]))
                                       :sign-update (use/sign-update-deltas w args)
                                       nil)]
                              [(with-edits w ds) (into acc ds)]))
                          [world []]
                          (map-indexed vector events))]
    edits))

(defn block-edits [world events]
  [#(block-edits-deltas world events)])
