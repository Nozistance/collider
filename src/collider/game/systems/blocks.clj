(ns collider.game.systems.blocks
  "Player block actions such as digging, placing and using."
  (:require [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.blocks.bed :as bed]
            [collider.game.systems.blocks.bucket :as bucket]
            [collider.game.systems.blocks.dig :as dig]
            [collider.game.systems.blocks.door :as door]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.place :as place]
            [collider.game.systems.blocks.reach :as reach]
            [collider.game.systems.blocks.tools :as tools]
            [collider.game.systems.blocks.use :as use]
            [collider.game.systems.consume :as consume]
            [collider.game.systems.projectiles :as projectiles]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- clicked-scaffolding? [{:keys [world pos item use-item?]}]
  (and (= :scaffolding item)
       (not use-item?)
       (= :scaffolding (block/type-of (edit/block-at world pos)))))

(defn- when-use [f] (fn [c] (when (:use-item? c) (f c))))

(defn- when-hand [f] (fn [c] (when-not (:use-item? c) (f c))))

(defn- item-is [k] (comp #{k} :item))

(defn- tool-is [d] (fn [c] ((d) (:item c))))

(defn- on-args [f] (fn [{:keys [world args]}] (f world args)))

(defn- on-at [f]
  (fn [{:keys [world eid at]}] (f world eid at)))

(defn- axe-or-place [w args]
  (or (tools/axe-deltas w args) (place/solid-place-deltas w args)))

(defn- equip-deltas [{:keys [world eid item]}]
  (let [slot (tools/armor-slot-of item)]
    (tools/equip-armor-deltas world eid item slot)))

(def ^:private item-actions
  [[clicked-scaffolding?
    (fn [{:keys [world eid pos face]}]
      (place/scaffold-place-deltas world eid pos face))]
   [(comp nil? :item) (constantly nil)]
   [(comp projectiles/throwables :item)
    (on-at projectiles/throw-deltas)]
   [:pour
    (when-use (fn [{:keys [world eid at pour]}]
                (bucket/add world eid at pour)))]
   [(item-is :flint-and-steel)
    (when-hand (on-args tools/flint-deltas))]
   [(item-is :fire-charge)
    (when-hand (on-args tools/firecharge-deltas))]
   [(item-is :bucket) (when-use (on-at bucket/scoop-deltas))]
   [(item-is :glass-bottle) (when-use (on-at consume/bottle-deltas))]
   [(comp #{:lily-pad :frogspawn} :item)
    (when-use (fn [{:keys [world eid at item]}]
                (bucket/lily-deltas world eid at item)))]
   [(item-is :potion)
    (when-hand (fn [{:keys [world eid pos face]}]
                 (tools/mud-deltas world eid pos face)))]
   [(item-is :bone-meal) (when-hand (on-args tools/bonemeal-deltas))]
   [(tool-is tools/hoes) (when-hand (on-args tools/till-deltas))]
   [(item-is :honeycomb) (when-hand (on-args tools/wax-deltas))]
   [(tool-is tools/axes) (when-hand (on-args axe-or-place))]
   [(tool-is tools/shovels)
    (when-hand (on-args tools/flatten-deltas))]
   [(comp mobs/egg-type :item)
    (when-hand (on-args tools/spawn-egg-deltas))]
   [(fn [{:keys [item use-item?]}]
      (and use-item? (tools/armor-slot-of item)))
    equip-deltas]
   [(constantly true) (on-args place/solid-place-deltas)]])

(defn- fresh? [{:keys [at item world]}]
  (not (state/on-cooldown? at item (:tick world))))

(defn- item-deltas [ctx]
  (when (fresh? ctx)
    (let [acts (filter (fn [[pred _]] (pred ctx)) item-actions)]
      (when-let [[_ f] (first acts)] (f ctx)))))

(defn- place-ctx [world at [eid pos face item cursor :as args]]
  {:world     world :eid eid :pos pos :face face :item item :at at
   :args      args :cursor cursor
   :use-item? (= 255 (bit-and (long face) 0xFF))
   :pour      (liquid/bucket->state item)})

(defn- hand-deltas [ctx]
  (let [{:keys [world eid pos face item cursor at use-item?]} ctx]
    (when-not (or use-item? (and item (:sneaking? at)))
      (use/deltas world eid pos face item cursor))))

(defn- place-deltas [world [eid pos face item cursor] origin]
  (let [e (get-in world [:entities eid])
        item (or item (sense/held-of e))
        at (merge e origin)
        world (assoc-in world [:entities eid] at)
        ctx (place-ctx world at [eid pos face item cursor])
        use? (:use-item? ctx)]
    (cond
      (door/opens? world eid pos item use?)
      (door/toggle-deltas world eid pos (edit/block-at world pos))
      (bed/uses-bed? world eid pos item use?)
      (bed/sleep-deltas world eid pos)
      :else (or (hand-deltas ctx) (item-deltas ctx)))))

(def ^:private mob-buckets
  #{:pufferfish-bucket :salmon-bucket :cod-bucket
    :tropical-fish-bucket :axolotl-bucket :tadpole-bucket})

(defn- placement-attempt?
  "Tests whether item tries to put a block or a liquid.
  ServerGamePacketListenerImpl.wasBlockPlacementAttempt."
  [world e item]
  (and item
       (or (block/item->block item 1) (liquid/bucket->state item)
           (contains? mob-buckets item))
       (not (state/on-cooldown? e item (:tick world)))))

(defn- consumed? [deltas]
  (some (fn [[tag m]]
          (not (and (= :fx tag)
                    (#{:blocks-changed :overlay} (:msg m)))))
        deltas))

(defn- on-block? [cursor]
  (every? #(< (Math/abs (- (/ (double %) 16.0) 0.5)) 1.0000001)
          cursor))

(defn- barred
  "Returns [high? y] when the use stops before the block is used.
  The player then sees the limit in the message."
  [world e pos]
  (let [y (long (nth pos 1))
        top (chunk/level-max-y world)
        bottom (chunk/level-min-y world)]
    (cond
      (> y top) [true top]
      (< y bottom) [false bottom]
      (:tp-target e) [true top])))

(defn- failed-limits
  "Returns the limit messages after a use that did nothing.
  Facing up at the top, vanilla checks twice and says it twice."
  [world e [eid pos face item] deltas]
  (let [y (long (nth pos 1))
        top (chunk/level-max-y world)
        bottom (chunk/level-min-y world)
        item (or item (sense/held-of e))]
    (when-not (or (consumed? deltas)
                  (not (placement-attempt? world e item)))
      (cond
        (and (= 1 face) (>= y top))
        (repeat 2 (edit/build-limit eid true top))
        (and (= 0 face) (<= y bottom))
        [(edit/build-limit eid false bottom)]))))

(defn- use-on-deltas
  "Returns the deltas for a use on the face of a block.
  ServerGamePacketListenerImpl.handleUseItemOn."
  [world [eid pos _ _ cursor :as args] origin]
  (let [e (merge (get-in world [:entities eid]) origin)
        valid? (and (reach/in-reach? e pos) (on-block? cursor))
        [high? y :as bar] (when valid? (barred world e pos))]
    (if bar
      [(edit/build-limit eid high? y)]
      (let [r (place-deltas world args origin)]
        (concat r (when valid? (failed-limits world e args r)))))))

(defn- sequence-of
  "Returns the sequence number the player sent with the action.
  The block ack carries it back."
  [tag args]
  (case tag
    :dig (when (#{0 1 2} (long (first args))) (nth args 3 nil))
    :place (nth args 4 nil)
    :use-item (nth args 1 nil)
    nil))

(defn- acted-at [world eid origin pos]
  (reach/in-reach? (merge (get-in world [:entities eid]) origin) pos))

(defn- ack-changes [world eid pos off]
  (let [pos' (mapv + pos off)]
    (cond-> [(edit/own-change world eid pos)]
            (chunk/in-level? world (nth pos' 1))
            (conj (edit/own-change world eid pos')))))

(defn- one-ack [world origins [i [tag eid pos face]]]
  (when-let [off (and (= :place tag)
                      (dir/face-offset (bit-and (long face) 0xFF)))]
    (when (and (chunk/in-level? world (nth pos 1))
               (acted-at world eid (get origins i) pos))
      (ack-changes world eid pos off))))

(defn- use-ack-deltas [world events origins]
  (mapcat #(one-ack world origins %) (map-indexed vector events)))

(defn- latest-sequences [events]
  (reduce (fn [m [tag eid & args]]
            (if-let [sq (sequence-of tag args)]
              (update m eid (fnil max -1) (long sq))
              m))
          {} events))

(defn acks
  "Returns the deltas that confirm the block actions of this tick.
  Each player gets its last sequence, and the blocks of each place
  go back to its player."
  [world d]
  (let [events (:input d)
        ack (fn [[eid sq]] (out/to eid (out/block-ack sq)))]
    (concat (map ack (latest-sequences events))
            (use-ack-deltas world events (:use-origins world)))))

(defn- edit-deltas [world i [tag & args] origins]
  (case tag
    :dig (dig/dig-deltas world args)
    :place (use-on-deltas world args (get origins i))
    :use-item (let [a [(first args) [-1 -1 -1] 255 nil [0 0 0]]]
                (place-deltas world a (get origins i)))
    :sign-update (use/sign-update-deltas world args)
    nil))

(defn- block-edits-deltas [world events]
  (let [origins (:use-origins world)]
    (state/fold-events world (map-indexed vector events)
                       (fn [w [i ev]] (edit-deltas w i ev origins))
                       second)))

(defn block-edits
  "Returns a step for the digs, places and sign edits of this tick."
  [world d]
  (let [events (:input d)]
    [#(block-edits-deltas world events)]))
