(ns collider.game.systems.blocks
  "Player block actions such as digging, placing and using."
  (:require [collider.data :as data]
            [collider.game.book :as book]
            [collider.game.deltas :as deltas]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.game-mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.apply :as apply]
            [collider.game.player :as player]
            [collider.game.systems.blocks.bed :as bed]
            [collider.game.systems.blocks.bucket :as bucket]
            [collider.game.systems.blocks.dig :as dig]
            [collider.game.systems.blocks.door :as door]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.held :as held]
            [collider.game.systems.blocks.place :as place]
            [collider.game.systems.blocks.reach :as reach]
            [collider.game.systems.blocks.tools :as tools]
            [collider.game.systems.blocks.use :as use]
            [collider.game.systems.consume :as consume]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.hanging :as hanging]
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

(defn- equip-deltas [world eid e held]
  (held/equip-deltas world eid e (:use-hand e) held))

(defn- held-deltas [f]
  (fn [{:keys [world eid at]}]
    (f world eid at (player/hand-stack at (:use-hand at)))))

(defn- egg-deltas [{:keys [world eid at item use-item?] :as c}]
  (if use-item?
    (tools/fluid-egg-deltas world eid at item)
    (tools/spawn-egg-deltas world (:args c))))

(defn- read-deltas [{:keys [eid at]}]
  (let [hand (:use-hand at)]
    (book/open-deltas at eid hand (player/hand-slot at hand))))

(def ^:private item-actions
  [[clicked-scaffolding?
    (fn [{:keys [world eid pos face]}]
      (place/scaffold-place-deltas world eid pos face))]
   [(comp nil? :item) (constantly nil)]
   [(comp projectiles/throwables :item)
    (when-use (on-at projectiles/throw-deltas))]
   [:pour
    (when-use (fn [{:keys [world eid at item pour]}]
                (bucket/add world eid at item pour)))]
   [(comp data/mob-bucket :item)
    (when-use (fn [{:keys [world eid at item]}]
                (bucket/mob-deltas world eid at item)))]
   [(item-is :flint-and-steel)
    (when-hand (on-args tools/flint-deltas))]
   [(item-is :fire-charge)
    (when-hand (on-args tools/firecharge-deltas))]
   [(item-is :bucket) (when-use (on-at bucket/scoop-deltas))]
   [(item-is :written-book) (when-use read-deltas)]
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
   [(item-is :shears) (when-hand (on-args tools/shear-deltas))]
   [(item-is :compass) (when-hand (on-args tools/compass-deltas))]
   [(comp mobs/egg-type :item) egg-deltas]
   [(comp #{:painting :item-frame :glow-item-frame} :item)
    (when-hand (fn [{:keys [world eid at pos face]}]
                 (hanging/place-deltas world eid at pos face)))]
   [(item-is :spyglass)
    (when-use (fn [{:keys [eid at]}] (held/spyglass-deltas eid at)))]
   [(item-is :goat-horn) (when-use (held-deltas held/horn-deltas))]
   [(comp held/swap-slot :item) (when-use (held-deltas equip-deltas))]
   [(constantly true) (on-args place/solid-place-deltas)]])

(defn- fresh? [{:keys [at item world]}]
  (not (player/on-cooldown? at item (:tick world))))

(defn- item-deltas [ctx]
  (when (fresh? ctx)
    (let [acts (filter (fn [[pred _]] (pred ctx)) item-actions)]
      (when-let [[_ f] (first acts)] (f ctx)))))

(defn- suppressed?
  "Whether a sneaking player with something in either hand, item in
  the hand that clicks, skips the use of the block clicked."
  [at item]
  (boolean (and (:sneaking? at)
                (or item (seq (sense/hands-of at))))))

(defn- place-ctx [world at [eid pos face item cursor :as args]]
  (let [use-item? (= 255 (bit-and (long face) 0xFF))]
    {:world world :eid eid :pos pos :face face :item item :at at
     :args args :cursor cursor :use-item? use-item?
     :use-block? (not (or use-item? (suppressed? at item)))
     :pour (liquid/bucket->state item)}))

(defn- hand-deltas [ctx]
  (let [{:keys [world eid pos face item cursor use-block?]} ctx]
    (when use-block?
      (use/deltas world eid pos face item cursor))))

(defn- without-item-deltas [{:keys [world eid pos use-block?]}]
  (when use-block?
    (cond
      (door/opens? world pos)
      (door/toggle-deltas world eid pos (edit/block-at world pos))
      (bed/uses-bed? world pos) (bed/sleep-deltas world eid pos))))

(defn- play-deltas [world [eid pos face item cursor _ _ hand] origin]
  (let [hand (or hand :main)
        e (get-in world [:entities eid])
        item (or item (sense/in-hand e hand))
        at (assoc (merge e origin) :use-hand hand)
        world (assoc-in world [:entities eid] at)
        ctx (place-ctx world at [eid pos face item cursor])]
    (if (= :main hand)
      (or (without-item-deltas ctx) (hand-deltas ctx)
          (item-deltas ctx))
      (when item (or (hand-deltas ctx) (item-deltas ctx))))))

(defn- spectator-deltas [world eid pos face]
  (when-not (= 255 (bit-and (long face) 0xFF))
    (seq (containers/spectator-open-deltas world eid pos))))

(defn- place-deltas [world [eid pos face :as args] origin]
  (if (game-mode/spectator? (get-in world [:entities eid]))
    (spectator-deltas world eid pos face)
    (play-deltas world args origin)))

(defn- placement-attempt? [world e item]
  (and item
       (or (block/item->block item 1) (liquid/bucket->state item)
           (data/mob-bucket item))
       (not (player/on-cooldown? e item (:tick world)))))

(defn- consumed? [deltas]
  (some (fn [[tag m]]
          (not (and (= :fx tag)
                    (#{:blocks-changed :overlay} (:msg m)))))
        deltas))

(defn- on-block? [cursor]
  (every? #(< (Math/abs (- (/ (double %) 16.0) 0.5)) 1.0000001)
          cursor))

(defn- barred [world e pos]
  (let [y (long (nth pos 1))
        top (chunk/level-max-y world)
        bottom (chunk/level-min-y world)]
    (cond
      (> y top) [true top]
      (< y bottom) [false bottom]
      (:tp-target e) [true top])))

(defn- failed-limits
  [world e [eid pos face item _ _ _ hand] deltas]
  (let [y (long (nth pos 1))
        top (chunk/level-max-y world)
        bottom (chunk/level-min-y world)
        item (or item (sense/in-hand e (or hand :main)))]
    (when-not (or (consumed? deltas)
                  (not (placement-attempt? world e item)))
      (cond
        (and (= 1 face) (>= y top))
        (repeat 2 (edit/build-limit eid true top))
        (and (= 0 face) (<= y bottom))
        [(edit/build-limit eid false bottom)]))))

(defn- use-on-deltas
  [world [eid pos face _ cursor :as args] origin]
  (let [e (merge (get-in world [:entities eid]) origin)]
    (cond
      (= 255 (bit-and (long face) 0xFF))
      (place-deltas world args origin)
      (and (reach/in-reach? e pos) (on-block? cursor))
      (if-let [[high? y] (barred world e pos)]
        [(edit/build-limit eid high? y)]
        (let [r (place-deltas world args origin)]
          (concat r (failed-limits world e args r)))))))

(defn- sequence-of [tag args]
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

(defn- ack-offset [world origin [eid pos face _ cursor]]
  (when-let [off (dir/face-offset (bit-and (long face) 0xFF))]
    (when (and (chunk/in-level? world (nth pos 1))
               (acted-at world eid origin pos)
               (on-block? cursor))
      off)))

(defn- placed-deltas [world [eid pos :as args] origin]
  (let [ds (vec (use-on-deltas world args origin))]
    (if-let [off (ack-offset world origin args)]
      (let [w (first (apply/deltas world ds))]
        (into ds (ack-changes w eid pos off)))
      ds)))

(defn sequences
  "Returns the last block action sequence of each player in events,
  as a map of eid to sequence."
  [events]
  (reduce (fn [m [tag eid & args]]
            (if-let [sq (sequence-of tag args)]
              (update m eid (fnil max -1) (long sq))
              m))
          {} events))

(defn- edit-deltas [world i [tag & args] origins]
  (case tag
    :dig (dig/dig-deltas world args)
    :place (placed-deltas world args (get origins i))
    :use-item (let [[eid hand] args
                    a [eid [-1 -1 -1] 255 nil [0 0 0] nil nil hand]]
                (place-deltas world a (get origins i)))
    :sign-update (use/sign-update-deltas world args)
    nil))

(defn- block-edits-deltas [world events]
  (let [origins (get-in world [:input :use-origins])]
    (apply/fold-events world (map-indexed vector events)
                       (fn [w [i ev]] (edit-deltas w i ev origins))
                       second)))

(defn block-edits
  "Returns the deltas of the digs, places and sign edits of the tick."
  {:wake {:events #{:dig :place :use-item :sign-update}}}
  [world d]
  (let [events (:input d)]
    (deltas/of-vec (block-edits-deltas world events))))
