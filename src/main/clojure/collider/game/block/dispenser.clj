(ns collider.game.block.dispenser
  "A dispenser tick spits one plain item along its facing, pours
  or fills a bucket, primes tnt, sets a boat on water, or clicks
  when the dispenser is empty."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.block.boat :as boat]
            [collider.game.block.tnt :as tnt]
            [collider.game.entity :as entity]
            [collider.game.item :as item]
            [collider.game.loot :as loot]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.campfire :as campfire]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.attribute :as attribute]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn- size ^long [s] (if s (stack/size s) 0))

(defn- slot-of [items ^long t pos]
  (loop [i 0 odds 1 slot -1]
    (if (>= i (count items))
      slot
      (if (nil? (nth items i))
        (recur (inc i) odds slot)
        (let [r (random/below (random/rnd t (hash pos) :dispenser i)
                              odds)]
          (recur (inc i) (inc odds) (if (zero? r) i slot)))))))

(defn- mouth [pos facing]
  (let [c (mapv #(+ (double %) 0.5) pos)
        step (dir/offset facing)
        at (mapv + c (mapv #(* 0.7 (double %)) step))
        drop (if (#{:up :down} facing) 0.125 0.15625)]
    (update (vec at) 1 - drop)))

(def ^:private ^:const spread (* 0.0172275 6.0))

(defn- roll [t pos k]
  (random/rnd t (hash pos) k))

(defn- axis-speed [t pos k ^double centre]
  (let [other (keyword (str (name k) "-b"))]
    (random/triangle centre spread
                     (roll t pos k) (roll t pos other))))

(defn- speed [t pos facing]
  (let [step (mapv double (dir/offset facing))
        pow (+ (* (random/rnd t (hash pos) :power) 0.1) 0.2)]
    [(axis-speed t pos :vx (* (step 0) pow))
     (axis-speed t pos :vy 0.2)
     (axis-speed t pos :vz (* (step 2) pow))]))

(defn- shrink [s]
  (let [n (dec (size s))]
    (when (pos? n) (assoc s :count n))))

(defn- shot [world pos facing stack]
  (let [t (:tick world)
        at (mouth pos facing)
        vel (speed t pos facing)]
    [(entity/item at vel stack 10)
     (out/all (out/level-event :sound-dispenser-dispense pos 0))
     (out/all (out/level-event :particles-shoot-smoke pos
                               (dir/index facing)))]))

(defn- empty-shot [pos]
  (concat [(out/all (out/level-event :sound-dispenser-fail pos 0))]
          (signal/game-event :block-activate pos nil)))

(defn- shot-deltas [world pos facing e items i]
  (let [s (nth items i)
        left (shrink s)
        one (assoc s :count 1)
        [spawn sound smoke] (shot world pos facing one)
        e (assoc e :items (assoc items i left))]
    [[:set-block-entity pos e] [:spawn-entity spawn] sound smoke]))

(defn- ahead [pos facing]
  (mapv + pos (dir/offset facing)))

(defn- bucket [item] {:item item :count 1})

(defn- takes? [st water?]
  (or (zero? (long st))
      (block/can-be-replaced? st)
      (not (block/blocks-motion? st))
      (and water? (= :false (:waterlogged (block/props-of st))))))

(defn- fizz [world pos]
  (let [roll #(random/of-key (:tick world) pos %)]
    (random/triangle 2.6 0.8 (roll :fizz-a) (roll :fizz-b))))

(defn- pour-fx [pos water?]
  (let [snd (if water? :bucket/empty :bucket/empty-lava)]
    (conj (signal/game-event :fluid-place pos nil)
          (out/all (out/block-sound snd pos 1.0 1.0)))))

(defn- here [world pos]
  (chunk/at (:chunks world) pos))

(defn- broken [world pos cur]
  (when (and (pos? (long cur)) (not (block/liquid? cur))
             (get-in world [:rules :block-drops] true))
    (map-indexed
      (fn [i stack]
        [:spawn-entity (item/popped world pos stack [:bucket i])])
      (loot/block-drops {:state cur :pos pos
                         :block-entity (be/at world pos)
                         :block-at #(chunk/at (:chunks world) %)}
                        #(random/of-key (:tick world) pos %)))))

(defn- evaporates [world pos]
  {:fx [(out/all (out/block-sound :block.fire.extinguish pos 0.5
                                  (fizz world pos)))]
   :edits []})

(defn- logged [pos cur]
  {:fx (pour-fx pos true)
   :edits [[pos (block/with-water cur) [:fluid-tick]]]})

(defn- drowned [pos cur]
  {:fx (pour-fx pos true)
   :edits [[pos (campfire/drowned cur) [:fluid-tick]]]})

(defn- source [world pos state cur water?]
  {:fx (into (vec (broken world pos cur)) (pour-fx pos water?))
   :edits [[pos state]]})

(defn- place-at [world pos state]
  (let [cur (here world pos)
        water? (block/water? state)]
    (when (takes? cur water?)
      (cond
        (and water? (attribute/water-evaporates? (:dim world)))
        (evaporates world pos)
        (and water? (campfire/drowned cur)) (drowned pos cur)
        (and water? (= :false (:waterlogged (block/props-of cur))))
        (logged pos cur)
        :else (source world pos state cur water?)))))

(defn- fill-of [st]
  (cond
    (liquid/bubble-column? st) :water-bucket
    (and (block/source-state? st) (block/water? st)) :water-bucket
    (and (block/source-state? st) (block/lava? st)) :lava-bucket
    (= :true (:waterlogged (block/props-of st))) :water-bucket))

(defn- cleared [st]
  (if (or (block/source-state? st) (liquid/bubble-column? st))
    0
    (block/without-water st)))

(defn- fill-fx [pos st]
  (let [snd (if (block/lava? st) :bucket/fill-lava :bucket/fill)]
    (conj (signal/game-event :fluid-pickup pos nil)
          (out/all (out/block-sound snd pos 1.0 1.0)))))

(defn- taken-at [world pos]
  (let [st (here world pos)]
    (when-let [item (fill-of st)]
      {:item item
       :fx (fill-fx pos st)
       :edits [[pos (cleared st)]]})))

(defn- opened [stack result]
  (if (<= (size stack) 1)
    [result nil]
    [(assoc stack :count (dec (size stack))) result]))

(defn- fit [items stack]
  (let [n (stack/max-size stack)
        same (fn [i s]
               (when (and s (= (:item s) (:item stack))
                          (< (size s) n))
                 i))
        hole (fn [i s] (when (nil? s) i))
        i (or (first (keep-indexed same items))
              (first (keep-indexed hole items)))]
    (if (nil? i)
      [items stack]
      [(assoc items i (if (nth items i)
                        (update (nth items i) :count + (size stack))
                        stack))
       nil])))

(defn- kept [world pos facing e items i left extra]
  (let [[items spat] (if extra (fit items extra) [items nil])
        be [:set-block-entity pos (assoc e :items (assoc items i left))]]
    (if spat
      (let [[spawn sound smoke] (shot world pos facing spat)]
        [be [:spawn-entity spawn] sound smoke])
      [be])))

(defn- report [pos facing sound]
  [(out/all (out/level-event sound pos 0))
   (out/all (out/level-event :particles-shoot-smoke pos
                             (dir/index facing)))])

(defn- primed-at [world there seed]
  (let [e (dissoc (tnt/primed there seed) :origin)]
    [[:spawn-entity e]
     (out/all (out/sound :tnt/primed (:pos e) 1.0 1.0))
     [:game-event :entity-place there nil]]))

(defn- tnt-fired [world pos facing e items i stack]
  (when (= :tnt (:item stack))
    (if-not (get-in world [:rules :tnt-explodes] true)
      [(vec (report pos facing :sound-dispenser-fail)) []]
      (let [there (ahead pos facing)
            left (shrink stack)
            e (assoc e :items (assoc items i left))
            born (primed-at world there [(:tick world) pos])
            fx (report pos facing :sound-dispenser-dispense)]
        [(into [[:set-block-entity pos e]] (concat born fx)) []]))))

(defn- with-bucket [placed world pos facing e items i left extra]
  [(into (vec (:fx placed))
         (kept world pos facing e items i left extra))
   (:edits placed)])

(defn- liquid-fired [world pos facing e items i stack]
  (let [there (ahead pos facing)
        state (liquid/bucket->state (:item stack))]
    (cond
      state
      (when-let [placed (place-at world there state)]
        (let [[left extra] (opened stack (bucket :bucket))]
          (with-bucket placed world pos facing e items i left extra)))
      (= :bucket (:item stack))
      (when-let [placed (taken-at world there)]
        (let [[left extra] (opened stack (bucket (:item placed)))]
          (with-bucket placed world pos facing e items i left extra))))))

(defn- boat-fired [world pos facing e items i stack]
  (when (entity/boat? (:item stack))
    (if-let [born (boat/entity (:chunks world) pos facing
                               (:item stack))]
      (let [box (assoc e :items (assoc items i (shrink stack)))]
        [(into [[:set-block-entity pos box] [:spawn-entity born]]
               (report pos facing :sound-dispenser-dispense))
         []])
      [(into (shot-deltas world pos facing e items i)
             (report pos facing :sound-dispenser-dispense))
       []])))

(defn- chosen [world pos facing e items i]
  (let [stack (nth items i)]
    (or (boat-fired world pos facing e items i stack)
        (tnt-fired world pos facing e items i stack)
        (liquid-fired world pos facing e items i stack)
        [(shot-deltas world pos facing e items i) []])))

(defn fired
  "Returns [deltas edits] of dispenser at pos firing once.
  edits are the block changes of a bucket it pours or fills."
  [world pos]
  (let [found (be/at world pos)
        e (or found (be/fresh :dispenser nil))
        items (vec (:items e))
        facing (block/facing-of (chunk/at (:chunks world) pos))
        i (slot-of items (:tick world) pos)
        [ds edits]
        (if (neg? i)
          [(vec (empty-shot pos)) []]
          (chosen world pos facing e items i))]
    (if found
      [ds edits]
      [(into [[:set-block-entity pos e]] ds) edits])))

(defn picked
  "Returns the slot items of a launcher at pos gives up at tick t,
  or -1 when every slot is empty."
  [items t pos]
  (slot-of items t pos))

(defn thrown
  "Returns the deltas of launcher e at pos throwing slot i along
  facing. The stack leaves as one item."
  [world pos facing e items i]
  (shot-deltas world pos facing e items i))
