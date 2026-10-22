(ns collider.game.block.dispenser
  "A dispenser tick spits one plain item along its facing, or
  clicks when the dispenser is empty."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
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

(defn- held [world pos]
  (or (be/at world pos)
      (throw (ex-info "a dispenser has no block entity" {:pos pos}))))

(defn- shot-deltas [world pos facing e items i]
  (let [s (nth items i)
        left (shrink s)
        one (assoc s :count 1)
        [spawn sound smoke] (shot world pos facing one)
        e (assoc e :items (assoc items i left))]
    [[:set-block-entity pos e] [:spawn-entity spawn] sound smoke]))

(defn fired-deltas
  "Returns the deltas of dispenser at pos firing once."
  [world pos]
  (let [e (held world pos)
        items (vec (:items e))
        facing (block/facing-of (chunk/at (:chunks world) pos))
        i (slot-of items (:tick world) pos)]
    (if (neg? i)
      (vec (empty-shot pos))
      (shot-deltas world pos facing e items i))))
