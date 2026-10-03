(ns collider.world.blocks.water
  "Coral, kelp and sponges in water."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.support :as support]
            [collider.world.direction :as dir]
            [collider.world.env.attribute :as attribute])
  (:import (clojure.lang PersistentQueue)))

(set! *warn-on-reflection* true)

(def ^:private around6 (mapv dir/offset dir/six))

(def ^:private kelp-types #{:kelp :kelp-plant})

(def ^:private plants (disj block/water-holder-types :bubble-column))

(defn coral-wet?
  "Returns true when coral st at p holds water or touches it."
  [chunks p st]
  (or (block/waterlogged? st)
      (boolean
        (some #(block/water? (chunk/at chunks (mapv + p %)))
              around6))))

(defn- held-side [st]
  (case (block/type-of st)
    (:coral-plant :coral-fan) :down
    :coral-wall-fan (dir/opposite (block/facing-of st))
    nil))

(defn- die-tick ^long [tick p]
  (let [roll (random/of-key tick p :coral)]
    (+ (long tick) 60 (long (Math/floor (* 40.0 roll))))))

(defn- coral-wake [chunks _dim tick p _old side]
  (let [st (chunk/at chunks p)]
    (cond
      (and side (= side (held-side st))
           (not (support/supported? chunks p st))) :neighbor
      (not (coral-wet? chunks p st)) (die-tick tick p))))

(defn- coral-dies [chunks p _ctx]
  (let [st (chunk/at chunks p)]
    (when-not (coral-wet? chunks p st)
      [[p (block/dead-coral st)]])))

(def coral-rule
  {:name    :coral
   :match?  (fn [_chunks st _p]
              (contains? block/coral-types (block/type-of st)))
   :wake    coral-wake
   :reshape (:due support/rule)
   :due     coral-dies})

(defn kelp?
  "Returns true when st is kelp, the head or the body."
  [st]
  (contains? kelp-types (block/type-of (long st))))

(defn kelp-still?
  "Returns true when a change on side only turns kelp st at p from
  head to body or back. Such a change needs no fluid tick."
  [chunks p st side]
  (let [above? (kelp? (chunk/at chunks (dir/up p)))]
    (cond
      (not (kelp? st)) false
      (= :kelp-plant (block/type-of (long st)))
      (and (= :up side) (not above?))
      (= :up side) above?
      (= :down side) (and above? (support/supported? chunks p st))
      :else false)))

(defn kelp-head-state
  "Returns the kelp head that grows at pos on tick, with its age."
  ^long [tick pos]
  (block/state :kelp {:age (support/plant-age tick pos)}))

(defn- above [chunks [x y z]]
  (let [y (inc (long y))]
    (if (chunk/in-range? y)
      (chunk/chunks-get-block chunks [x y z])
      0)))

(defn- kelp-grown [chunks p ctx]
  (let [st (chunk/chunks-get-block chunks p)
        up? (kelp? (above chunks p))]
    (case (block/type-of st)
      :kelp (when up? [[p (block/state :kelp-plant)]])
      :kelp-plant (when-not up?
                    [[p (kelp-head-state (:tick ctx) p)]])
      nil)))

(defn- kelp-wake [chunks _dim tick p _old side]
  (let [held? (support/supported? chunks p (chunk/at chunks p))]
    (cond
      (and (contains? #{nil :down} side) (not held?))
      (inc (long tick))
      (contains? #{nil :up} side) :neighbor)))

(def kelp-rule
  {:name    :kelp
   :match?  (fn [_chunks st _p] (kelp? st))
   :wake    kelp-wake
   :reshape kelp-grown
   :due     (:due support/rule)})

(defn- dried [st]
  (cond
    (contains? plants (block/type-of st)) 0
    (= :true (:waterlogged (block/props-of st)))
    (block/without-water st)
    (block/water? st) 0))

(defn- dried-at [chunks p]
  (let [st (chunk/at chunks p)]
    (when-let [st' (dried st)]
      (if (contains? plants (block/type-of st))
        [p st' [[:drop st]]]
        [p st']))))

(def ^:private absorb-sound [:sound :block.sponge.absorb 1.0 1.0])

(defn- soaked [pos dim]
  (if (attribute/water-evaporates? dim)
    [[pos (block/state :wet-sponge) nil 2]
     [pos (block/state :sponge) [:dry absorb-sound]]]
    [[pos (block/state :wet-sponge) [absorb-sound] 2]]))

(def ^:private ^:const max-cells 65)

(def ^:private ^:const max-depth 6)

(defn- visit [bfs p ^long d]
  (let [n (inc (long (:n bfs)))
        bfs (-> (assoc bfs :n n) (update :seen conj p))]
    (cond
      (>= n max-cells) (assoc bfs :queue [])
      (< d max-depth) (update bfs :queue into
                      (map (fn [o] [(mapv + p o) (inc d)]))
                      around6)
      :else bfs)))

(defn- searched [chunks {:keys [pos queue seen] :as bfs}]
  (if-let [[p d] (peek queue)]
    (let [bfs (assoc bfs :queue (pop queue))
          c (when-not (= p pos) (dried-at chunks p))]
      (cond
        (seen p) (recur chunks bfs)
        c [c #(searched % (visit bfs p d))]
        (= p pos) (recur chunks (visit bfs p d))
        :else (recur chunks (update bfs :seen conj p))))
    (when (> (long (:n bfs)) 1) (soaked pos (:dim bfs)))))

(defn absorbed
  "Returns the changes of a sponge at pos in dim that soaks up water.
  Each removal comes as a step that reads the level it left."
  [chunks pos dim]
  (searched chunks {:pos pos :dim dim :n 0 :seen #{}
                    :queue (conj PersistentQueue/EMPTY [pos 0])}))

(def sponge-rule
  {:name    :sponge
   :match?  (fn [_chunks st _p] (= :sponge (block/type-of st)))
   :pass    :neighbor
   :wake    (fn [_chunks _dim _tick _p _old _side] :neighbor)
   :reach   6
   :reshape (fn [chunks p ctx] (absorbed chunks p (:dim ctx)))})
