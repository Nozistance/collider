(ns collider.world.blocks.grow.vine
  "Vines, and the plants that grow along one direction."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.grow.common
             :refer [age aged air-at? chance? crowd pick with]]
            [collider.world.blocks.multiface :as multiface]))

(set! *warn-on-reflection* true)

(def ^:private sides [:north :south :west :east])

(def ^:private ^:const max-age 25)

(defn- grow-into ^long [^long st ^long a roll]
  (let [st' (aged st a)]
    (if (= :cave-vines (block/type-of st))
      (with st' :berries
            (if (< (double (roll :berries)) 0.11) :true :false))
      st')))

(defn- growth-offset [st]
  (dir/offset (:dir (block/growing-plant (block/type-of st)))))

(defn plant-tick
  "Returns the cell a growing plant head at p grows into, if any."
  [chunks p st roll _time _ctx]
  (let [q (mapv + p (growth-offset st))]
    (when (and (< (age st) max-age)
               (< (double (roll :grow)) 0.1)
               (air-at? chunks q))
      [[q (grow-into st (inc (age st)) roll)]])))

(defn- vine-with [st dir] (with st dir :true))

(defn- vine-has? [st dir] (= :true (get (block/props-of st) dir)))

(defn- crowded? [chunks p self] (>= (crowd chunks p self) 5))

(defn- fresh-with [st dir]
  (vine-with (block/state (block/block-of st)) dir))

(defn- onto [chunks st test turn]
  (when (and (vine-has? st turn)
             (multiface/attaches? chunks test turn))
    [[test (fresh-with st turn)]]))

(defn- wrapped [chunks st test turn back]
  (let [corner (mapv + test (dir/offset turn))]
    (when (and (vine-has? st turn) (air-at? chunks corner)
               (multiface/attaches? chunks corner back))
      [[corner (fresh-with st back)]])))

(defn- into-air [chunks st test dir roll]
  (let [turns [(dir/clockwise dir) (dir/counter-clockwise dir)]
        back (dir/opposite dir)]
    (or (some #(onto chunks st test %) turns)
        (some #(wrapped chunks st test % back) turns)
        (when (and (< (double (roll :up-wall)) 0.05)
                   (multiface/attaches? chunks test :up))
          [[test (fresh-with st :up)]]))))

(defn- sideways [chunks p st dir roll]
  (let [test (mapv + p (dir/offset dir))]
    (if (air-at? chunks test)
      (into-air chunks st test dir roll)
      (when (multiface/attaches? chunks p dir)
        [[p (vine-with st dir)]]))))

(defn- climbs? [chunks p]
  (or (multiface/attaches? chunks p :up)
      (air-at? chunks (dir/up p))))

(defn- kept-side [chunks above roll s dir]
  (if (or (< (double (roll [:keep dir])) 0.5)
          (not (multiface/attaches? chunks above dir)))
    (with s dir :false)
    s))

(defn- upward [chunks p st roll]
  (let [above (dir/up p)]
    (cond
      (multiface/attaches? chunks p :up) [[p (vine-with st :up)]]
      (crowded? chunks p (block/block-of st)) nil
      :else
      (let [keep-side #(kept-side chunks above roll %1 %2)
            st' (reduce keep-side st sides)]
        (when (some #(vine-has? st' %) sides) [[above st']])))))

(defn- copied-side [st roll s dir]
  (if (and (< (double (roll [:copy dir])) 0.5) (vine-has? st dir))
    (vine-with s dir)
    s))

(defn- downward [chunks p st roll]
  (let [below (dir/down p) bst (chunk/at chunks below)
        self (block/block-of st)]
    (when (or (zero? bst) (= (block/block-of bst) self))
      (let [before (if (zero? bst) (block/state self) bst)
            after (reduce #(copied-side st roll %1 %2) before sides)]
        (when (and (not= after before)
                   (some #(vine-has? after %) sides))
          [[below after]])))))

(defn tick
  "VineBlock.randomTick: the cells a vine at p grows into."
  [chunks p st roll _time ctx]
  (when (and (get-in ctx [:rules :spread-vines] true)
             (chance? roll :gate 4))
    (let [dir (dir/six (pick roll :dir 6))]
      (cond
        (and (contains? dir/horizontal-offset dir)
             (not (vine-has? st dir)))
        (when-not (crowded? chunks p (block/block-of st))
          (sideways chunks p st dir roll))
        (and (= :up dir) (climbs? chunks p)) (upward chunks p st roll)
        :else (downward chunks p st roll)))))

(defn- nether-count ^long [roll]
  (loop [p 1.0 n 0]
    (if (and (< n 25) (< (double (roll [:count n])) p))
      (recur (* p 0.826) (inc n))
      n)))

(defn- head-pos [chunks p st]
  (let [plant (block/growing-plant (block/type-of st))
        {:keys [head body dir]} plant
        off (dir/offset dir)]
    (loop [q p n 0]
      (let [nq (mapv + q off) b (block/block-of (chunk/at chunks nq))]
        (cond
          (= head b) nq
          (and (= body b) (< n 256)) (recur nq (inc n))
          :else nil)))))

(defn plant-meal
  "Returns the cells bone meal grows a plant head at p into."
  [chunks p st roll]
  (let [off (growth-offset st)
        cave? (= :cave-vines (block/type-of st))
        n (if cave? 1 (nether-count roll))]
    (loop [q (mapv + p off) a (min max-age (inc (age st)))
           left n acc []]
      (if (and (pos? left) (air-at? chunks q))
        (recur (mapv + q off) (min max-age (inc a)) (dec left)
               (conj acc [q (aged st a)]))
        (when (seq acc) {:changes acc})))))

(defn body-meal
  "Returns what bone meal on a plant body at p grows at its head."
  [chunks p st roll]
  (when-let [h (head-pos chunks p st)]
    (plant-meal chunks h (chunk/at chunks h) roll)))

(defn berries-meal
  "Returns the cave vine at p with berries, when it has none."
  [_chunks p st _roll]
  (when (= :false (:berries (block/props-of st)))
    {:changes [[p (with st :berries :true)]]}))
