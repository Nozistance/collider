(ns collider.world.blocks.grow.vine
  "Vines, and the plants that grow along one direction."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.grow.common
             :refer [age aged air-at? chance? crowd]]
            [collider.world.blocks.multiface :as multiface
             :refer [has-face?]]))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-age 25)

(defn- hung-from-above? [chunks pos st side]
  (let [a (chunk/at chunks (dir/up pos))]
    (and (= (block/block-of a) (block/block-of st))
         (has-face? a side))))

(defn face-held?
  "Returns true when the face on side of the vine st at pos is held."
  [chunks pos st side]
  (and (not= :down side)
       (or (multiface/attaches? chunks pos side)
           (and (contains? dir/horizontal-offset side)
                (hung-from-above? chunks pos st side)))))

(defn- face-kept [chunks pos st m side]
  (if (= :true (get m side))
    (let [held? (if (= :up side)
                  (multiface/face-held? chunks (dir/up pos) :up)
                  (face-held? chunks pos st side))]
      (assoc m side (block/flag held?)))
    m))

(defn updated
  "Returns the vine st without the faces nothing holds, or what is
  left of it when no face is left."
  ^long [chunks pos ^long st]
  (let [step (fn [m side] (face-kept chunks pos st m side))
        props' (reduce step (block/props-of st)
                       [:up :north :south :west :east])
        st' (block/state (block/block-of st) props')]
    (if (seq (block/faces-of st')) st' (block/emptied st))))

(defn- grow-into ^long [^long st ^long a roll]
  (let [st' (aged st a)]
    (if (= :cave-vines (block/type-of st))
      (block/with st' :berries
                  (block/flag (< (double (roll :berries)) 0.11)))
      st')))

(defn- growth-offset [st]
  (dir/offset (:dir (block/growing-plant (block/type-of st)))))

(defn plant-tick
  "Returns the cell a growing plant head at p grows into, if any."
  [chunks p st roll _time _world]
  (let [q (mapv + p (growth-offset st))]
    (when (and (< (age st) max-age)
               (< (double (roll :grow)) 0.1)
               (air-at? chunks q))
      [[q (grow-into st (inc (age st)) roll)]])))

(defn- vine-with [st side] (block/with st side :true))

(defn- crowded? [chunks p self] (>= (crowd chunks p self) 5))

(defn- fresh-with [st side]
  (vine-with (block/state (block/block-of st)) side))

(defn- onto [chunks st test turn]
  (when (and (has-face? st turn)
             (multiface/attaches? chunks test turn))
    [[test (fresh-with st turn)]]))

(defn- wrapped [chunks st test turn back]
  (let [corner (dir/toward test turn)]
    (when (and (has-face? st turn) (air-at? chunks corner)
               (multiface/attaches? chunks corner back))
      [[corner (fresh-with st back)]])))

(defn- into-air [chunks st test side roll]
  (let [turns [(dir/clockwise side) (dir/counter-clockwise side)]
        back (dir/opposite side)]
    (or (some #(onto chunks st test %) turns)
        (some #(wrapped chunks st test % back) turns)
        (when (and (< (double (roll :up-wall)) 0.05)
                   (multiface/attaches? chunks test :up))
          [[test (fresh-with st :up)]]))))

(defn- sideways [chunks p st side roll]
  (let [test (dir/toward p side)]
    (if (air-at? chunks test)
      (into-air chunks st test side roll)
      (when (multiface/attaches? chunks p side)
        [[p (vine-with st side)]]))))

(defn- climbs? [chunks p]
  (or (multiface/attaches? chunks p :up)
      (air-at? chunks (dir/up p))))

(defn- kept-side [chunks above roll s side]
  (if (or (< (double (roll [:keep side])) 0.5)
          (not (multiface/attaches? chunks above side)))
    (block/with s side :false)
    s))

(defn- upward [chunks p st roll]
  (let [above (dir/up p)]
    (cond
      (multiface/attaches? chunks p :up) [[p (vine-with st :up)]]
      (crowded? chunks p (block/block-of st)) nil
      :else
      (let [keep-side #(kept-side chunks above roll %1 %2)
            st' (reduce keep-side st dir/horizontal)]
        (when (some #(has-face? st' %) dir/horizontal)
          [[above st']])))))

(defn- copied-side [st roll s side]
  (if (and (< (double (roll [:copy side])) 0.5) (has-face? st side))
    (vine-with s side)
    s))

(defn- downward [chunks p st roll]
  (let [below (dir/down p) bst (chunk/at chunks below)
        self (block/block-of st)]
    (when (or (zero? bst) (= (block/block-of bst) self))
      (let [before (if (zero? bst) (block/state self) bst)
            copy #(copied-side st roll %1 %2)
            after (reduce copy before dir/horizontal)]
        (when (and (not= after before)
                   (some #(has-face? after %) dir/horizontal))
          [[below after]])))))

(defn- grown [chunks p st roll side]
  (cond
    (and (contains? dir/horizontal-offset side)
         (not (has-face? st side)))
    (when-not (crowded? chunks p (block/block-of st))
      (sideways chunks p st side roll))
    (and (= :up side) (climbs? chunks p)) (upward chunks p st roll)
    :else (downward chunks p st roll)))

(defn tick
  "Returns the cells a vine at p grows into on a random tick."
  [chunks p st roll _time world]
  (when (and (get-in world [:rules :spread-vines] true)
             (chance? roll :gate 4))
    (grown chunks p st roll (dir/six (random/below (roll :dir) 6)))))

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
  "Returns the cells bone meal grows a weeping or twisting vine head
  at p into."
  [chunks p st roll]
  (let [off (growth-offset st)]
    (loop [q (mapv + p off) a (min max-age (inc (age st)))
           left (nether-count roll) acc []]
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
    {:changes [[p (block/with st :berries :true)]]}))
