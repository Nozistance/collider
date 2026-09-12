(ns collider.world.blocks.grow.vine
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]
            [collider.world.blocks.grow.common :refer [age aged air-at? chance? crowd pick with]]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(def ^:private sides [:north :south :west :east])

(defn- grow-into ^long [^long st ^long a roll]
  (let [st' (aged st a)]
    (if (= :cave-vines (block/type-of st))
      (with st' :berries (if (< (double (roll :berries)) 0.11) :true :false))
      st')))

(defn plant-tick [chunks p st roll _time _ctx]
  (let [q (mapv + p (dir/offset (:dir (block/growing-plant (block/type-of st)))))]
    (when (and (< (age st) 25) (< (double (roll :grow)) 0.1) (air-at? chunks q))
      [[q (grow-into st (inc (age st)) roll)]])))

(defn- vine-with [st dir] (with st dir :true))
(defn- vine-has? [st dir] (= :true (get (block/props-of st) dir)))

(defn- attachable? [chunks p dir]
  (let [n (gen/at chunks (mapv + p (dir/offset dir)))]
    (and (pos? n) (block/face-sturdy? n (dir/opposite dir)))))

(defn- face-held? [chunks p st dir]
  (support/supported? chunks gen/flat-chunk p (vine-with (block/state (block/block-of st)) dir)))

(defn- crowded? [chunks p self] (>= (crowd chunks p self) 5))

(defn- sideways [chunks p st dir roll]
  (let [self (block/block-of st) fresh (block/state self)
        test (mapv + p (dir/offset dir))
        cw (dir/clockwise dir) ccw (dir/counter-clockwise dir)
        cw? (vine-has? st cw) ccw? (vine-has? st ccw)
        cw-test (mapv + test (dir/offset cw)) ccw-test (mapv + test (dir/offset ccw))
        opp (dir/opposite dir)]
    (cond
      (not (air-at? chunks test)) (when (attachable? chunks p dir) [[p (vine-with st dir)]])
      (and cw? (attachable? chunks test cw)) [[test (vine-with fresh cw)]]
      (and ccw? (attachable? chunks test ccw)) [[test (vine-with fresh ccw)]]
      (and cw? (air-at? chunks cw-test) (attachable? chunks (mapv + p (dir/offset cw)) opp)) [[cw-test (vine-with fresh opp)]]
      (and ccw? (air-at? chunks ccw-test) (attachable? chunks (mapv + p (dir/offset ccw)) opp)) [[ccw-test (vine-with fresh opp)]]
      (and (< (double (roll :up-wall)) 0.05) (attachable? chunks (dir/up test) :up)) [[test (vine-with fresh :up)]])))

(defn- upward [chunks p st roll]
  (let [above (dir/up p)]
    (cond
      (face-held? chunks p st :up) [[p (vine-with st :up)]]
      (air-at? chunks above)
      (when-not (crowded? chunks p (block/block-of st))
        (let [st' (reduce (fn [s dir]
                            (if (or (< (double (roll [:keep dir])) 0.5) (not (attachable? chunks above dir)))
                              (with s dir :false)
                              s))
                          st sides)]
          (when (some #(vine-has? st' %) sides) [[above st']])))
      :else nil)))

(defn- downward [chunks p st roll]
  (let [below (dir/down p) bst (gen/at chunks below)]
    (when (or (zero? bst) (= (block/block-of bst) (block/block-of st)))
      (let [before (if (zero? bst) (block/state (block/block-of st)) bst)
            after (reduce (fn [s dir] (if (and (< (double (roll [:copy dir])) 0.5) (vine-has? st dir)) (vine-with s dir) s))
                          before sides)]
        (when (and (not= after before) (some #(vine-has? after %) sides))
          [[below after]])))))

(defn tick [chunks p st roll _time _ctx]
  (when (chance? roll :gate 4)
    (let [dir (dir/six (pick roll :dir 6))]
      (cond
        (and (contains? dir/horizontal-offset dir) (not (vine-has? st dir)))
        (when-not (crowded? chunks p (block/block-of st)) (sideways chunks p st dir roll))
        (= :up dir) (or (when (chunk/in-range? (inc (long (p 1)))) (upward chunks p st roll))
                        (downward chunks p st roll))
        :else (downward chunks p st roll)))))

(defn- nether-count ^long [roll]
  (loop [p 1.0 n 0]
    (if (and (< n 25) (< (double (roll [:count n])) p))
      (recur (* p 0.826) (inc n))
      n)))

(defn- head-pos [chunks p st]
  (let [{:keys [head body dir]} (block/growing-plant (block/type-of st))
        off (dir/offset dir)]
    (loop [q p n 0]
      (let [nq (mapv + q off) b (block/block-of (gen/at chunks nq))]
        (cond
          (= head b) nq
          (and (= body b) (< n 256)) (recur nq (inc n))
          :else nil)))))

(defn plant-meal [chunks p st roll]
  (let [off (dir/offset (:dir (block/growing-plant (block/type-of st))))
        n (if (= :cave-vines (block/type-of st)) 1 (nether-count roll))]
    (loop [q (mapv + p off) a (min 25 (inc (age st))) left n acc []]
      (if (and (pos? left) (air-at? chunks q))
        (recur (mapv + q off) (min 25 (inc a)) (dec left) (conj acc [q (aged st a)]))
        (when (seq acc) {:changes acc})))))

(defn body-meal [chunks p st roll]
  (when-let [h (head-pos chunks p st)] (plant-meal chunks h (gen/at chunks h) roll)))

(defn berries-meal [_chunks p st _roll]
  (when (= :false (:berries (block/props-of st)))
    {:changes [[p (with st :berries :true)]]}))
