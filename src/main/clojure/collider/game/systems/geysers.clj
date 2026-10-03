(ns collider.game.systems.geysers
  "Geysers of potent sulfur under water and what they lift."
  (:require [collider.game.changes :as changes]
            [collider.game.entity :as entity]
            [collider.game.mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.level :as level]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.blocks.geyser :as geyser]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const countdown-period 20)

(def ^:private lift (num/f32 0.2))

(def ^:private base-speed (num/f32 0.3))

(defn- launched-player? [e]
  (and (entity/alive? e) (not (game-mode/spectator? e))
       (not (:flying e))))

(defn- launched? [e]
  (let [t (:type e)]
    (cond
      (= :player t) (launched-player? e)
      (#{:item :tnt :falling-block} t) true
      (contains? entity/thrown-types t) true
      :else (and (mobs/mob-type? t) (entity/alive? e)))))

(defn- launch-size [e] (when (launched? e) (entity/box e)))

(defn- inside? [[x lo z] ^double hi e [half h]]
  (let [[ex ey ez] (:pos e)
        ex (double ex) ey (double ey) ez (double ez)
        half (double half) x (long x) z (long z)]
    (and (< (- ex half) (inc x)) (> (+ ex half) x)
         (< ey hi) (> (+ ey (double h)) (double lo))
         (< (- ez half) (inc z)) (> (+ ez half) z))))

(defn- slow? [e ^long depth]
  (< (v/y (or (:vel e) [0.0 0.0 0.0])) (+ base-speed (* depth 0.1))))

(defn- lifted [eid e]
  (cond-> [[:push eid [0.0 lift 0.0]]]
    (#{:item :player} (:type e))
    (conj [:merge-entity eid {:needs-sync? true}])))

(defn- launch-deltas
  [world [x y z] depth]
  (let [n (geyser/reach (:chunks world) [x y z] depth)
        lo [x (+ (long y) 1 (min 0 (dec n))) z]
        hi (double (+ (long y) 2 (max 0 (dec n))))]
    (for [[eid e] (sort-by key (:entities world))
          :let [s (launch-size e)]
          :when (and s (inside? lo hi e s) (slow? e depth))
          d (lifted eid e)]
      d)))

(defn- turn-deltas [world pos st]
  (changes/set-deltas world [[pos (geyser/turned st)]]))

(defn- countdown-deltas [world pos st e depth]
  (when (zero? (rem (long (:tick world)) countdown-period))
    (let [ph (geyser/phase st)
          c (geyser/counted pos ph depth (:countdown e))]
      (concat (when (not= c (:countdown e))
                [[:set-block-entity pos (assoc e :countdown c)]])
              (when (zero? c) (turn-deltas world pos st))))))

(defn tick-deltas
  "Returns the deltas of one tick of the potent sulfur e at pos."
  [world [pos e]]
  (let [st (chunk/at (:chunks world) pos)
        ph (geyser/phase st)
        q (when (#{:dormant :erupting :continuous} ph)
            (geyser/source (:chunks world) pos))
        depth (when q (geyser/water-depth pos q))]
    (when depth
      (concat
        (when (#{:erupting :continuous} ph)
          (launch-deltas world pos depth))
        (when (#{:dormant :erupting} ph)
          (countdown-deltas world pos st e depth))))))

(defn unsynced
  "Returns the deltas that end the lift of every player lifted
  before, ahead of the geysers of this tick."
  [world]
  (for [[eid e] (level/of-types world [:player])
        :when (:needs-sync? e)]
    [:merge-entity eid {:needs-sync? false}]))
