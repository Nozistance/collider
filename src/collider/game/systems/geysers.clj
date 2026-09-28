(ns collider.game.systems.geysers
  "Geysers of potent sulfur under water and what they lift."
  (:require [collider.game.entity :as entity]
            [collider.game.game-mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.state :as state]
            [collider.game.systems.blocks.edit :as edit]
            [collider.vec :as v]
            [collider.world.blocks.geyser :as geyser]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const countdown-period 20)

(def ^:private lift (double (float 0.2)))

(def ^:private base-speed (double (float 0.3)))

(defn- alive? [e] (pos? (double (:health e 1.0))))

(def ^:private player-size [0.3 1.8])

(defn- launched-player? [e]
  (and (alive? e) (not (game-mode/spectator? e)) (not (:flying e))))

(defn- size [e]
  (let [t (:type e)]
    (cond
      (= :player t) (when (launched-player? e) player-size)
      (= :item t) [0.125 0.25]
      (#{:tnt :falling-block} t) [0.49 0.98]
      (contains? entity/thrown-types t) [0.125 0.25]
      (and (mobs/mob-type? t) (alive? e)) (mobs/box-of e))))

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
          :let [s (size e)]
          :when (and s (inside? lo hi e s) (slow? e depth))
          d (lifted eid e)]
      d)))

(defn- turn-deltas [world pos st]
  (edit/set-deltas world [[pos (geyser/turned st)]]))

(defn- countdown-deltas [world pos st e depth]
  (when (zero? (rem (long (:tick world)) countdown-period))
    (let [ph (geyser/phase st)
          c (geyser/counted pos ph depth (:countdown e))]
      (concat (when (not= c (:countdown e))
                [[:set-block-entity pos (assoc e :countdown c)]])
              (when (zero? c) (turn-deltas world pos st))))))

(defn- geyser-deltas [world [pos e]]
  (let [st (chunk/chunks-get-block (:chunks world) pos)
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

(defn- sulfurs [world]
  (let [active (state/active-chunks world)]
    (for [[cid entries] (:block-entities world)
          :when (contains? active cid)
          [pos e] entries
          :when (= :potent-sulfur (:kind e))]
      [pos e])))

(defn- synced [world deltas]
  (let [lifted (into #{} (keep (fn [[k eid]] (when (= :push k) eid)))
                     deltas)]
    (for [[eid e] (sort-by key (:entities world))
          :when (and (= :player (:type e)) (:needs-sync? e)
                     (not (lifted eid)))]
      [:merge-entity eid {:needs-sync? false}])))

(defn- all-deltas [world]
  (let [ds (into [] (mapcat #(geyser-deltas world %))
                 (sulfurs world))]
    (into ds (synced world ds))))

(defn geysers [world _d]
  [#(all-deltas world)])
