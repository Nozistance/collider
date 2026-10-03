(ns collider.game.entity.chunks
  "The entities a player can be shown, by the chunk they stand in."
  (:require [collider.data.long-map :as lm]
            [collider.game.entity :as entity]
            [collider.game.hanging :as hanging]
            [collider.game.mob.mobs :as mobs]
            [collider.world.chunk :as chunk])
  (:import (collider.data LongMap)))

(set! *warn-on-reflection* true)

(def ^:private tracked-types
  (into #{:player :item :tnt :falling-block :area-effect-cloud
          :experience-orb}
        cat [entity/thrown-types hanging/types (keys mobs/types)]))

(defn tracked?
  "Returns true when players can be shown entity e."
  [e]
  (contains? tracked-types (:type e)))

(defn- left [idx c eid]
  (let [s (lm/disj (lm/get idx c) eid)]
    (if (lm/empty? s) (lm/dissoc idx c) (lm/assoc idx c s))))

(defn- came [idx c eid]
  (lm/assoc idx c (lm/conj (lm/get idx c (lm/long-set)) eid)))

(defn- chunk-of [e]
  (when (and e (tracked? e)) (chunk/pos-chunk (:pos e))))

(defn crossing
  "Returns [eid from to] when entity eid goes from chunk from to chunk
  to between old and new. A chunk is nil where eid is not tracked."
  [eid old new]
  (when-not (and old new
                 (identical? (:pos old) (:pos new))
                 (identical? (:type old) (:type new)))
    (let [c0 (chunk-of old) c1 (chunk-of new)]
      (when-not (= c0 c1) [eid c0 c1]))))

(defn- crossed [idx [eid c0 c1]]
  (cond-> idx
    c0 (left c0 eid)
    c1 (came c1 eid)))

(defn- caught-up [idx es now]
  (let [step (fn [idx eid a b]
               (if-let [x (crossing eid a b)] (crossed idx x) idx))]
    (lm/diff es now step idx)))

(defn- stamp [lv] (::index (meta lv)))

(defn of
  "Returns the index of entities es, built anew."
  [es]
  (reduce-kv #(crossed %1 [%2 nil (chunk-of %3)]) (lm/long-map) es))

(defn index
  "Returns the eids of the tracked entities of level lv by chunk."
  [lv]
  (let [[es idx] (stamp lv)
        now (or (:entities lv) (lm/long-map))]
    (cond
      (identical? es now) idx
      (and idx (instance? LongMap es) (instance? LongMap now))
      (caught-up idx es now)
      :else (of now))))

(defn indexed
  "Returns level lv that keeps its index for its entities."
  [lv]
  (let [now (or (:entities lv) (lm/long-map))]
    (if (identical? now (nth (stamp lv) 0 nil))
      lv
      (vary-meta lv assoc ::index [now (index lv)]))))

(defn moved
  "Returns level lv with entities es that differ from its own by the
  crossings xs alone."
  [lv es xs]
  (let [idx (reduce crossed (index lv) xs)]
    (vary-meta (assoc lv :entities es) assoc ::index [es idx])))

(defn near
  "Returns the eids of index idx that stand in the chunks seen."
  [idx seen]
  (if (< (count idx) (count seen))
    (reduce (fn [acc [c es]]
              (if (contains? seen c) (lm/union acc es) acc))
            (lm/long-set) idx)
    (reduce (fn [acc c]
              (if-let [es (lm/get idx c)] (lm/union acc es) acc))
            (lm/long-set) seen)))
