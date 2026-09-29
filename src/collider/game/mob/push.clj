(ns collider.game.mob.push
  "Shoves between overlapping bodies."
  (:require [collider.game.game-mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.vec :as v]
            [collider.world.chunk :as chunk])
  (:import (collider.game.mob Islands PushGrid)))

(set! *warn-on-reflection* true)

(def ^:private ^:const player-half (double (float 0.3)))

(def ^:private ^:const player-height (double (float 1.8)))

(defn- pushable-half ^double [e]
  (case (:type e)
    :player player-half
    (double (or (first (mobs/box-of e)) 0.0))))

(defn- pushable-height ^double [e]
  (case (:type e)
    :player player-height
    (double (or (second (mobs/box-of e)) 1.0))))

(defn- cell-key ^long [^long cx ^long cz]
  (bit-or (bit-shift-left (bit-and cx 0xFFFFFFFF) 32)
          (bit-and cz 0xFFFFFFFF)))

(defn- cell-of ^long [^double x ^double z]
  (cell-key (bit-shift-right (long (Math/floor x)) 2)
            (bit-shift-right (long (Math/floor z)) 2)))

(defn alive?
  "Returns true when a body takes shoves.
  A dead body takes none but still steps and shoves the living."
  [e]
  (pos? (double (:health e 1.0))))

(defn- body?
  [held [_ e]]
  (and (or (= :player (:type e)) (mobs/mob-type? (:type e)))
       (not (game-mode/spectator? e))
       (contains? held (chunk/pos-chunk (:pos e)))))

(defn- pushable?
  [held [_ e :as entry]]
  (and (body? held entry) (alive? e)))

(defn- filled ^PushGrid [es]
  (let [n (count es)
        eids (long-array n) xs (double-array n) ys (double-array n)
        zs (double-array n) hs (double-array n) ts (double-array n)]
    (dotimes [i n]
      (let [[eid e] (nth es i) p (:pos e)]
        (aset eids i (long eid))
        (aset xs i (double (v/x p)))
        (aset ys i (double (v/y p)))
        (aset zs i (double (v/z p)))
        (aset hs i (pushable-half e))
        (aset ts i (pushable-height e))))
    (PushGrid. eids hs ts xs ys zs)))

(defn index-of
  "Returns the index in which a body finds every body near enough to
  shove it. Each move of a body changes it in place."
  ^PushGrid [entries]
  (filled (vec (sort-by first entries))))

(defn moved
  "Returns index after body eid moved to its place in entry e."
  [^PushGrid index eid _old-pos e]
  (let [p (:pos e)]
    (PushGrid/moved index (long eid) (pushable-half e)
                    (pushable-height e) (double (v/x p))
                    (double (v/y p)) (double (v/z p)))))

(defn- grouped [entries]
  (let [n (count entries)
        eids (long-array n) cells (long-array n)]
    (dotimes [i n]
      (let [[eid e] (nth entries i) p (:pos e)]
        (aset eids i (long eid))
        (aset cells i (cell-of (double (v/x p)) (double (v/z p))))))
    (Islands/of eids cells)))

(defn islands
  "Returns the pushable bodies in groups that one tick of movement
  cannot bring together. Each group steps on its own."
  [world held]
  (let [entries (into [] (filter #(body? held %)) (:entities world))
        group (fn [^ints g] (mapv #(nth entries %) g))]
    (mapv group (grouped entries))))

(defn- scan [^PushGrid index eid p half height hi]
  (if index
    (PushGrid/shoves index (double (v/x p)) (double (v/y p))
                     (double (v/z p)) (double half) (double height)
                     (long eid) (long hi))
    []))

(defn touching
  "Returns the ids of the bodies whose boxes overlap the box of body
  eid."
  [^PushGrid index eid e half height]
  (let [p (:pos e)]
    (PushGrid/touching index (double (v/x p)) (double (v/y p))
                       (double (v/z p)) (double half) (double height)
                       (long eid))))

(defn shoves
  "Returns the shoves of one run of the body over every body it meets.
  The run is the last part of its tick."
  [index eid e half height]
  (scan index eid (:pos e) half height Long/MAX_VALUE))

(defn shoves-at
  "Returns the shoves of body eid run to pos, as shoves does."
  [index eid pos half height]
  (scan index eid pos half height Long/MAX_VALUE))

(defn before
  "Returns the shoves that the bodies which stepped earlier this tick
  gave to this body. This body takes the opposite of what each of
  them took."
  [index eid e half height]
  (scan index eid (:pos e) half height (long eid)))
