(ns collider.game.mob.push
  "Shoves between overlapping bodies."
  (:require [collider.game.game-mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.vec :as v]
            [collider.world.chunk :as chunk])
  (:import (collider.game.mob Islands PushGrid Turns)))

(set! *warn-on-reflection* true)

(def ^:private ^:const player-half (double (float 0.3)))

(def ^:private ^:const player-height (double (float 1.8)))

(defn- pushable-half ^double [e]
  (case (:type e)
    :player player-half
    (double (or (nth (mobs/box-of e) 0 nil) 0.0))))

(defn- pushable-height ^double [e]
  (case (:type e)
    :player player-height
    (double (or (nth (mobs/box-of e) 1 nil) 1.0))))

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
  (and (or (= :player (:type e))
           (and (mobs/mob-type? (:type e))
                (not (mobs/death-ends? e))))
       (not (game-mode/spectator? e))
       (contains? held (chunk/pos-chunk (:pos e)))))

(defn- pushable?
  [held [_ e :as entry]]
  (and (body? held entry) (alive? e)))

(defn- came ^long [e]
  (if-let [a (:arrived e)] (long (nth a 0)) Long/MIN_VALUE))

(defn- rank ^long [eid e]
  (if-let [r (nth (:arrived e) 1 nil)] (long r) (* 2 (long eid))))

(defn- arrivals [es]
  (let [n (count es) cs (long-array n) rs (long-array n)]
    (dotimes [i n]
      (let [[eid e] (nth es i)]
        (aset cs i (came e))
        (aset rs i (rank eid e))))
    [cs rs]))

(defn- filled ^PushGrid [es]
  (let [n (count es) [cs rs] (arrivals es)
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
    (PushGrid. eids hs ts xs ys zs cs rs)))

(defn- by-id? [es]
  (and (vector? es)
       (loop [i 1]
         (or (>= i (count es))
             (and (< (long (nth (nth es (dec i)) 0))
                     (long (nth (nth es i) 0)))
                  (recur (inc i)))))))

(defn index-of
  "Returns the index in which a body finds every body near enough to
  shove it. Each move of a body changes it in place."
  ^PushGrid [entries]
  (filled (if (by-id? entries)
            entries
            (vec (sort-by first entries)))))

(defn moved
  "Returns index after body eid moved to its place in entry e."
  [^PushGrid index eid _old-pos e]
  (let [p (:pos e)]
    (PushGrid/moved index (long eid) (pushable-half e)
                    (pushable-height e) (double (v/x p))
                    (double (v/y p)) (double (v/z p)) (came e)
                    (rank eid e))))

(defn- same-section? [c c']
  (== (bit-shift-right (long (Math/floor (double c))) 4)
      (bit-shift-right (long (Math/floor (double c'))) 4)))

(defn arrived
  "Returns when body e, which moves to to in tick t, came into its
  entity section, as the order of Level.getPushableEntities follows
  it: its old arrival when it stays in its section, else this tick
  after the bodies of lower id."
  [e to t eid]
  (let [from (:pos e)]
    (if (and (same-section? (v/x from) (v/x to))
             (same-section? (v/y from) (v/y to))
             (same-section? (v/z from) (v/z to)))
      (:arrived e)
      [(* 2 (long t)) (inc (* 2 (long eid)))])))

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

(defn pinned
  "Returns index, which keeps each body in the column it stands in,
  so threads may read it while bodies move no further than reach."
  ^PushGrid [^PushGrid index reach]
  (PushGrid/pinned index (double reach)))

(defn turns
  "Calls mind and then body with the index of each body of the
  pinned index, in parallel. The call of body with a body waits for
  the calls with each body of lower id that it could meet in a tick
  in which no body moves further than reach along x or z."
  [^PushGrid index reach mind body]
  (Turns/run index (double reach) mind body))

(defn within?
  "Returns true when body e, now e2, kept its box and moved no
  further than reach along x or z."
  [e e2 reach]
  (let [p (:pos e) q (:pos e2) r (double reach)]
    (and (<= (Math/abs (- (v/x q) (v/x p))) r)
         (<= (Math/abs (- (v/z q) (v/z p))) r)
         (== (pushable-half e) (pushable-half e2))
         (== (pushable-height e) (pushable-height e2)))))
