(ns collider.game.mob.push
  "The shoves that bodies which overlap hand each other."
  (:require [clojure.data.int-map :as im]
            [collider.game.game-mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.vec :as v]
            [collider.world.chunk :as chunk])
  (:import (collider.java PushCell)))

(set! *warn-on-reflection* true)

(def ^:private ^:const strength (double (float 0.05)))

(def ^:private ^:const threshold (double (float 0.01)))

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

(defn- cell-eids ^longs [^PushCell c] (.eids c))

(defn- cell-xs ^doubles [^PushCell c] (.xs c))

(defn- cell-ys ^doubles [^PushCell c] (.ys c))

(defn- cell-zs ^doubles [^PushCell c] (.zs c))

(defn- cell-halfs ^doubles [^PushCell c] (.halfs c))

(defn- cell-heights ^doubles [^PushCell c] (.heights c))

(defn- cell-key ^long [^long cx ^long cz]
  (bit-or (bit-shift-left (bit-and cx 0xFFFFFFFF) 32)
          (bit-and cz 0xFFFFFFFF)))

(defn- cell-of ^long [^double x ^double z]
  (cell-key (bit-shift-right (long (Math/floor x)) 2)
            (bit-shift-right (long (Math/floor z)) 2)))

(defn alive?
  "Tells whether a body takes shoves: a dead one never does, though
  it still steps and shoves the living (LivingEntity.isPushable)."
  [e]
  (pos? (double (:health e 1.0))))

(defn- body?
  "Returns true when the body steps with the bodies around it. Only
  players and mobs do, and a spectator never does. The body must
  stand in a held chunk. A chunk that stops ticking stays held, so
  its bodies still take shoves."
  [held [_ e]]
  (and (or (= :player (:type e)) (mobs/mob-type? (:type e)))
       (not (game-mode/spectator? e))
       (contains? held (chunk/pos-chunk (:pos e)))))

(defn- pushable?
  "EntitySelector.pushableBy: a body that takes shoves."
  [held [_ e :as entry]]
  (and (body? held entry) (alive? e)))

(defn- bodies [world held]
  (filter (fn [entry] (body? held entry)) (:entities world)))

(defn- by-cell [entries]
  (persistent!
    (reduce (fn [m entry]
              (let [p (:pos (nth entry 1))
                    k (cell-of (double (v/x p)) (double (v/z p)))]
                (assoc! m k (conj (get m k []) entry))))
            (transient (im/int-map))
            entries)))

(defn- packed-cell ^PushCell [entries]
  (let [n (count entries)
        eids (long-array n) xs (double-array n) ys (double-array n)
        zs (double-array n) hs (double-array n) ts (double-array n)]
    (loop [i 0 es (seq entries)]
      (when es
        (let [[eid e] (first es) p (:pos e)]
          (aset eids i (long eid))
          (aset xs i (double (v/x p)))
          (aset ys i (double (v/y p)))
          (aset zs i (double (v/z p)))
          (aset hs i (pushable-half e))
          (aset ts i (pushable-height e))
          (recur (inc i) (next es)))))
    (PushCell. eids hs ts xs ys zs)))

(defn- hood ^objects [cells ^long k]
  (let [cx (long (unchecked-int (bit-shift-right k 32)))
        cz (long (unchecked-int k))
        cs (object-array 9)]
    (dotimes [c 9]
      (let [dx (dec (long (quot c 3)))
            dz (dec (long (rem c 3)))]
        (aset cs c (get cells (cell-key (+ cx dx) (+ cz dz))))))
    cs))

(defn- packed [cells]
  (persistent!
    (reduce-kv (fn [m k es]
                 (assoc! m k (packed-cell (sort-by first es))))
               (transient (im/int-map))
               cells)))

(defn index-of
  "Returns the entries laid out so that a body finds every body
  near enough to shove it."
  [entries]
  (let [cells (packed (by-cell entries))
        f (fn [m k _] (assoc! m k (hood cells k)))]
    (persistent! (reduce-kv f (transient (im/int-map)) cells))))

(defn- touching
  "Returns the cells that a body in cell k can reach in one tick.
  A cell is wider than that reach."
  [^long k]
  (let [cx (long (unchecked-int (bit-shift-right k 32)))
        cz (long (unchecked-int k))]
    (for [dx [-1 0 1] dz [-1 0 1]]
      (cell-key (+ cx (long dx)) (+ cz (long dz))))))

(defn- blob
  "Returns the occupied cells that k reaches by steps between
  touching cells."
  [cells ^long k]
  (loop [q [k] seen #{k}]
    (if-let [c (peek q)]
      (let [near (filter (fn [n] (contains? cells n)) (touching c))
            cs (remove seen near)]
        (recur (into (pop q) cs) (into seen cs)))
      seen)))

(defn- group-of [cells b]
  (vec (sort-by first (mapcat (fn [k] (get cells k)) b))))

(defn- grouped [cells]
  (loop [ks (keys cells) seen #{} out []]
    (if-let [k (first ks)]
      (if (contains? seen k)
        (recur (next ks) seen out)
        (let [b (blob cells (long k))]
          (recur (next ks) (into seen b)
                 (conj out (group-of cells b)))))
      out)))

(defn islands
  "Returns the pushable bodies in groups that one tick of movement
  cannot bring together. Each group steps on its own."
  [world held]
  (sort-by ffirst (grouped (by-cell (bodies world held)))))

(defn- impulse
  "Writes to out the shove that a body takes from another body at
  offset dx dz. Returns true when there is a shove. Bodies that
  almost coincide give no shove."
  [^doubles out ^double dx ^double dz]
  (let [m (Math/max (Math/abs dx) (Math/abs dz))]
    (when (>= m threshold)
      (let [s (Math/sqrt m)
            p (Math/min 1.0 (/ 1.0 s))]
        (aset out 0 (* (* (/ dx s) p) strength))
        (aset out 1 (* (* (/ dz s) p) strength))
        true))))

(defn- pair
  "Returns true when body j of the cell shoves this body. Writes
  the shove to out."
  [^doubles out ^doubles me ^PushCell c ^long j]
  (let [x (aget me 0) y (aget me 1) z (aget me 2)
        ox (aget (cell-xs c) j)
        oy (aget (cell-ys c) j)
        oz (aget (cell-zs c) j)
        r (+ (aget me 3) (aget (cell-halfs c) j))]
    (and (< (Math/abs (- ox x)) r)
         (< (Math/abs (- oz z)) r)
         (< oy (+ y (aget me 4)))
         (> (+ oy (aget (cell-heights c) j)) y)
         (impulse out (- x ox) (- z oz)))))

(defn- cell-shoves [^doubles out ^doubles me ^PushCell c hi acc]
  (let [ids (cell-eids c) hi (long hi) eid (long (aget me 5))]
    (loop [j 0 acc acc]
      (if (= j (alength ids))
        acc
        (let [o (aget ids j)]
          (recur (inc j)
                 (if (and (not= o eid) (< o hi) (pair out me c j))
                   (conj acc [o (aget out 0) (aget out 1)])
                   acc)))))))

(defn- hood-shoves [^objects cs ^doubles out ^doubles me hi]
  (loop [c 0 acc []]
    (if (= c 9)
      acc
      (recur (inc c)
             (if-let [cell (aget cs c)]
               (cell-shoves out me ^PushCell cell hi acc)
               acc)))))

(defn- scan
  "Returns the shoves between this body and each body with an id
  below hi, in the order found. Each shove is [eid dx dz]. This
  body takes dx dz and the other body takes the opposite. The
  shoves are not summed."
  [index eid e half height hi]
  (let [p (:pos e)
        x (double (v/x p)) z (double (v/z p))
        fields [x (double (v/y p)) z (double half)
                (double height) (double (long eid))]
        me (double-array fields)
        out (double-array 2)]
    (if-let [^objects cs (get index (cell-of x z))]
      (hood-shoves cs out me hi)
      [])))

(defn shoves
  "Returns the shoves of one run of the body over every body it
  meets. The run is the last part of its tick."
  [index eid e half height]
  (scan index eid e half height Long/MAX_VALUE))

(defn before
  "Returns the shoves that the bodies which stepped earlier this
  tick gave to this body. This body takes the opposite of what
  each of them took."
  [index eid e half height]
  (scan index eid e half height (long eid)))
