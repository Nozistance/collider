(ns collider.game.mob.push
  "The shoves that bodies which overlap hand each other."
  (:require [clojure.data.int-map :as im]
            [collider.game.game-mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]))

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

(deftype PushCell [^longs eids ^doubles xs ^doubles ys ^doubles zs
                   ^doubles halfs ^doubles heights])

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

(defn- pushable?
  "Whether the body is shoved: a player or a mob in a chunk the
  view still reaches. A chunk that stopped ticking keeps its
  section reachable, so its bodies still take shoves. Items and
  primed TNT are never pushed, nor a spectator, which has no
  physics (Entity.push, LivingEntity.isPushable)."
  [held [_ e]]
  (and (or (= :player (:type e)) (mobs/mob-type? (:type e)))
       (not (game-mode/spectator? e))
       (contains? held (chunk/pos-chunk (:pos e)))))

(defn- bodies [world held]
  (filter (fn [entry] (pushable? held entry)) (:entities world)))

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
    (PushCell. eids xs ys zs hs ts)))

(defn- hood ^objects [cells ^long k]
  (let [cx (long (unchecked-int (bit-shift-right k 32)))
        cz (long (unchecked-int k))
        cs (object-array 9)]
    (dotimes [c 9]
      (let [k (cell-key (+ cx (dec (long (quot c 3))))
                        (+ cz (dec (long (rem c 3)))))]
        (aset cs c (get cells k))))
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

(defn push-index [world held]
  (index-of (bodies world held)))

(defn- touching
  "The cells whose bodies one still within this cell can reach after
  a tick of movement: a cell is wider than that reach."
  [^long k]
  (let [cx (long (unchecked-int (bit-shift-right k 32)))
        cz (long (unchecked-int k))]
    (for [dx [-1 0 1] dz [-1 0 1]]
      (cell-key (+ cx (long dx)) (+ cz (long dz))))))

(defn- blob
  "The occupied cells reachable from k by steps to a touching cell."
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
  "The pushable bodies split into groups that one tick of movement
  cannot bring together, so each group steps on its own."
  [world held]
  (sort-by ffirst (grouped (by-cell (bodies world held)))))

(defn- impulse
  "Puts into out the shove one body takes from another: 0.05
  apart along the line between them, cut by the root of their
  Chebyshev distance. Bodies nearer than 0.01 shove nothing, and
  then there is no shove to report."
  [^doubles out ^double dx ^double dz]
  (let [m (Math/max (Math/abs dx) (Math/abs dz))]
    (when (>= m threshold)
      (let [s (Math/sqrt m)
            p (Math/min 1.0 (/ 1.0 s))]
        (aset out 0 (* (* (/ dx s) p) strength))
        (aset out 1 (* (* (/ dz s) p) strength))
        true))))

(defn- pair
  "Whether body j of the cell meets this one, its shove in out."
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
  "The shoves between this body and the ones below hi, an
  [eid dx dz] each in the order the lookup found them, where dx dz
  is what this body takes and that one the opposite. Nothing is
  summed: vanilla hands each increment to a velocity of its own."
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
  "One run of the body's own shove over every body it meets, the
  last thing its tick does."
  [index eid e half height]
  (scan index eid e half height Long/MAX_VALUE))

(defn before
  "The shoves the bodies that stepped earlier this tick handed to
  this one: each of them ran its own shove against this box, and
  what it took this body takes the other way round."
  [index eid e half height]
  (scan index eid e half height (long eid)))
