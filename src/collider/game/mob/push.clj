(ns collider.game.mob.push
  "The shoves that bodies which overlap hand each other."
  (:require [clojure.data.int-map :as im]
            [collider.game.mob.mobs :as mobs]
            [collider.game.state :as state]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const strength (double (float 0.05)))

(def ^:private ^:const threshold (double (float 0.01)))

(defn- pushable-half ^double [e]
  (case (:type e)
    :player 0.3
    (double (or (first (mobs/box-of e)) 0.0))))

(defn- pushable-height ^double [e]
  (case (:type e)
    :player 1.8
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
  "Whether the body both shoves and is shoved: a player or a mob,
  in a chunk the tick still runs. Items and primed TNT are never
  pushed."
  [active [_ e]]
  (and (or (= :player (:type e)) (mobs/mob-type? (:type e)))
       (state/active-at? active (:pos e))))

(defn- bodies [world active]
  (filter (fn [entry] (pushable? active entry)) (:entities world)))

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

(defn push-index [world active]
  (index-of (bodies world active)))

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
  [world active]
  (sort-by ffirst (grouped (by-cell (bodies world active)))))

(defn- impulse!
  "Adds to acc the shove of one body on another: 0.05 apart along
  the line between them, cut by the root of their Chebyshev
  distance and dropped altogether below 0.01."
  [^doubles acc ^double dx ^double dz]
  (let [m (Math/max (Math/abs dx) (Math/abs dz))]
    (when (>= m threshold)
      (let [s (Math/sqrt m)
            p (Math/min 1.0 (/ 1.0 s))]
        (aset acc 0 (+ (aget acc 0) (* (* (/ dx s) p) strength)))
        (aset acc 1 (+ (aget acc 1) (* (* (/ dz s) p) strength)))))))

(defn- shove-pair! [^doubles acc ^doubles me ^PushCell c ^long j]
  (let [x (aget me 0) y (aget me 1) z (aget me 2)
        ox (aget (cell-xs c) j)
        oy (aget (cell-ys c) j)
        oz (aget (cell-zs c) j)
        r (+ (aget me 3) (aget (cell-halfs c) j))]
    (when (and (< (Math/abs (- ox x)) r)
               (< (Math/abs (- oz z)) r)
               (< oy (+ y (aget me 4)))
               (> (+ oy (aget (cell-heights c) j)) y))
      (impulse! acc (- x ox) (- z oz)))))

(defn- shove-cell! [^doubles acc ^doubles me ^PushCell c ^long hi]
  (let [ids (cell-eids c)
        eid (long (aget me 5))]
    (dotimes [j (alength ids)]
      (let [o (aget ids j)]
        (when (and (not= o eid) (< o hi))
          (shove-pair! acc me c j))))))

(defn- shove [index e eid hi half height]
  (let [p (:pos e)
        x (double (v/x p)) z (double (v/z p))
        fields [x (double (v/y p)) z (double half)
                (double height) (double (long eid))]
        me (double-array fields)
        acc (double-array 2)]
    (when-let [^objects cs (get index (cell-of x z))]
      (dotimes [c 9]
        (when-let [cell (aget cs c)]
          (shove-cell! acc me ^PushCell cell (long hi)))))
    [(aget acc 0) (aget acc 1)]))

(defn before
  "The shoves already in the mob's velocity when its tick starts.
  A lower eid pushed earlier this tick against this same box, a
  higher one pushed at the end of the last tick, when both stood
  where the snapshot holds them."
  [index eid e half height]
  (shove index e (long eid)
         (if (:ticked? e) Long/MAX_VALUE (long eid)) half height))

(defn after
  "The shove the mob hands out itself, the last thing its tick
  does: its new box against every other box."
  [index eid e half height]
  (shove index e (long eid) Long/MAX_VALUE half height))
