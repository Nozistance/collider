(ns collider.game.entity.sections
  "Entities by the entity section they stand in, in the order the
  game visits them."
  (:require [collider.data.long-map :as lm]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- section ^long [^double c]
  (bit-shift-right (long (Math/floor c)) 4))

(defn- packed ^long [^long x ^long y ^long z]
  (bit-or (bit-shift-left (bit-and x 0x3FFFFF) 42)
          (bit-shift-left (bit-and z 0x3FFFFF) 20)
          (bit-and y 0xFFFFF)))

(defn- key-of ^long [p]
  (packed (section (v/x p)) (section (v/y p)) (section (v/z p))))

(defn- key-y ^long [^long k]
  (bit-shift-right (bit-shift-left k 44) 44))

(defn- key-z ^long [^long k]
  (bit-shift-right (bit-shift-left k 22) 42))

(def empty-index
  "An index without entities."
  {:secs (sorted-map) :at (lm/long-map)})

(defn removed
  [idx id]
  (if-let [[k n] (get (:at idx) id)]
    (assoc idx :secs (update (:secs idx) k assoc n nil)
               :at (dissoc (:at idx) id))
    idx))

(defn placed
  "Returns index idx with entity id at p. An entity that comes to
  another section goes last in it."
  [idx id p]
  (let [k (key-of p) at (get (:at idx) id) entry [id (v/v3 p)]]
    (if (and at (== k (long (nth at 0))))
      (assoc-in idx [:secs k (nth at 1)] entry)
      (let [idx (removed idx id) es (get (:secs idx) k [])]
        (assoc idx :secs (assoc (:secs idx) k (conj es entry))
                   :at (assoc (:at idx) id [k (count es)]))))))

(defn- came ^long [e]
  (if-let [a (:arrived e)] (long (nth a 0)) Long/MIN_VALUE))

(defn- rank ^long [id e]
  (if-let [r (nth (:arrived e) 1 nil)] (long r) (* 2 (long id))))

(defn- earlier [[a ea] [b eb]]
  (let [c (compare (came ea) (came eb))]
    (if (zero? c) (compare (rank a ea) (rank b eb)) c)))

(defn of
  "Returns the index of the entries [id e], each where it stands, in
  the order they came to their sections."
  [entries]
  (let [add (fn [m [id e]]
              (let [p (v/v3 (:pos e)) k (key-of p)]
                (assoc! m k (conj (get m k []) [id p]))))
        secs (persistent!
               (reduce add (transient {}) (sort earlier entries)))
        at (fn [[k es]] (map-indexed (fn [n [id]] [id [k n]]) es))]
    {:secs (into (sorted-map) secs)
     :at (into (lm/long-map) (mapcat at) secs)}))

(defn- in-range? [k y0 y1 z0 z1]
  (and (<= (long y0) (key-y (long k)) (long y1))
       (<= (long z0) (key-z (long k)) (long z1))))

(defn- column [secs ^long x]
  (subseq secs >= (packed x 0 0) <= (packed x -1 -1)))

(defn within
  "Returns [id place] of the entities in the sections the game
  visits for the box from lo to hi, in the order it visits them."
  [idx lo hi]
  (let [y0 (section (- (v/y lo) 4.0)) y1 (section (v/y hi))
        z0 (section (- (v/z lo) 2.0)) z1 (section (+ (v/z hi) 2.0))
        hit? (fn [[k]] (in-range? k y0 y1 z0 z1))]
    (into [] (comp (mapcat #(column (:secs idx) %)) (filter hit?)
                   (mapcat val) (filter some?))
          (range (section (- (v/x lo) 2.0))
                 (inc (section (+ (v/x hi) 2.0)))))))
