(ns collider.game.entity.sections
  "Entities by the entity section they stand in, in the order the
  game visits them."
  (:require [collider.cell :as cell]
            [collider.data.long-map :as lm]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- section ^long [^double c]
  (bit-shift-right (long (Math/floor c)) 4))

(defn- key-of ^long [p]
  (cell/pack-section
    (section (v/x p)) (section (v/y p)) (section (v/z p))))

(def empty-index
  "An index without entities."
  {:secs (lm/long-map) :at (lm/long-map)})

(defn- put [idx ^long k n entry]
  (let [secs (:secs idx) es (assoc (lm/get secs k) n entry)]
    (assoc idx :secs (lm/assoc secs k es))))

(defn removed
  [idx id]
  (if-let [[k n] (lm/get (:at idx) id)]
    (assoc (put idx k n nil) :at (lm/dissoc (:at idx) id))
    idx))

(defn placed
  "Returns index idx with entity id at p. An entity that comes to
  another section goes last in it."
  [idx id p]
  (let [k (key-of p) at (lm/get (:at idx) id) entry [id (v/v3 p)]]
    (if (and at (== k (long (nth at 0))))
      (put idx k (nth at 1) entry)
      (let [idx (removed idx id)
            secs (:secs idx)
            es (lm/get secs k [])]
        (assoc idx :secs (lm/assoc secs k (conj es entry))
                   :at (lm/assoc (:at idx) id [k (count es)]))))))

(defn- came ^long [e]
  (if-let [a (:arrived e)] (long (nth a 0)) Long/MIN_VALUE))

(defn- rank ^long [id e]
  (if-let [r (nth (:arrived e) 1 nil)] (long r) (* 2 (long id))))

(defn- arrival [[id e :as entry]]
  [(came e) (rank id e) entry])

(defn- earlier [a b]
  (let [c (Long/compare (nth a 0) (nth b 0))]
    (if (zero? c) (Long/compare (nth a 1) (nth b 1)) c)))

(defn of
  "Returns the index of the entries [id e], each where it stands, in
  the order they came to their sections."
  [entries]
  (let [add (fn [m [_ _ [id e]]]
              (let [p (v/v3 (:pos e)) k (key-of p)]
                (assoc! m k (conj (get m k []) [id p]))))
        secs (reduce add (transient (lm/long-map))
                     (sort earlier (mapv arrival entries)))
        secs (persistent! secs)
        at (fn [[k es]] (map-indexed (fn [n [id]] [id [k n]]) es))]
    {:secs secs :at (into (lm/long-map) (mapcat at) secs)}))

(defn- in-range? [k y0 y1 z0 z1]
  (and (<= (long y0) (cell/section-y (long k)) (long y1))
       (<= (long z0) (cell/section-z (long k)) (long z1))))

(defn- column [secs ^long x]
  (lm/range secs (cell/pack-section x 0 0)
            (cell/pack-section x -1 -1)))

(defn- kept [acc e] (if e (conj! acc e) acc))

(defn within
  "Returns [id place] of the entities in the sections the game
  visits for the box from lo to hi, in the order it visits them."
  [idx lo hi]
  (let [y0 (section (- (v/y lo) 4.0)) y1 (section (v/y hi))
        z0 (section (- (v/z lo) 2.0)) z1 (section (+ (v/z hi) 2.0))
        add (fn [acc k es]
              (if (in-range? k y0 y1 z0 z1) (reduce kept acc es) acc))
        secs (:secs idx)
        x0 (section (- (v/x lo) 2.0)) x1 (section (+ (v/x hi) 2.0))]
    (persistent!
      (reduce (fn [acc x] (reduce-kv add acc (column secs x)))
              (transient []) (range x0 (inc x1))))))
