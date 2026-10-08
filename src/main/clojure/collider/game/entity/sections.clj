(ns collider.game.entity.sections
  "Entities by the entity section they stand in, in the order the
  game visits them."
  (:require [collider.cell :as cell]
            [collider.data.long-map :as lm]
            [collider.game.entity.stamp :as stamp]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- section ^long [^double c]
  (bit-shift-right (long (Math/floor c)) 4))

(defn- key-of ^long [p]
  (cell/pack-section
    (section (v/x p)) (section (v/y p)) (section (v/z p))))

(defn- with-section [idx ^long k ids]
  (let [secs (:secs idx)]
    (assoc idx :secs (if (zero? (count ids))
                       (lm/dissoc secs k)
                       (lm/assoc secs k ids)))))

(defn removed
  [idx id]
  (if-let [k (lm/get (:at idx) id)]
    (let [k (long k) id (long id)
          ids (lm/get (:secs idx) k)
          ids (into [] (remove #(== id (long %))) ids)]
      (with-section (assoc idx :at (lm/dissoc (:at idx) id)) k ids))
    idx))

(defn- inserted [idx ^long k id]
  (let [ids (conj (lm/get (:secs idx) k []) id)
        at (lm/assoc (:at idx) id (num k))]
    (with-section (assoc idx :at at) k ids)))

(defn placed
  "Returns index idx with entity id at p. An entity that comes to
  another section goes last in it."
  [idx id p]
  (let [k (key-of p) at (lm/get (:at idx) id)]
    (if (and at (== k (long at)))
      idx
      (let [idx (removed idx id)]
        (inserted idx k id)))))

(defn- came ^long [e]
  (if-let [a (:arrived e)] (long (nth a 0)) Long/MIN_VALUE))

(defn- rank ^long [id e]
  (if-let [r (nth (:arrived e) 1 nil)] (long r) (* 2 (long id))))

(defn- arrival [[id e :as entry]]
  [(came e) (rank id e) entry])

(defn- earlier [a b]
  (let [c (Long/compare (nth a 0) (nth b 0))]
    (if (zero? c) (Long/compare (nth a 1) (nth b 1)) c)))

(defn- added [[secs at] [_ _ [id e]]]
  (let [k (key-of (:pos e))]
    [(assoc! secs k (conj (get secs k []) id)) (assoc! at id k)]))

(defn of
  "Returns the index of the entries [id e], each where it stands, in
  the order they came to their sections."
  [entries]
  (let [es (sort earlier (mapv arrival entries))
        init [(transient (lm/long-map)) (transient (lm/long-map))]
        [secs at] (reduce added init es)]
    {:secs (persistent! secs) :at (persistent! at)}))

(defn- stays? [id a b]
  (and (== (key-of (:pos a)) (key-of (:pos b)))
       (== (came a) (came b))
       (== (rank id a) (rank id b))))

(defn- kept? [a b]
  (and (identical? (:pos a) (:pos b))
       (identical? (:arrived a) (:arrived b))))

(defn- changed [[gone news :as acc] id a b]
  (cond
    (nil? b) [(conj gone id) news]
    (nil? a) [gone (conj news [id b])]
    (kept? a b) acc
    (stays? id a b) acc
    :else [(conj gone id) (conj news [id b])]))

(defn- dropped [idx gone]
  (let [at (:at idx)
        f (fn [idx k ids]
            (let [out (set ids)
                  kept (into [] (remove out) (lm/get (:secs idx) k))]
              (with-section idx k kept)))
        idx (reduce-kv f idx (group-by #(lm/get at %) gone))]
    (assoc idx :at (reduce #(lm/dissoc %1 %2) at gone))))

(defn- id-of [a] (nth (nth a 2) 0))

(defn- after? [now id a]
  (pos? (long (earlier (arrival [id (get now id)]) a))))

(defn- merged [now ids news]
  (loop [i (count ids) j (dec (count news)) tail ()]
    (let [a (nth news j nil) id (nth ids (dec i) nil)]
      (cond
        (nil? a) (into (subvec ids 0 i) tail)
        (and id (after? now id a)) (recur (dec i) j (cons id tail))
        :else (recur i (dec j) (cons (id-of a) tail))))))

(defn- arrived [now idx k news]
  (let [ids (lm/get (:secs idx) k [])
        ids (merged now ids news)
        at (reduce #(lm/assoc %1 (id-of %2) k) (:at idx) news)]
    (with-section (assoc idx :at at) k ids)))

(defn- shifted [idx now gone news]
  (let [news (sort earlier (mapv arrival news))
        by-key (group-by #(key-of (:pos (nth (nth % 2) 1))) news)]
    (reduce-kv #(arrived now %1 %2 %3) (dropped idx gone) by-key)))

(defn- caught-up [idx es now]
  (let [[gone news] (lm/diff es now changed [[] []])]
    (shifted idx now gone news)))

(defn- made [es] (of (seq es)))

(defn index
  "Returns the entities of level lv by section, in the order they
  came there."
  [lv]
  (stamp/value lv ::index made caught-up))

(defn indexed
  "Returns level lv that keeps its index for its entities."
  [lv]
  (stamp/kept lv ::index made caught-up))

(defn crossing
  "Returns [id from to] when entity id changes its section or the
  time it came there between old and new. A section is nil where id
  is not."
  [id old new]
  (when-not (and old new (or (kept? old new) (stays? id old new)))
    [id (some-> old :pos key-of) (some-> new :pos key-of)]))

(defn moved
  "Returns level lv that keeps the index for entities es, which differ
  from its own by the crossings xs alone. The level still holds its
  own entities."
  [lv es xs]
  (let [came-to #(when (nth % 2) (find es (nth % 0)))
        gone (into [] (keep #(when (nth % 1) (nth % 0))) xs)
        news (into [] (keep came-to) xs)]
    (stamp/with lv ::index es (shifted (index lv) es gone news))))

(defn- in-range? [k y0 y1 z0 z1]
  (and (<= (long y0) (cell/section-y (long k)) (long y1))
       (<= (long z0) (cell/section-z (long k)) (long z1))))

(defn- column [secs ^long x]
  (lm/range secs (cell/pack-section x 0 0)
            (cell/pack-section x -1 -1)))

(defn- visited [idx x0 x1 in?]
  (let [add (fn [acc k ids] (if (in? k) (reduce conj! acc ids) acc))
        secs (:secs idx)]
    (persistent!
      (reduce (fn [acc x] (reduce-kv add acc (column secs x)))
              (transient []) (range x0 (inc (long x1)))))))

(defn within
  "Returns the ids of the entities in the sections the game visits
  for the box from lo to hi, in the order it visits them."
  [idx lo hi]
  (let [y0 (section (- (v/y lo) 4.0)) y1 (section (v/y hi))
        z0 (section (- (v/z lo) 2.0)) z1 (section (+ (v/z hi) 2.0))]
    (visited idx (section (- (v/x lo) 2.0)) (section (+ (v/x hi) 2.0))
             #(in-range? % y0 y1 z0 z1))))

(defn columns
  "Returns the ids of the entities in the sections whose columns meet
  the square from x0 z0 to x1 z1, column by column."
  [idx x0 z0 x1 z1]
  (let [z0 (section z0) z1 (section z1)]
    (visited idx (section x0) (section x1)
             #(<= z0 (cell/section-z (long %)) z1))))

(defn in-chunk
  "Returns the ids of the entities in the sections of chunk cx cz."
  [idx cx cz]
  (let [ids (lm/range (:secs idx) (cell/pack-section cx 0 cz)
                      (cell/pack-section cx -1 cz))]
    (persistent!
      (reduce-kv (fn [acc _ xs] (reduce conj! acc xs))
                 (transient []) ids))))
