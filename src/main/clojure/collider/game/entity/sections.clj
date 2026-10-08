(ns collider.game.entity.sections
  "Entities by the entity section they stand in, in the order the
  game visits them."
  (:require [collider.cell :as cell]
            [collider.data.long-map :as lm]
            [collider.game.entity.stamp :as stamp]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]))

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

(defn- slot ^long [ids ^long id]
  (loop [i 0]
    (let [x (nth ids i)]
      (if (and x (== id (long x))) i (recur (inc i))))))

(defn removed
  "Returns index idx without entity id. Its place in its section stays
  empty, nil, and the readers of the index pass over it."
  [idx id]
  (if-let [k (lm/get (:at idx) id)]
    (let [k (long k) ids (lm/get (:secs idx) k)
          ids (assoc ids (slot ids id) nil)]
      (assoc idx :at (lm/dissoc (:at idx) id)
                 :secs (lm/assoc (:secs idx) k ids)))
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

(defn appeared
  "Returns the arrival of an entity that appears in tick t."
  [t]
  [(inc (* 2 (long t))) nil])

(defn- arrival-at [t id old new]
  (cond
    (nil? new) nil
    (nil? old) (when-not (:arrived new) (appeared t))
    (identical? (:arrived old) (:arrived new))
    (when (crossing id old new)
      [(* 2 (long t)) (inc (* 2 (long id)))])))

(defn entered
  "Returns entity id as new, which was old before tick t, arrived in t
  when it appeared or came to another section with no arrival of its
  own."
  [t id old new]
  (if-let [x (arrival-at t id old new)] (assoc new :arrived x) new))

(defn arrivals
  "Returns entities es, which were old before tick t, each one as
  entered gives it."
  [old es t]
  (let [f (fn [acc id a b]
            (let [b' (entered t id a b)]
              (if (identical? b b') acc (lm/assoc acc id b'))))]
    (lm/diff old es f es)))

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

(defn- present [acc id] (if id (conj! acc id) acc))

(defn- visited [idx x0 x1 in?]
  (let [add (fn [acc k ids] (if (in? k) (reduce present acc ids) acc))
        secs (:secs idx)]
    (persistent!
      (reduce (fn [acc x] (reduce-kv add acc (column secs x)))
              (transient []) (range x0 (inc (long x1)))))))

(defn- gap ^double [^double c ^long s]
  (let [lo (* 16.0 s)]
    (max 0.0 (- lo c) (- c (+ lo 16.0)))))

(defn- meets? [c ^double r k]
  (let [k (long k)
        dx (gap (v/x c) (cell/section-x k))
        dy (gap (v/y c) (cell/section-y k))
        dz (gap (v/z c) (cell/section-z k))]
    (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* r r))))

(defn- boxed [idx lo hi in?]
  (let [y0 (section (- (v/y lo) 4.0)) y1 (section (v/y hi))
        z0 (section (- (v/z lo) 2.0)) z1 (section (+ (v/z hi) 2.0))]
    (visited idx (section (- (v/x lo) 2.0)) (section (+ (v/x hi) 2.0))
             (every-pred #(in-range? % y0 y1 z0 z1) in?))))

(defn within
  "Returns the ids of the entities in the sections the game visits
  for the box from lo to hi, in the order it visits them. With point
  c and r, only of the sections that come within r of c."
  ([idx lo hi] (boxed idx lo hi any?))
  ([idx lo hi c r] (boxed idx lo hi #(meets? c r %))))

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
      (reduce-kv (fn [acc _ xs] (reduce present acc xs))
                 (transient []) ids))))

(defn- chunk-of ^long [k]
  (chunk/pos->id (cell/section-x (long k)) (cell/section-z (long k))))

(defn chunked
  "Returns the ids of the entities that pass (keep? id) by the chunk
  they stand in, a set for each chunk that has any."
  [idx keep?]
  (let [add (fn [s id] (if (and id (keep? id)) (conj! s id) s))
        step (fn [m k ids]
               (let [c (chunk-of k)
                     s (transient (lm/get m c (lm/long-set)))
                     s (persistent! (reduce add s ids))]
                 (if (lm/empty? s) m (lm/assoc m c s))))]
    (reduce-kv step (lm/long-map) (:secs idx))))
