(ns collider.game.schedule
  "Lists of scheduled block ticks."
  (:require [collider.cell :as cell]
            [collider.data.long-map :as lm]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def block-list
  "The empty list of block ticks."
  {:queue (lm/long-map) :index (lm/long-map) :next 0})

(def fluid-list
  "The empty list of fluid ticks."
  {:queue (lm/long-map) :index (lm/long-map) :next 0})

(defn- queued [q at id ty v]
  (let [m (or (get q at) (lm/long-map))]
    (assoc q at (assoc m id (assoc (get m id {}) ty v)))))

(defn scheduled?
  "Returns true when ticks hold a tick of type ty at block id."
  [ticks ^long id ty]
  (some? (get (get (:index ticks) id) ty)))

(defn- added [ticks at id ty priority order]
  (let [at (long at) id (long id)
        v [(long priority) (long order)]]
    (if (scheduled? ticks id ty)
      ticks
      (-> ticks
          (update :queue queued at id ty v)
          (assoc-in [:index id ty] at)))))

(defn add
  "Returns ticks with a tick of type ty at block id due at tick at.
  Among ticks due at the same tick, a lower priority runs first and
  equal priorities run in the order they were added."
  ([ticks at id ty] (add ticks at id ty 0))
  ([ticks at id ty priority]
   (let [order (long (:next ticks))]
     (-> (assoc ticks :next (inc order))
         (added at id ty priority order)))))

(defn- queued! [q at id ty v]
  (let [m (or (get q at) (lm/long-map))]
    (assoc! q at (assoc m id (assoc (get m id {}) ty v)))))

(defn- added-to [[q ix ^long n] [at id ty priority]]
  (let [at (long at) id (long id)
        tys (get ix id)]
    (if (some? (get tys ty))
      [q ix (inc n)]
      [(queued! q at id ty [(long priority) n])
       (assoc! ix id (assoc tys ty at))
       (inc n)])))

(defn add-all
  "Returns ticks with each tick [at id ty priority] of entries added
  in order, as add adds them one by one."
  [ticks entries]
  (let [start [(transient (:queue ticks)) (transient (:index ticks))
               (long (:next ticks))]
        [q ix n] (reduce added-to start entries)]
    (assoc ticks
           :queue (persistent! q) :index (persistent! ix) :next n)))

(defn- due-rows [ticks ^long t]
  (into [] (take-while (fn [[at _]] (<= (long at) t)))
        (:queue ticks)))

(defn due
  "Returns the ticks due by tick t, by block id."
  [ticks t]
  (transduce (map val) (completing #(merge-with into %1 %2))
             (lm/long-map) (due-rows ticks (long t))))

(defn- row-entries [[at m]]
  (for [[id tys] m [ty [priority order]] tys]
    [at priority order id ty]))

(defn- entry-order [e] (nth e 2))

(defn- entry-id [e] (nth e 3))

(defn- lane-order [[at priority order]] [at priority order])

(defn- lanes [es]
  (->> (group-by #(chunk/block-id-chunk (entry-id %)) es)
       vals
       (mapv #(sort-by lane-order %))))

(defn- head-key [lane]
  (let [[_ priority order id] (first lane)]
    [priority order (chunk/block-id-chunk id)]))

(defn- merged [lanes]
  (loop [heads (into (sorted-map)
                     (map (fn [l] [(head-key l) l])) lanes)
         acc (transient [])]
    (if-let [[k lane] (first heads)]
      (let [more (next lane)
            heads (dissoc heads k)]
        (recur (if more (assoc heads (head-key more) more) heads)
               (conj! acc (first lane))))
      (persistent! acc))))

(defn- by-order [[_ p1 o1 id1] [_ p2 o2 id2]]
  (let [c (Long/compare (long p1) (long p2))
        c (if (zero? c) (Long/compare (long o1) (long o2)) c)]
    (if (zero? c)
      (Long/compare (chunk/block-id-chunk (long id1))
                    (chunk/block-id-chunk (long id2)))
      c)))

(defn- tys-into [acc at id tys]
  (reduce-kv (fn [acc ty [priority order]]
               (conj! acc [at priority order id ty]))
             acc tys))

(defn- due-into [acc [at m] runs?]
  (let [kept #(if (runs? %2) (tys-into %1 at %2 %3) %1)]
    (reduce-kv kept acc m)))

(defn run-order
  "Returns [id ty priority] of the first limit ticks due at t, in
  run order.
  Only the ticks whose block id runs? accepts count. A chunk gives
  its ticks by due tick, priority and the order they were added in.
  The next ticks of the chunks take turns by priority and order."
  [ticks t runs? limit]
  (let [rows (due-rows ticks (long t))
        into-due #(due-into %1 %2 runs?)
        es (persistent! (reduce into-due (transient []) rows))]
    (into [] (comp (take limit)
                   (map (fn [[_ priority _ id ty]] [id ty priority])))
          (if (next rows) (merged (lanes es)) (sort by-order es)))))

(defn- unqueued! [q at id ty]
  (let [m (get q at)
        tys (dissoc (get m id) ty)
        m (if (seq tys) (assoc m id tys) (dissoc m id))]
    (if (seq m) (assoc! q at m) (dissoc! q at))))

(defn- taken-out [[q ix] [id ty]]
  (let [id (long id)
        tys (get ix id)]
    (if-some [at (get tys ty)]
      (let [left (dissoc tys ty)]
        [(unqueued! q at id ty)
         (if (seq left) (assoc! ix id left) (dissoc! ix id))])
      [q ix])))

(defn without
  "Returns ticks without the ticks [id ty] of taken."
  [ticks taken]
  (let [[q ix] (reduce taken-out
                       [(transient (:queue ticks))
                        (transient (:index ticks))]
                       taken)]
    (assoc ticks :queue (persistent! q) :index (persistent! ix))))

(defn- in-chunk? [^long cid ^long id]
  (= cid (chunk/block-id-chunk id)))

(defn- outside [^long cid m]
  (into (lm/long-map) (remove #(in-chunk? cid (key %))) m))

(defn dropped
  "Returns ticks without the ticks of chunk cid."
  [ticks cid]
  (let [cid (long cid)
        row (fn [[at m]]
              (let [m (outside cid m)] (when (seq m) [at m])))
        q (into (lm/long-map) (keep row) (:queue ticks))]
    (assoc ticks :queue q :index (outside cid (:index ticks)))))

(defn entries
  "Returns each tick of ticks as [at id ty]."
  [ticks]
  (for [[at m] (:queue ticks) [id tys] m ty (keys tys)] [at id ty]))

(defn copied
  "Returns as [at id ty priority order] every tick of a block id that
  in? accepts, moved to (moved id). They keep their order among
  themselves and follow the last tick of ticks."
  [ticks in? moved]
  (let [xf (comp (mapcat row-entries) (filter #(in? (entry-id %))))
        es (into [] xf (:queue ticks))
        orders (map entry-order es)
        lo (long (reduce min Long/MAX_VALUE orders))
        hi (long (reduce max Long/MIN_VALUE orders))]
    (mapv (fn [[at priority order id ty]]
            [at (moved id) ty priority (+ (- (long order) lo) hi 1)])
          es)))

(defn add-ordered
  "Returns ticks with each tick [at id ty priority order] of entries
  added in its own order. A block keeps a tick of a type
  it already has."
  [ticks entries]
  (reduce (fn [t e] (apply added t e)) ticks entries))

(defn- relative [^long t [at priority _ id ty]]
  [(- (long at) t) (cell/unpack id) ty priority])

(defn saved
  "Returns the ticks of chunk cid as [delay pos type priority].
  They come in the order they were added in."
  [ticks cid t]
  (->> (mapcat row-entries (:queue ticks))
       (filter #(in-chunk? cid (entry-id %)))
       (sort-by entry-order)
       (mapv #(relative t %))))

(defn saved-by-chunk
  "Returns the ticks of every chunk as saved, by chunk id."
  [ticks t]
  (let [by-chunk #(chunk/block-id-chunk (entry-id %))
        es (mapcat row-entries (:queue ticks))]
    (update-vals (group-by by-chunk es)
                 #(mapv (partial relative t) (sort-by entry-order %)))))

(defn restored
  "Returns ticks with the saved ticks back, due after their delays.
  They come before every tick added after the load and keep their
  order among themselves."
  [ticks t saved]
  (let [base (- (count saved))]
    (reduce (fn [ticks [n [dt p ty priority]]]
              (added ticks (+ (long t) (max 1 (long dt)))
                     (cell/pack p) ty priority
                     (+ base (long n))))
            ticks (map-indexed vector saved))))
