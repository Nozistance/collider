(ns collider.game.schedule
  "Lists of scheduled block ticks."
  (:require [clojure.data.int-map :as i]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def block-list {:queue (i/int-map) :index (i/int-map) :next 0})

(def fluid-list {:queue (i/int-map) :index (i/int-map) :next 0})

(defn- queued [q at id ty order]
  (let [m (or (get q at) (i/int-map))
        tys (get m id {})]
    (if (contains? tys ty)
      q
      (assoc q at (assoc m id (assoc tys ty order))))))

(defn- pending? [ticks ^long id ty]
  (some? (get (get (:index ticks) id) ty)))

(defn- added [ticks at id ty order]
  (let [at (long at) id (long id) order (long order)]
    (cond
      (nil? (:index ticks))
      (update ticks :queue queued at id ty order)
      (pending? ticks id ty) ticks
      :else (-> ticks
                (update :queue queued at id ty order)
                (assoc-in [:index id ty] at)))))

(defn add
  "Returns ticks with a tick of type ty at block id due at tick at.
  It comes after every tick added before it."
  [ticks at id ty]
  (let [order (long (:next ticks 0))
        at (long at) id (long id)
        index (:index ticks)
        tys (get index id)
        ticks (assoc ticks :next (inc order))]
    (if (and index (some? (get tys ty)))
      ticks
      (cond-> (update ticks :queue queued at id ty order)
        index (assoc :index (assoc index id (assoc tys ty at)))))))

(defn- queued! [q at id ty order]
  (let [m (or (get q at) (i/int-map))
        tys (get m id {})]
    (if (contains? tys ty)
      q
      (assoc! q at (assoc m id (assoc tys ty order))))))

(defn- added-to [[q ix ^long n] [at id ty]]
  (let [at (long at) id (long id)
        tys (when ix (get ix id))]
    (if (and ix (some? (get tys ty)))
      [q ix (inc n)]
      [(queued! q at id ty n)
       (if ix (assoc! ix id (assoc tys ty at)) ix)
       (inc n)])))

(defn add-all
  "Returns ticks with each tick [at id ty] of entries added in
  order, as add adds them one by one."
  [ticks entries]
  (let [index (:index ticks)
        start [(transient (or (:queue ticks) (i/int-map)))
               (some-> index transient)
               (long (:next ticks 0))]
        [q ix n] (reduce added-to start entries)]
    (cond-> (assoc ticks :queue (persistent! q) :next n)
      ix (assoc :index (persistent! ix)))))

(defn- due-rows [ticks ^long t]
  (into [] (take-while (fn [[at _]] (<= (long at) t)))
        (:queue ticks)))

(defn due
  "Returns the ticks due by tick t, by block id."
  [ticks t]
  (transduce (map val) (completing #(merge-with into %1 %2))
             (i/int-map) (due-rows ticks (long t))))

(defn- row-entries [[at m]]
  (for [[id tys] m [ty order] tys] [at order id ty]))

(defn- lane-order [[at order]] [at order])

(defn- lanes [es]
  (->> (group-by #(chunk/block-id-chunk (% 2)) es)
       vals
       (mapv #(sort-by lane-order %))))

(defn- head-key [lane]
  (let [[_ order id] (first lane)]
    [order (chunk/block-id-chunk id)]))

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

(defn- by-order [[_ o1 id1] [_ o2 id2]]
  (let [c (Long/compare (long o1) (long o2))]
    (if (zero? c)
      (Long/compare (chunk/block-id-chunk (long id1))
                    (chunk/block-id-chunk (long id2)))
      c)))

(defn- due-into [acc [at m] runs?]
  (let [kept (fn [acc id tys]
               (if (runs? id)
                 (reduce-kv #(conj! %1 [at %3 id %2]) acc tys)
                 acc))]
    (reduce-kv kept acc m)))

(defn run-order
  "Returns [id ty] of the ticks due at t, in run order.
  Only the ticks whose block id runs? accepts count. A chunk gives
  its ticks by due tick and by the order they were added in. The
  chunks take turns by the order of their next tick."
  [ticks t runs?]
  (let [rows (due-rows ticks (long t))
        es (persistent! (reduce #(due-into %1 %2 runs?)
                                (transient []) rows))]
    (mapv (fn [[_ _ id ty]] [id ty])
          (if (next rows) (merged (lanes es)) (sort by-order es)))))

(defn- unindexed! [index [id tys]]
  (let [left (reduce dissoc (get index id) (keys tys))]
    (if (seq left) (assoc! index id left) (dissoc! index id))))

(defn- unindexed [index gone]
  (persistent! (reduce unindexed! (transient index) gone)))

(defn- kept-row [q [at m] parked]
  (let [kept (filter #(contains? parked (key %)))
        m' (when (seq parked) (into (i/int-map) kept m))]
    (if (seq m') (assoc q at m') (dissoc q at))))

(defn flushed
  "Returns ticks without the ticks due at t.
  The ticks of the blocks in parked stay, overdue."
  [ticks t parked]
  (let [rows (due-rows ticks (long t))
        parked (into (i/int-set) parked)
        q (reduce #(kept-row %1 %2 parked) (:queue ticks) rows)
        gone (for [[_ m] rows [id tys] m
                   :when (not (contains? parked id))]
               [id tys])]
    (cond-> (assoc ticks :queue q)
      (:index ticks) (update :index unindexed gone))))

(defn- in-chunk? [^long cid ^long id]
  (= cid (chunk/block-id-chunk id)))

(defn- outside [^long cid m]
  (into (i/int-map) (remove #(in-chunk? cid (key %))) m))

(defn dropped
  "Returns ticks without the ticks of chunk cid."
  [ticks cid]
  (let [cid (long cid)
        row (fn [[at m]]
              (let [m (outside cid m)] (when (seq m) [at m])))
        q (into (i/int-map) (keep row) (:queue ticks))]
    (cond-> (assoc ticks :queue q)
      (:index ticks) (update :index #(outside cid %)))))

(defn entries
  "Returns each tick of ticks as [at id ty]."
  [ticks]
  (for [[at m] (:queue ticks) [id tys] m ty (keys tys)] [at id ty]))

(defn copied
  "Returns the ticks that LevelTicks.copyAreaFrom copies, each as
  [at id ty order]: every tick of a block id that in? accepts, moved
  to (moved id). They keep their order among themselves and follow
  the last of them."
  [ticks in? moved]
  (let [xf (comp (mapcat row-entries) (filter #(in? (% 2))))
        es (into [] xf (:queue ticks))
        orders (map second es)
        lo (long (reduce min Long/MAX_VALUE orders))
        hi (long (reduce max Long/MIN_VALUE orders))]
    (mapv (fn [[at order id ty]]
            [at (moved id) ty (+ (- (long order) lo) hi 1)])
          es)))

(defn add-ordered
  "Returns ticks with each tick [at id ty order] of entries added in
  its own order. A block keeps a tick of a type it already has."
  [ticks entries]
  (reduce (fn [t [at id ty order]] (added t at id ty order))
          ticks entries))

(defn- relative [^long t [at _ id ty]]
  [(- (long at) t) (chunk/id->block-pos id) ty])

(defn saved
  "Returns the ticks of chunk cid as [delay pos type].
  They come in the order they were added in."
  [ticks cid t]
  (->> (mapcat row-entries (:queue ticks))
       (filter #(in-chunk? cid (% 2)))
       (sort-by second)
       (mapv #(relative t %))))

(defn saved-by-chunk
  "Returns the ticks of every chunk as saved, by chunk id."
  [ticks t]
  (update-vals (group-by #(chunk/block-id-chunk (% 2))
                         (mapcat row-entries (:queue ticks)))
               #(mapv (partial relative t) (sort-by second %))))

(defn restored
  "Returns ticks with the saved ticks back, due after their delays.
  They come before every tick added after the load and keep their
  order among themselves."
  [ticks t saved]
  (let [base (- (count saved))]
    (reduce (fn [ticks [n [dt p ty]]]
              (added ticks (+ (long t) (max 1 (long dt)))
                     (chunk/block-pos->id p) ty (+ base (long n))))
            ticks (map-indexed vector saved))))
