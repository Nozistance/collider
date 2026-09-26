(ns collider.game.schedule
  "Lists of scheduled ticks, as LevelTicks of vanilla. Each tick
  keeps the order it was scheduled in, the subTickOrder."
  (:require [clojure.data.int-map :as i]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def block-list {:queue (i/int-map) :index (i/int-map) :next 0})

(def fluid-list {:queue (i/int-map) :index (i/int-map) :next 0})

(def wake-list
  "The queue of neighbour updates. It keeps each entry: the limit
  of one tick for each block and type is not for these."
  {:queue (i/int-map) :next 0})

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
  (let [order (long (:next ticks 0))]
    (-> (assoc ticks :next (inc order))
        (added at id ty order))))

(defn- due-rows [ticks ^long t]
  (into [] (take-while (fn [[at _]] (<= (long at) t)))
        (:queue ticks)))

(defn due [ticks t]
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
  (let [c (compare o1 o2)]
    (if (zero? c)
      (compare (chunk/block-id-chunk id1) (chunk/block-id-chunk id2))
      c)))

(defn run-order
  "Returns the ticks due at t whose block id runs? accepts, as
  [id ty] in the order LevelTicks runs them. A chunk gives its
  ticks by due tick, then by the order they were added in; the
  chunks take turns by the order of their next tick. Ticks all
  due at one tick simply go by their order."
  [ticks t runs?]
  (let [rows (due-rows ticks (long t))
        due (comp (mapcat row-entries) (filter #(runs? (% 2))))
        es (into [] due rows)]
    (mapv (fn [[_ _ id ty]] [id ty])
          (if (next rows) (merged (lanes es)) (sort by-order es)))))

(defn- unindexed [index [id tys]]
  (let [left (reduce dissoc (get index id) (keys tys))]
    (if (seq left) (assoc index id left) (dissoc index id))))

(defn- kept-row [q [at m] parked]
  (let [m' (into (i/int-map) (filter #(contains? parked (key %))) m)]
    (if (seq m') (assoc q at m') (dissoc q at))))

(defn flushed
  "Returns ticks without the ticks due at t, but for those of the
  blocks in parked. These stay as they are, overdue."
  [ticks t parked]
  (let [rows (due-rows ticks (long t))
        parked (into (i/int-set) parked)
        q (reduce #(kept-row %1 %2 parked) (:queue ticks) rows)
        gone (for [[_ m] rows [id tys] m
                   :when (not (contains? parked id))]
               [id tys])]
    (cond-> (assoc ticks :queue q)
      (:index ticks) (update :index #(reduce unindexed % gone)))))

(defn- in-chunk? [^long cid ^long id]
  (= cid (chunk/block-id-chunk id)))

(defn- outside [^long cid m]
  (into (i/int-map) (remove #(in-chunk? cid (key %))) m))

(defn dropped [ticks cid]
  (let [cid (long cid)
        row (fn [[at m]]
              (let [m (outside cid m)] (when (seq m) [at m])))
        q (into (i/int-map) (keep row) (:queue ticks))]
    (cond-> (assoc ticks :queue q)
      (:index ticks) (update :index #(outside cid %)))))

(defn entries [ticks]
  (for [[at m] (:queue ticks) [id tys] m ty (keys tys)] [at id ty]))

(defn- relative [^long t [at _ id ty]]
  [(- (long at) t) (chunk/id->block-pos id) ty])

(defn saved
  "Returns the ticks of chunk cid as [delay pos type], in the order
  they were added in, as LevelChunkTicks.pack."
  [ticks cid t]
  (->> (mapcat row-entries (:queue ticks))
       (filter #(in-chunk? cid (% 2)))
       (sort-by second)
       (mapv #(relative t %))))

(defn restored
  "Returns ticks with the saved ticks back, due after their delays.
  They come before every tick added after the load, and keep their
  order among themselves, as LevelChunkTicks.unpack."
  [ticks t saved]
  (let [base (- (count saved))]
    (reduce (fn [ticks [n [dt p ty]]]
              (added ticks (+ (long t) (max 1 (long dt)))
                     (chunk/block-pos->id p) ty (+ base (long n))))
            ticks (map-indexed vector saved))))
