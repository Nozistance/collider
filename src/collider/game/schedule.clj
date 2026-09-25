(ns collider.game.schedule
  (:require [clojure.data.int-map :as i]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def block-list {:queue (i/int-map) :index (i/int-map)})

(def fluid-list {:queue (i/int-map) :index (i/int-map)})

(def wake-list
  "The queue of neighbour updates. It keeps each entry: the limit
  of one tick for each block and type is not for these."
  {:queue (i/int-map)})

(defn- queued [q ^long at ^long id ty]
  (let [m (or (get q at) (i/int-map))]
    (assoc q at (update m id (fnil conj #{}) ty))))

(defn- pending? [ticks ^long id ty]
  (some? (get (get (:index ticks) id) ty)))

(defn add [ticks at id ty]
  (let [at (long at) id (long id)]
    (cond
      (nil? (:index ticks)) (update ticks :queue queued at id ty)
      (pending? ticks id ty) ticks
      :else (-> ticks
                (update :queue queued at id ty)
                (assoc-in [:index id ty] at)))))

(defn- due-rows [ticks ^long t]
  (into [] (take-while (fn [[at _]] (<= (long at) t)))
        (:queue ticks)))

(defn due [ticks t]
  (transduce (map val) (completing #(merge-with into %1 %2))
             (i/int-map) (due-rows ticks (long t))))

(defn- unindexed [index [id tys]]
  (let [left (reduce dissoc (get index id) tys)]
    (if (seq left) (assoc index id left) (dissoc index id))))

(defn- parked-back [ticks ^long t gone id]
  (reduce #(add %1 t id %2) ticks (get gone id)))

(defn- flushed-index [ticks gone]
  (when-let [index (:index ticks)]
    (reduce unindexed index gone)))

(defn flushed [ticks t parked]
  (let [t (long t)
        gone (due ticks t)
        q (reduce dissoc (:queue ticks) (map key (due-rows ticks t)))
        index (flushed-index ticks gone)
        kept (cond-> (assoc ticks :queue q)
               index (assoc :index index))]
    (reduce #(parked-back %1 t gone %2) kept parked)))

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
  (for [[at m] (:queue ticks) [id tys] m ty tys] [at id ty]))

(defn- relative [^long t [at id ty]]
  [(- (long at) t) (chunk/id->block-pos id) ty])

(defn saved [ticks cid t]
  (into [] (comp (filter #(in-chunk? cid (% 1)))
                 (map #(relative t %)))
        (entries ticks)))

(defn restored [ticks t saved]
  (reduce (fn [ticks [dt p ty]]
            (add ticks (+ (long t) (max 1 (long dt)))
                 (chunk/block-pos->id p) ty))
          ticks saved))
