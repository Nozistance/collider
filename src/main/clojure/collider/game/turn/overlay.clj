(ns collider.game.turn.overlay
  "Blocks earlier turns of a tick wrote, as later turns see them, and
  what mobs remember to other mobs, as those take it in their turns."
  (:require [collider.game.delta :as delta]
            [collider.game.mob.brain :as brain]))

(set! *warn-on-reflection* true)

(defn- applied [world ds]
  (let [f (fn [w d]
            (if-let [g (delta/world-apply (nth d 0))] (g w d) w))]
    (reduce f world ds)))

(defn seen
  "Returns world as the turn of eid sees it, with what the turns
  before it wrote."
  [world ^long eid]
  (let [ws (::writes world) top (peek ws)
        f (fn [w [b _ bw]] (if (< (long b) eid) bw (reduced w)))]
    (if (and top (< (long (nth top 0)) eid))
      (nth top 2)
      (reduce f world ws))))

(defn- chained [base ws]
  (let [f (fn [[acc w] [eid ds]]
            (let [w (applied w ds)] [(conj acc [eid ds w]) w]))]
    (nth (reduce f [[] base] ws) 0)))

(defn- inserted [ws eid ds]
  (let [[lo hi] (split-with (fn [[b]] (< (long b) (long eid))) ws)]
    (into (conj (vec lo) [eid ds]) hi)))

(defn wrote
  "Returns world where the turns after eid see the writes of the
  level deltas ds of turn eid, and those of the turns after it on
  top. Deltas that change nothing leave world as it is."
  [world eid ds]
  (let [ws (::writes world) top (peek ws)]
    (if (and top (< (long eid) (long (nth top 0))))
      (assoc world ::writes
             (chained (dissoc world ::writes) (inserted ws eid ds)))
      (let [prev (if top (nth top 2) (dissoc world ::writes))
            w (applied prev ds)]
        (if (identical? w prev)
          world
          (assoc world ::writes (conj (or ws []) [eid ds w])))))))

(defn- remember? [d] (identical? :remember (nth d 0)))

(defn told
  "Returns world where each mob that steps? takes hears in its own
  turn what the :remember deltas ds of the turn of eid write to it."
  [world eid ds steps?]
  (let [m0 (::told world {})
        f (fn [m d]
            (let [pid (nth d 1)]
              (if (and (remember? d) (not= pid eid) (steps? pid))
                (update m pid (fnil conj []) [eid d])
                m)))
        m (reduce f m0 ds)]
    (if (identical? m m0) world (assoc world ::told m))))

(defn took
  "Returns mob e after it remembers what the told writes ws say."
  [e ws]
  (let [f (fn [e [_ [_ _ k v until]]] (brain/remember e k v until))]
    (reduce f e ws)))

(defn heard
  "Returns [e later] for mob e in the turn of eid: e after what the
  turns before it told it, and what the turns after it tell it,
  which it takes after its step."
  [world ^long eid e]
  (if-let [ws (get (::told world) eid)]
    (let [before? (fn [[w]] (< (long w) eid))]
      [(took e (filter before? ws)) (into [] (remove before?) ws)])
    [e nil]))

(defn- heard? [m eid d]
  (and (remember? d)
       (some (fn [[w x]] (and (= w eid) (= x d))) (get m (nth d 1)))))

(defn untold
  "Returns the deltas ds of the turn of eid without the writes that
  the mobs it told take in their own turns."
  [world eid ds]
  (if-let [m (::told world)]
    (into [] (remove #(heard? m eid %)) ds)
    ds))
