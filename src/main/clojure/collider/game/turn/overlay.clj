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

(defn- told?
  "Returns true when the turn of eid tells mob pid delta d. A blow
  reaches only a mob whose turn comes after."
  [eid pid d]
  (case (nth d 0)
    :remember true
    (:damage :knockback) (< (long eid) (long pid))
    false))

(defn told
  "Returns world where each mob that steps? takes in its own turn
  what the :remember deltas ds of the turn of eid write to it, and
  the blows they deal it before its turn."
  [world eid ds steps?]
  (let [m0 (::told world {})
        f (fn [m d]
            (let [pid (nth d 1)]
              (if (and (not= pid eid) (told? eid pid d) (steps? pid))
                (update m pid (fnil conj []) [eid d])
                m)))
        m (reduce f m0 ds)]
    (if (identical? m m0) world (assoc world ::told m))))

(defn took
  "Returns mob e at tick t after it takes the told writes ws."
  [t e ws]
  (let [f (fn [e [_ d]] ((delta/entity-apply (nth d 0)) t e d))]
    (reduce f e ws)))

(defn heard
  "Returns [e later blows] for mob e in the turn of eid. The e has
  taken what the turns before it told it, and blows holds the deltas
  of those that are no memory, which its turn writes in its place.
  The later holds what the turns after it tell it, which it takes
  after its step."
  [world ^long eid e]
  (if-let [ws (get (::told world) eid)]
    (let [before? (fn [[w]] (< (long w) eid))
          bs (filterv before? ws)]
      [(took (:tick world) e bs) (into [] (remove before?) ws)
       (into [] (comp (map second) (remove remember?)) bs)])
    [e nil nil]))

(defn- heard? [m eid d]
  (some (fn [[w x]] (and (= w eid) (= x d))) (get m (nth d 1))))

(defn untold
  "Returns the deltas ds of the turn of eid without the writes that
  the mobs it told take in their own turns."
  [world eid ds]
  (if-let [m (::told world)]
    (into [] (remove #(heard? m eid %)) ds)
    ds))
