(ns collider.game.turn.overlay
  "The blocks the turns of a tick wrote so far, as each later turn
  sees them. Each write is a world after it, so every read of the
  world sees it; a tick without writes reads the world itself."
  (:require [collider.game.delta :as delta]))

(set! *warn-on-reflection* true)

(defn- applied [world ds]
  (let [f (fn [w d]
            (if-let [g (delta/world-apply (nth d 0))] (g w d) w))]
    (reduce f world ds)))

(defn seen
  "Returns world as the turn of eid sees it: with what the turns
  before it wrote."
  [world ^long eid]
  (let [f (fn [w [b _ bw]] (if (< (long b) eid) bw (reduced w)))]
    (reduce f world (::writes world))))

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
