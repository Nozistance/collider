(ns collider.game.entity.arrows
  "The arrows stuck in a living body, working loose one at a time.")

(set! *warn-on-reflection* true)

(defn loose-deltas
  "Returns the deltas of the arrows stuck in living e of eid this
  tick. The more it holds, the sooner the next one goes."
  [eid e]
  (let [n (long (:arrow-count e 0))]
    (when (pos? n)
      (let [t (long (:arrow-time e 0))
            t (dec (if (<= t 0) (* 20 (- 30 n)) t))]
        [[:merge-entity eid (cond-> {:arrow-time t}
                              (<= t 0) (assoc :arrow-count (dec n)))]]))))
