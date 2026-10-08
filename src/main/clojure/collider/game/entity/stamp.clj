(ns collider.game.entity.stamp
  "Values a level derives from its entities and keeps up to date."
  (:require [collider.data.long-map :as lm])
  (:import (collider.data LongMap)))

(set! *warn-on-reflection* true)

(defn- entities [lv] (or (:entities lv) (lm/long-map)))

(defn value
  "Returns the value under k of level lv for its entities. A value
  kept for earlier entities es is caught up by (caught v es now),
  and without one the value is (made now)."
  [lv k made caught]
  (let [[es v] (get (meta lv) k)
        now (entities lv)]
    (cond
      (identical? es now) v
      (and v (instance? LongMap es) (instance? LongMap now))
      (caught v es now)
      :else (made now))))

(defn with
  "Returns level lv that keeps v under k for entities es, its own by
  default."
  ([lv k v] (with lv k (entities lv) v))
  ([lv k es v]
   (let [[es0 v0] (get (meta lv) k)]
     (if (and (identical? es es0) (identical? v v0))
       lv
       (vary-meta lv assoc k [es v])))))

(defn kept
  "Returns level lv that keeps the value under k for its entities."
  [lv k made caught]
  (if (identical? (entities lv) (nth (get (meta lv) k) 0 nil))
    lv
    (with lv k (value lv k made caught))))
