(ns collider.game.entity.size
  "The bounding boxes and eye heights of entity types."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(defn- f32 ^double [v] (double (float v)))

(defn- measured [{:keys [width height eye baby]}]
  (cond-> {:box [(* 0.5 (f32 width)) (f32 height)] :eye (f32 eye)}
    baby (assoc :baby (measured baby))))

(def ^:private ^:table sizes
  (delay (update-vals (data/entities) measured)))

(defn- size-of [e]
  (let [s (get @sizes (:type e))]
    (or (when (some? (:baby-until e)) (:baby s)) s)))

(defn box
  "Returns the half width and the height of entity e, as its type
  and age make it, or nil for a type of no size."
  [e]
  (:box (size-of e)))

(defn eye
  "Returns how far above its position entity e looks out."
  ^double [e]
  (double (:eye (size-of e))))
