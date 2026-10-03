(ns collider.game.entity.size
  "The bounding boxes and eye heights of entity types."
  (:require [collider.data :as data]
            [collider.num :as num]))

(set! *warn-on-reflection* true)

(defn- measured [{:keys [width height eye baby poses]}]
  (cond-> {:box [(* 0.5 (num/f32 width)) (num/f32 height)]
           :eye (num/f32 eye)}
    baby (assoc :baby (measured baby))
    poses (assoc :poses (update-vals poses measured))))

(def ^:private ^:table sizes
  (delay (update-vals (data/entities) measured)))

(defn- size-of [e]
  (let [s (get @sizes (:type e))]
    (or (when (some? (:baby-until e)) (:baby s))
        (get (:poses s) (:pose e))
        s)))

(defn box
  "Returns the half width and the height of entity e, as its type,
  age and pose make it, or nil for a type of no size."
  [e]
  (:box (size-of e)))

(defn eye
  "Returns how far above its position entity e looks out."
  ^double [e]
  (double (:eye (size-of e))))

(defn type-box
  "Returns the half width and the height of an adult of type t in
  pose, standing when pose is nil."
  ([t] (type-box t nil))
  ([t pose] (box {:type t :pose pose})))

(defn half
  "Returns the half width of an adult of type t."
  ^double [t]
  (double (nth (type-box t) 0)))

(defn height
  "Returns the height of an adult of type t."
  ^double [t]
  (double (nth (type-box t) 1)))

(defn pose-eye
  "Returns the eye height of an adult of type t in pose."
  ^double [t pose]
  (eye {:type t :pose pose}))
