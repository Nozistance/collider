(ns collider.tables.entities
  "The sizes of the entity types."
  (:require [collider.tables.reflect
             :refer [call cls elements hidden-field key-of registry]]
            [collider.tables.value :refer [flt]])
  (:import (java.lang.reflect Field Method ParameterizedType)))

(set! *warn-on-reflection* true)

(defn- size [d]
  {:width (flt (call d "width")) :height (flt (call d "height"))
   :eye (flt (call d "eyeHeight"))})

(defn- type-class [^Field f]
  (let [t (Field/.getGenericType f)]
    (when (instance? ParameterizedType t)
      (let [a (first (ParameterizedType/.getActualTypeArguments t))]
        (when (class? a) a)))))

(defn- classes []
  (let [types (cls "world.entity.EntityTypes")
        kind (cls "world.entity.EntityType")]
    (into {}
          (for [^Field f (Class/.getDeclaredFields types)
                :when (= kind (Field/.getType f))]
            [(Field/.get f nil) (type-class f)]))))

(defn- declares? [^Class c m]
  (some #(= m (Method/.getName %)) (Class/.getDeclaredMethods c)))

(defn- own-dimensions? [^Class c]
  (let [living (cls "world.entity.LivingEntity")]
    (some #(declares? % "getDefaultDimensions")
          (take-while #(not= living %) (iterate Class/.getSuperclass c)))))

(defn- aged? [^Class c]
  (Class/.isAssignableFrom (cls "world.entity.AgeableMob") c))

(defn- baby [c dims]
  (when c
    (if-some [b (hidden-field c nil "BABY_DIMENSIONS")]
      (size b)
      (when (and (aged? c) (not (own-dimensions? c)))
        (size (call dims "scale" (float 0.5)))))))

(defn entities
  "Returns the width, height and eye height of each entity type, and
  of its baby when it has one."
  []
  (let [reg (registry "ENTITY_TYPE")
        by-type (classes)]
    (into (sorted-map)
          (for [t (elements reg)
                :let [dims (call t "getDimensions")
                      b (baby (by-type t) dims)]]
            [(key-of reg t) (cond-> (size dims) b (assoc :baby b))]))))
