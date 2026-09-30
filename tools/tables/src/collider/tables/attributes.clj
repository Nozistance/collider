(ns collider.tables.attributes
  "The attributes a living entity type starts with."
  (:require [collider.tables.reflect
             :refer [call call-static cls elements hidden-field key-of
                     registry]]
            [collider.tables.value :refer [kw]]))

(set! *warn-on-reflection* true)

(def ^:private defaults "world.entity.ai.attributes.DefaultAttributes")

(defn- values [supplier]
  (let [c (cls "world.entity.ai.attributes.AttributeSupplier")
        ^java.util.Map m (hidden-field c supplier "instances")]
    (into (sorted-map)
          (map (fn [[h i]]
                 [(kw (call h "getRegisteredName")) (call i "getValue")]))
          m)))

(defn attributes
  "Returns the default attribute values of each living entity type,
  as DefaultAttributes gives them."
  []
  (let [types (registry "ENTITY_TYPE")]
    (into (sorted-map)
          (keep (fn [t]
                  (when (call-static defaults "hasSupplier" t)
                    [(key-of types t)
                     (values (call-static defaults "getSupplier" t))])))
          (elements types))))
