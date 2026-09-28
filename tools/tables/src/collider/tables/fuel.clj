(ns collider.tables.fuel
  "The items a furnace burns and for how long."
  (:require [collider.tables.reflect
             :refer [call call-static key-of registry]]))

(set! *warn-on-reflection* true)

(defn fuel
  "Returns how many ticks each item burns for in a furnace of a
  server with the registries access and the features enabled."
  [access features]
  (let [items (registry "ITEM")
        c "world.level.block.entity.FuelValues"
        values (call-static c "vanillaBurnTimes" access features)
        ticks #(call values "burnDuration"
                     (call % "getDefaultInstance"))]
    (into (sorted-map)
          (map (fn [i] [(key-of items i) (ticks i)]))
          (call values "fuelItems"))))
