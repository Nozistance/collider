(ns collider.tables.blockentities
  "The tags of fresh block entities, as vanilla writes them."
  (:require [collider.tables.reflect
             :refer [call call-static cls elements hidden-field key-of
                     registry static-field]]))

(set! *warn-on-reflection* true)

(def ^:private numbers
  {1 ['nbt/b "byteValue"] 2 ['nbt/s "shortValue"]
   3 ['nbt/i "intValue"] 4 ['nbt/l "longValue"]
   5 ['nbt/f "floatValue"] 6 ['nbt/d "doubleValue"]})

(def ^:private arrays
  {7 ['nbt/B "getAsByteArray"] 11 ['nbt/I "getAsIntArray"]
   12 ['nbt/L "getAsLongArray"]})

(declare nbt)

(defn- compound [t]
  (into (sorted-map)
        (map (fn [k] [(keyword k) (nbt (call t "get" k))]))
        (call t "keySet")))

(defn- nbt-list [t]
  (mapv #(nbt (call t "get" (int %))) (range (call t "size"))))

(defn- nbt
  "Returns tag t as edn, each number and array tagged with its type."
  [t]
  (let [id (long (call t "getId"))]
    (if-let [[tag m] (or (numbers id) (arrays id))]
      (tagged-literal tag (let [v (call t m)]
                            (if (numbers id) v (vec v))))
      (case id
        8 (call t "value")
        9 (nbt-list t)
        10 (compound t)))))

(def ^:private type-class "world.level.block.entity.BlockEntityType")

(defn- first-block [blocks type]
  (let [valid (hidden-field (cls type-class) type "validBlocks")]
    (first (sort-by #(call blocks "getId" %) valid))))

(defn- sends-update? [e]
  (let [m (Class/.getMethod (class e) "getUpdatePacket"
                            (make-array Class 0))]
    (not= (cls "world.level.block.entity.BlockEntity")
          (java.lang.reflect.Method/.getDeclaringClass m))))

(defn- output [access]
  (call-static "world.level.storage.TagValueOutput"
               "createWithContext"
               (static-field "util.ProblemReporter" "DISCARDING")
               access))

(defn- custom [e access]
  (let [out (output access)]
    (call e "saveCustomOnly" out)
    (nbt (call out "buildResult"))))

(defn- fresh [blocks access type]
  (let [st (call (first-block blocks type) "defaultBlockState")
        zero (static-field "core.BlockPos" "ZERO")
        e (call type "create" zero st)]
    {:custom (custom e access)
     :update (nbt (call e "getUpdateTag" access))
     :update-packet? (sends-update? e)}))

(defn- made
  "The fresh tags of type, or nil when a fresh one cannot be saved:
  a moving piston has no direction until it moves."
  [blocks access type]
  (try (fresh blocks access type)
       (catch Exception _ nil)))

(defn block-entities
  "Returns the tags of a fresh block entity of every type by name:
  saveCustomOnly, getUpdateTag, and whether it sends an update."
  [access]
  (let [types (registry "BLOCK_ENTITY_TYPE")
        blocks (registry "BLOCK")]
    (into (sorted-map)
          (keep (fn [t]
                  (when-let [f (made blocks access t)]
                    [(key-of types t) f])))
          (elements types))))
