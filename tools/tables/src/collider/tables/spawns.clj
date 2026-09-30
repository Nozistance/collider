(ns collider.tables.spawns
  "What natural spawning reads of the mob categories, the entity
  types and the block states."
  (:require [clojure.string :as str]
            [collider.tables.reflect
             :refer [call call-static cls elements hidden-field key-of
                     registry static-field]]
            [collider.tables.value :refer [flt kw]])
  (:import (java.lang.reflect Field Modifier)))

(set! *warn-on-reflection* true)

(defn- category [c]
  [(kw (call c "getName"))
   {:max (call c "getMaxInstancesPerChunk")
    :friendly (call c "isFriendly")
    :persistent (call c "isPersistent")
    :despawn (call c "getDespawnDistance")
    :no-despawn (call c "getNoDespawnDistance")}])

(defn- categories []
  (mapv category (call-static "world.entity.MobCategory" "values")))

(defn- field-kw [^Field f] (kw (str/lower-case (Field/.getName f))))

(defn- placement-names []
  (let [c (cls "world.entity.SpawnPlacementTypes")]
    (into {}
          (for [^Field f (Class/.getFields c)
                :when (Modifier/isStatic (Field/.getModifiers f))]
            [(Field/.get f nil) (field-kw f)]))))

(defn- runs [ids]
  (reduce (fn [acc ^long id]
            (let [[lo hi] (peek acc)]
              (if (and hi (= id (inc (long hi))))
                (conj (pop acc) [lo id])
                (conj acc [id id]))))
          [] ids))

(defn- states []
  (let [b "world.level.block.Block"
        reg (static-field b "BLOCK_STATE_REGISTRY")]
    (mapv (fn [st] [(call reg "getId" st) st]) (elements reg))))

(defn- ids-where [states pred]
  (runs (sort (for [[id st] states :when (pred st)] id))))

(defn- spawnable? [t]
  (not= "misc" (call (call t "getCategory") "getName")))

(defn- heightmap [t]
  (let [h (call-static "world.entity.SpawnPlacements"
                       "getHeightmapType" t)]
    (kw (str/lower-case (call h "name")))))

(defn- type-facts [placements t]
  (let [sp "world.entity.SpawnPlacements"]
    {:category (kw (call (call t "getCategory") "getName"))
     :far (call t "canSpawnFarFromPlayer")
     :summon (call t "canSummon")
     :peaceful (call t "isAllowedInPeaceful")
     :scale (flt (hidden-field (class t) t "spawnDimensionsScale"))
     :placement (placements (call-static sp "getPlacementType" t))
     :heightmap (heightmap t)}))

(defn- index-of [sets v]
  (or (first (keep-indexed (fn [i s] (when (= s v) i)) @sets))
      (dec (count (swap! sets conj v)))))

(defn- with-sets [states floors dangers t facts]
  (let [air (static-field "world.level.EmptyBlockGetter" "INSTANCE")
        zero (static-field "core.BlockPos" "ZERO")
        floor (ids-where states #(call % "isValidSpawn" air zero t))
        danger (ids-where states #(call t "isBlockDangerous" %))]
    (assoc facts :floor (index-of floors floor)
           :danger (index-of dangers danger))))

(defn- conductors [states]
  (let [air (static-field "world.level.EmptyBlockGetter" "INSTANCE")
        zero (static-field "core.BlockPos" "ZERO")]
    (ids-where states #(call % "isRedstoneConductor" air zero))))

(defn spawns
  "Returns the mob categories in their order, the spawn facts of each
  entity type that is no misc, the states each type may stand on and
  the ones that hurt it, and the states that conduct redstone."
  []
  (let [reg (registry "ENTITY_TYPE")
        placements (placement-names)
        st (states)
        floors (atom []) dangers (atom [])
        facts #(with-sets st floors dangers %
                 (type-facts placements %))
        types (into (sorted-map)
                    (for [t (elements reg) :when (spawnable? t)]
                      [(key-of reg t) (facts t)]))]
    {:categories (categories) :types types
     :floors @floors :dangers @dangers
     :conductors (conductors st)}))
