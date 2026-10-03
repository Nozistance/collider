(ns collider.persist.snapshot
  "The world as a store keeps it, and its bytes."
  (:require [collider.data.long-map :as lm]
            [collider.game.level :as level]
            [collider.game.schema :as schema]
            [collider.world.chunk :as chunk]
            [taoensso.nippy :as nippy]
            [taoensso.nippy.compression :refer [lz4-compressor]])
  (:import (collider.data LongMap LongSet)
           (collider.world Chunk)
           (java.io DataInput DataOutput)))

(set! *warn-on-reflection* true)

(nippy/extend-freeze Chunk ::chunk [^Chunk c ^DataOutput out]
  (chunk/save-chunk! c out))

(nippy/extend-thaw ::chunk [^DataInput in]
  (chunk/load-chunk in))

(nippy/extend-freeze LongMap ::longmap [m out]
  (nippy/freeze-to-out! out (lm/key-array m))
  (nippy/freeze-to-out! out (object-array (vals m))))

(nippy/extend-thaw ::longmap [in]
  (lm/from-sorted (nippy/thaw-from-in! in) (nippy/thaw-from-in! in)))

(nippy/extend-freeze LongSet ::longset [s out]
  (nippy/freeze-to-out! out (lm/key-array s)))

(nippy/extend-thaw ::longset [in]
  (lm/from-sorted (nippy/thaw-from-in! in)))

(def ^:private freeze-opts {:compressor lz4-compressor})

(defn freeze
  "Returns the bytes of a chunk payload. Bytes stay as they are."
  ^bytes [payload]
  (if (bytes? payload) payload (nippy/freeze payload freeze-opts)))

(defn thaw
  [^bytes data]
  (nippy/thaw data))

(defn meta-of
  "Returns the world as a store keeps it, without the chunks."
  [world]
  (let [level #(schema/snapshot (level/level world %) :level)]
    (assoc (schema/snapshot world :shared)
      :levels (into {} (map (fn [dim] [dim (level dim)]))
                    (keys (:levels world))))))

(defn- level-chunks [world dim]
  (let [lv (level/level world dim)
        groups (schema/chunk-entities (:entities lv))
        entry (fn [id] [id (schema/chunk-payload lv id (groups id))])]
    (into {} (map entry) (keys (:chunks lv)))))

(defn snapshot
  "Returns the world as a store keeps it.
  Each level holds every chunk it has loaded and what belongs to it.
  The shared part holds the players of every level."
  [world]
  (reduce (fn [m dim]
            (assoc-in m [:levels dim :chunks]
                      (level-chunks world dim)))
          (meta-of world) (keys (:levels world))))

(def ^:private chunk-keys
  [:chunks :entities :block-ticks :fluid-ticks
   :block-entities])

(def ^:private level-defaults
  (get-in schema/initial-world [:levels :overworld]))

(defn- level-world-of [tick lm]
  (let [empty-parts (select-keys level-defaults chunk-keys)
        bare (dissoc lm :chunks :stored)
        base (assoc (schema/level-of bare) :tick tick)
        start (merge empty-parts base)]
    (-> (reduce-kv schema/with-chunk start (:chunks lm))
        (dissoc :tick)
        (assoc :stored (or (:stored lm) (lm/long-set))))))

(defn world-of
  "Returns the world that a snapshot holds.
  A level that the snapshot lacks is empty."
  [snap]
  (let [shared (schema/shared-of (dissoc snap :levels))
        tick (long (:tick shared 0))]
    (level/synced
      (assoc shared :levels
             (into (:levels schema/initial-world)
                   (for [[dim lm] (:levels snap)]
                     [dim (level-world-of tick lm)]))))))
