(ns collider.world.gen
  "The flat world: the chunk every loaded chunk starts from, and block reads
   against it."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- flat-sky ^bytes []
  (let [arr (byte-array 2048)]
    (dotimes [i 4096]
      (when (>= (quot i 256) 4)
        (chunk/nibble-set! arr i 15)))
    arr))

(defn- flat-section []
  (let [bs (short-array 4096)]
    (dotimes [i 4096]
      (let [y (quot i 256)
            st (long (case y 0 (block/state :bedrock), (1 2) (block/state :dirt), 3 (block/state :grass-block), 0))]
        (aset bs i (short st))))
    (chunk/section bs nil (flat-sky))))

(def ^:private ^:table flat
  (delay (chunk/chunk-of
           (assoc (vec (repeat chunk/section-count nil))
                  (chunk/section-index 0) (flat-section)))))

(defn flat-chunk
  "Returns the chunk every loaded chunk starts from."
  [] @flat)

(defn at
  "Returns the block state at p, air where no chunk is loaded and air outside
   the world height."
  ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks (flat-chunk) p) 0))

(defn at-void
  "Returns the block state at p, -1 outside the world height."
  ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks (flat-chunk) p) -1))
