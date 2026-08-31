(ns collider.world.gen
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
      (let [y  (quot i 256)
            st (long (case y 0 (block/state :bedrock), (1 2) (block/state :dirt), 3 (block/state :grass-block), 0))]
        (aset bs i (short st))))
    (chunk/->Section bs (byte-array 2048) (flat-sky))))

(def flat-chunk
  {:sections (assoc (vec (repeat chunk/section-count nil)) (chunk/section-index 0) (flat-section))})
