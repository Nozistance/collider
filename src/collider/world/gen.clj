(ns collider.world.gen
  (:require [collider.world.chunk :as chunk]))

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
            id (long (case y 0 7, (1 2) 3, 3 2, 0))]
        (aset bs i (short (bit-shift-left id 4)))))
    (chunk/->Section bs (byte-array 2048) (flat-sky))))

(def flat-chunk
  {:sections (assoc (vec (repeat 16 nil)) 0 (flat-section))})

(let [[bm data] (chunk/encode-column flat-chunk)]
  (def primary-bitmask bm)
  (def ^bytes flat-column data))

(def ^bytes unload-column (byte-array 256))
