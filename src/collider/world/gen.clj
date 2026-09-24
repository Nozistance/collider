(ns collider.world.gen
  "The flat chunk every new chunk of a dimension starts from."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private layers
  {:overworld  [:bedrock :dirt :dirt :grass-block]
   :the-nether [:bedrock :netherrack :netherrack :netherrack]
   :the-end    [:bedrock :end-stone :end-stone :end-stone]})

(defn- flat-sky ^bytes []
  (let [arr (byte-array 2048)]
    (dotimes [i 4096]
      (when (>= (quot i 256) 4)
        (chunk/nibble-set! arr i 15)))
    arr))

(defn- flat-section [dim]
  (let [bs (short-array 4096)
        states (mapv block/state (layers dim))]
    (dotimes [i 4096]
      (let [y (quot i 256)
            st (long (if (< y 4) (nth states y) 0))]
        (aset bs i (short st))))
    (chunk/section bs nil (flat-sky))))

(defn- flat-of [dim]
  (chunk/chunk-of
    (assoc (vec (repeat chunk/section-count nil))
           (chunk/section-index 0) (flat-section dim))))

(def ^:private ^:table flat
  (delay (into {} (for [dim (keys layers)]
                    [dim (flat-of dim)]))))

(defn flat-chunk
  "Returns the flat chunk of dimension dim."
  [dim]
  (get @flat dim))
