(ns collider.world.gen
  "The flat chunk every new chunk of a dimension starts from."
  (:require [collider.data :as data]
            [collider.world.block :as block]
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

(defn- sky? [dim] (:has-skylight (data/dimension-type dim) true))

(defn- flat-section [dim]
  (let [bs (short-array 4096)
        states (mapv block/state (layers dim))]
    (dotimes [i 4096]
      (let [y (quot i 256)
            st (long (if (< y 4) (nth states y) 0))]
        (aset bs i (short st))))
    (chunk/section bs nil (when (sky? dim) (flat-sky)))))

(def ^:private dark-top
  (chunk/section (short-array 4096) nil nil))

(defn- flat-of [dim]
  (chunk/chunk-of
    (cond-> (assoc (vec (repeat chunk/section-count nil))
                   (chunk/section-index 0) (flat-section dim))
      (not (sky? dim))
      (assoc (dec chunk/section-count) dark-top))))

(def ^:private ^:table flat
  (delay (into {} (for [dim (keys layers)]
                    [dim (flat-of dim)]))))

(defn flat-chunk
  "Returns the flat chunk of dimension dim."
  [dim]
  (get @flat dim))
