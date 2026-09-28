(ns collider.world.light
  "Block light, sky light, and the sky brightness of the day cycle."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.java Light)
           (java.util HashMap)))

(set! *warn-on-reflection* true)

(defn- pass!
  "Spreads channel ch of the cells, [x y z level] each, into cache."
  [cache chunks ch cells]
  (Light/pass cache chunks ch cells block/dampening block/emits
              block/shape-occludes?))

(defn- relit [c [[_ k] ^bytes arr]]
  (let [k (long k)
        si (int (bit-and (bit-shift-right k 1) 31))
        s (or (chunk/chunk-section c si) (chunk/new-section c si))]
    (chunk/with-section c si
      (if (= (bit-and k 1) Light/SKY)
        (chunk/with-sky-light s arr)
        (chunk/with-block-light s arr)))))

(defn- rebuild [chunks ^HashMap cache]
  (let [ks (reverse (sort (keys cache)))
        entry (fn [k]
                [[(bit-shift-right (long k) 6) k] (.get cache k)])]
    (chunk/with-chunks chunks relit
      (partition-by ffirst (map entry ks)))))

(defn light-at
  "Returns the brighter of the sky and block light at x y z."
  {:inline (fn [c x y z]
             `(Light/at ~c (long ~x) (long ~y) (long ~z)))}
  ^long [chunks x y z]
  (Light/at chunks (long x) (long y) (long z)))

(defn block-light-at
  "Returns the block light at x y z."
  {:inline (fn [c x y z]
             `(Light/blockAt ~c (long ~x) (long ~y) (long ~z)))}
  ^long [chunks x y z]
  (Light/blockAt chunks (long x) (long y) (long z)))

(defn sky-light-at
  "Returns the sky light at x y z, full above the world."
  {:inline (fn [c x y z]
             `(Light/skyAt ~c (long ~x) (long ~y) (long ~z)))}
  ^long [chunks x y z]
  (Light/skyAt chunks (long x) (long y) (long z)))

(defn sky-light-level
  "Returns the brightness of the sky, 0.0 to 15.0, at a time of day.
  The rain and thunder levels, 0.0 to 1.0, dim it."
  {:inline (fn
             ([t] `(Light/skyLevel (long ~t) 0.0 0.0))
             ([t r h]
              `(Light/skyLevel (long ~t) (double ~r) (double ~h))))
   :inline-arities #{1 3}}
  (^double [^long time] (Light/skyLevel time 0.0 0.0))
  (^double [^long time ^double rain-level ^double thunder-level]
   (Light/skyLevel time rain-level thunder-level)))

(defn sky-darken
  "Returns how much the sky light is dimmed, 0 to 15.
  The time of day and the weather decide."
  {:inline (fn
             ([t] `(Light/darken (long ~t) 0.0 0.0))
             ([t r h]
              `(Light/darken (long ~t) (double ~r) (double ~h))))
   :inline-arities #{1 3}}
  (^long [^long time] (Light/darken time 0.0 0.0))
  (^long [^long time ^double rain-level ^double thunder-level]
   (Light/darken time rain-level thunder-level)))

(defn- brightness-form [chunks x y z time rain thunder]
  `(Light/brightness ~chunks (long ~x) (long ~y) (long ~z)
                     (long ~time) (double ~rain) (double ~thunder)))

(defn brightness
  "Returns the light level at x y z.
  The time of day and the weather dim the sky part."
  {:inline (fn [c x y z t & [r h]]
             (brightness-form c x y z t (or r 0.0) (or h 0.0)))
   :inline-arities #{5 7}}
  ([chunks x y z time] (brightness chunks x y z time 0.0 0.0))
  ([chunks x y z time rain-level thunder-level]
   (Light/brightness chunks (long x) (long y) (long z) (long time)
                     (double rain-level) (double thunder-level))))

(defn- different? [^long old ^long new]
  (and (not= old new)
       (or (not= (block/dampening old) (block/dampening new))
           (not= (block/emits old) (block/emits new))
           (block/use-shape-for-light-occlusion? old)
           (block/use-shape-for-light-occlusion? new))))

(defn- sky-full? [chunks x y z]
  (= 15 (Light/stored chunks Light/SKY x y z)))

(defn- sky-drop [chunks ^long x ^long z ^long src]
  (loop [yy (dec src) acc []]
    (if (and (chunk/in-range? yy) (sky-full? chunks x yy z))
      (recur (dec yy) (conj acc [x yy z 0]))
      acc)))

(defn- sky-add [chunks ^long x ^long z ^long src]
  (loop [yy src acc []]
    (if (and (<= yy chunk/max-y) (not (sky-full? chunks x yy z)))
      (recur (inc yy) (conj acc [x yy z 15]))
      acc)))

(defn- sky-source
  "Returns the lowest y of the column x z that the sky reaches."
  ^long [chunks ^long x ^long z]
  (Light/skySource chunks x z block/dampening block/shape-occludes?))

(defn- sky-column [chunks x z ys]
  (let [x (long x) z (long z)
        src (sky-source chunks x z)
        drop (sky-drop chunks x z src)
        add (sky-add chunks x z src)
        seen (into #{} (map second) (concat drop add))
        cell (fn [y]
               (when-not (seen y)
                 [x y z (if (>= (long y) src) 15 0)]))]
    (into (into drop add) (keep cell) ys)))

(defn- columns [changed]
  (let [m (HashMap.)]
    (doseq [[[x y z] _ _] changed]
      (let [k (chunk/pos->id x z)]
        (.put m k (conj (.getOrDefault m k #{}) y))))
    m))

(defn- column-cells [chunks [k ys]]
  (let [[x z] (chunk/id->pos k)]
    (sky-column chunks x z ys)))

(defn- sky-cells [chunks changed]
  (into [] (mapcat #(column-cells chunks %)) (columns changed)))

(defn- light-changed? [[_ old new]]
  (different? (long old) (long new)))

(defn- block-cells [changed]
  (mapv (fn [[[x y z] _ new]] [x y z (block/emits (long new))])
        changed))

(defn- relight-changed [chunks changed bcells sky?]
  (let [cache (HashMap.)]
    (pass! cache chunks 0 bcells)
    (when sky?
      (pass! cache chunks Light/SKY (sky-cells chunks changed)))
    (if (.isEmpty cache) chunks (rebuild chunks cache))))

(defn relight-batch
  "Returns chunks relit after changes, [pos old new] each.
  Sky light moves only when sky? is true."
  ([chunks changes] (relight-batch chunks changes true))
  ([chunks changes sky?]
   (let [changed (filter light-changed? changes)
         bcells (block-cells changed)]
     (if (empty? bcells)
       chunks
       (relight-changed chunks changed bcells sky?)))))

(defn relight
  "Returns chunks relit after the block at pos changed state."
  [chunks pos old-state new-state]
  (relight-batch chunks [[pos old-state new-state]]))
