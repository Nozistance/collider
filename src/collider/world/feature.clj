(ns collider.world.feature
  "Placing the worldgen features that bone meal reaches."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.support :as support])
  (:import (collider.java Noise)))

(set! *warn-on-reflection* true)

(defn- pick ^long [roll salt ^long n]
  (long (Math/floor (* (double (roll salt)) n))))

(defn- one-of [xs roll salt]
  (nth xs (pick roll salt (count xs))))

(defn- tag-of [v]
  (if (map? v) (:tag v) (data/snake v)))

(defn- state-of ^long [m]
  (if-let [props (:props m)]
    (block/state (:block m) props)
    (block/state (:block m))))

(defn start
  "Returns a placement that reads and changes chunks."
  [chunks]
  {:chunks chunks :cells []})

(defn cells
  "Returns the [pos state] changes a placement made, one for each position."
  [acc]
  (let [final (into {} (:cells acc))]
    (into [] (comp (map first) (distinct) (map (fn [p] [p (final p)])))
          (:cells acc))))

(defn chunks
  "Returns the world a placement reads."
  [acc]
  (:chunks acc))

(defn state-at
  "Returns the block state a placement sees at p."
  ^long [acc p]
  (gen/at (:chunks acc) p))

(defn set-state
  "Returns the placement with the block state st put at p."
  [acc p ^long st]
  (-> acc
      (update :chunks chunk/chunks-set-blocks (gen/flat-chunk) [[p st]])
      (update :cells conj [p st])))

(defn- air-at? [acc p]
  (and (chunk/in-range? (p 1)) (zero? (state-at acc p))))

(defn- noise-key [m k]
  (let [n (get m k)] [(long (:seed m)) (int (:firstOctave n)) (:amplitudes n)]))

(defn- noise-keys [m]
  (keep #(when (get m %) (noise-key m %)) [:noise :slow-noise]))

(defn- deep-noises [v]
  (cond
    (map? v) (concat (when (:seed v) (noise-keys v))
                     (mapcat deep-noises (vals v)))
    (vector? v) (mapcat deep-noises v)))

(def ^:private ^:table noises
  (delay (into {} (map (fn [[seed octave amps :as k]]
                         [k (Noise/of seed octave (double-array amps))]))
               (distinct (deep-noises (:configured (data/features)))))))

(defn- fast-noise ^double [m p]
  (.at ^Noise (get @noises (noise-key m :noise))
       (double (p 0)) (double (p 1)) (double (p 2)) (float (:scale m))))

(defn- slow-noise ^double [m p]
  (.atFloat ^Noise (get @noises (noise-key m :slow-noise))
            (int (p 0)) (int (p 1)) (int (p 2)) (float (:slow-scale m))))

(defn- noise-state [states ^double v]
  (nth states (long (* (min 0.9999 (max 0.0 (/ (+ 1.0 v) 2.0))) (count states)))))

(defn- threshold-provider [m p roll salt]
  (let [v (fast-noise m p)]
    (cond
      (< v (double (float (:threshold m))))
      (one-of (:low-states m) roll (conj salt :low))
      (< (double (roll (conj salt :high))) (double (float (:high-chance m))))
      (one-of (:high-states m) roll (conj salt :pick))
      :else (:default-state m))))

(defn- variety-count ^long [m p]
  (let [[lo hi] (:variety m)
        a (double lo) b (double (inc (long hi)))
        v (slow-noise m p)]
    (long (max a (min b (+ a (* (/ (+ v 1.0) 2.0) (- b a))))))))

(defn- dual-provider [m p]
  (let [n (variety-count m p)
        spread (fn [^long i]
                 [(+ (long (p 0)) (* i 54545)) (p 1)
                  (+ (long (p 2)) (* i 34234))])
        slow #(noise-state (:states m) (slow-noise m (spread %)))
        states (mapv slow (range n))]
    (noise-state states (fast-noise m p))))

(defn- weighted-provider [entries roll salt]
  (loop [left (pick roll salt (reduce + (map :weight entries))) es entries]
    (let [w (long (:weight (first es)))]
      (if (< left w) (:data (first es)) (recur (- left w) (rest es))))))

(defn- provide [m p roll salt]
  (case (:type m)
    :simple-state-provider (:state m)
    :weighted-state-provider (weighted-provider (:entries m) roll salt)
    :noise-provider (noise-state (:states m) (fast-noise m p))
    :noise-threshold-provider (threshold-provider m p roll salt)
    :dual-noise-provider (dual-provider m p)))

(defn- double-plant [acc p ^long st]
  (if-not (air-at? acc (dir/up p))
    acc
    (let [self (block/block-of st)]
      (-> acc
          (set-state p (block/state self (assoc (block/props-of st) :half :lower)))
          (set-state (dir/up p) (block/state self {:half :upper}))))))

(defn- mossy-carpet [acc p roll salt]
  (let [side? (fn [d] (< (double (roll (conj salt d))) 0.5))
        fresh (block/state :pale-moss-carpet)
        base (moss/carpet-updated (:chunks acc) p fresh true)
        acc (set-state acc p base)
        top (moss/carpet-topper (:chunks acc) p side?)]
    (if (nil? top)
      acc
      (let [acc (set-state acc (dir/up p) top)]
        (set-state acc p (moss/carpet-updated (:chunks acc) p base true))))))

(defn- simple-block [acc cfg p roll salt]
  (let [st (state-of (provide (:to-place cfg) p roll salt))]
    (if-not (support/supported? (:chunks acc) (gen/flat-chunk) p st)
      acc
      (case (block/type-of st)
        :double-plant (double-plant acc p st)
        :mossy-carpet (mossy-carpet acc p roll salt)
        (set-state acc p st)))))

(defn- sample ^long [v roll salt]
  (if (number? v)
    (long v)
    (let [lo (long (:min-inclusive v))]
      (+ lo (pick roll salt (inc (- (long (:max-inclusive v)) lo)))))))

(defn- slide [acc p n want step]
  (loop [q p i 0]
    (if (and (< i (long n)) (want (state-at acc q))) (recur (step q) (inc i)) q)))

(defn- patch-ground [acc cfg p]
  (let [n (long (:vertical-range cfg))
        q (slide acc (slide acc p n zero? dir/down) n (complement zero?) dir/up)
        below (dir/down q)]
    (when (and (air-at? acc q) (block/face-sturdy? (state-at acc below) :up))
      below)))

(defn- place-ground [acc cfg below depth roll salt]
  (loop [acc acc q below i 0]
    (if (= i (long depth))
      [acc true]
      (let [st (state-of (provide (:ground-state cfg) q roll (conj salt i)))
            prev (state-at acc q)]
        (cond
          (= (block/block-of st) (block/block-of prev)) (recur acc q (inc i))
          (not (block/tagged? prev (tag-of (:replaceable cfg)))) [acc (not= 0 i)]
          :else (recur (set-state acc q st) (dir/down q) (inc i)))))))

(defn- patch-columns [^long xr ^long zr]
  (for [dx (range (- xr) (inc xr))
        dz (range (- zr) (inc zr))
        :let [ex (or (= dx (- xr)) (= dx xr)) ez (or (= dz (- zr)) (= dz zr))]
        :when (not (and ex ez))]
    [dx dz (or ex ez)]))

(defn- edge-column? [cfg roll salt]
  (let [c (double (:extra-edge-column-chance cfg))]
    (and (not (zero? c)) (<= (double (roll (conj salt :edge))) c))))

(defn- patch-cell [[acc surface :as state] cfg p roll salt [dx dz edge?]]
  (let [s (conj salt dx dz)
        q (mapv + p [dx 0 dz])
        below (when (or (not edge?) (edge-column? cfg roll s))
                (patch-ground acc cfg q))]
    (if (nil? below)
      state
      (let [d (sample (:depth cfg) roll (conj s :depth))
            [acc grown?] (place-ground acc cfg below d roll s)]
        [acc (cond-> surface grown? (conj below))]))))

(declare placed)

(defn- vegetation [acc cfg surface roll salt]
  (let [c (double (:vegetation-chance cfg))]
    (reduce (fn [acc q]
              (if (and (pos? c) (< (double (roll (conj salt q :grow))) c))
                (placed acc (:vegetation-feature cfg) (dir/up q) roll (conj salt q))
                acc))
            acc surface)))

(defn- vegetation-patch [acc cfg p roll salt]
  (let [xr (inc (sample (:xz-radius cfg) roll (conj salt :xr)))
        zr (inc (sample (:xz-radius cfg) roll (conj salt :zr)))
        [acc surface] (reduce #(patch-cell %1 cfg p roll salt %2)
                              [acc []] (patch-columns xr zr))]
    (vegetation acc cfg surface roll salt)))

(defn configured
  "Returns the placement with the configured feature placed at p."
  [acc feature p roll salt]
  (let [m (if (keyword? feature)
            (get-in (data/features) [:configured feature])
            feature)
        salt (conj salt feature)]
    (case (:type m)
      :simple-block (simple-block acc (:config m) p roll salt)
      :vegetation-patch (vegetation-patch acc (:config m) p roll salt)
      acc)))

(defn- keeps? [acc m p]
  (case (:type m)
    :block-predicate-filter
    (block/tagged? (state-at acc p) (tag-of (:tag (:predicate m))))
    true))

(defn placed
  "Returns the placement with the placed feature put at p, when its placement
   lets it stand there."
  [acc feature p roll salt]
  (let [m (if (keyword? feature)
            (get-in (data/features) [:placed feature])
            feature)]
    (if (every? #(keeps? acc % p) (:placement m))
      (configured acc (:feature m) p roll (conj salt feature))
      acc)))

(defn bone-meal-features
  "Returns the features bone meal grows in a biome."
  [biome]
  (get-in (data/features) [:bone-meal biome]))

(defn placer-feature
  "Returns the feature a block grows from bone meal, nil when it grows none."
  [block]
  (get-in (data/features) [:placers block]))
