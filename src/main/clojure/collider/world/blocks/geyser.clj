(ns collider.world.blocks.geyser
  "The geyser of potent sulfur under water and its countdown."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const water-reach 4)

(def ^:private ^:const reach-per-water 6)

(defn phase
  "Returns the phase of potent sulfur st, one of :dry, :wet,
  :dormant, :erupting and :continuous, or nil for another block."
  [^long st]
  (:potent-sulfur-state (block/props-of st)))

(defn- passable? [^long st]
  (or (= :water (block/block-of st))
      (= :scaffolding (block/type-of st))
      (empty? (block/collision-boxes st))))

(defn- through? [^long st]
  (and (block/holds-water-source? st) (passable? st)))

(defn source
  "Returns the cell above the water over the sulfur at pos, where
  its gas comes out, or nil when the water is too deep or capped."
  [chunks [x y z]]
  (loop [i 1]
    (when (<= i (inc water-reach))
      (let [q [x (+ (long y) i) z]
            st (chunk/at chunks q)]
        (cond
          (through? st) (recur (inc i))
          (passable? st) q)))))

(defn water-depth
  "Returns how many water blocks stand between pos and source q."
  ^long [[_ y _] [_ qy _]]
  (- (long qy) (long y) 1))

(defn reach
  "Returns how many cells above pos the geyser pushes through, up to
  six per water block."
  ^long [chunks [x y z] ^long depth]
  (let [top (* reach-per-water depth)
        at #(chunk/at chunks [x (+ (long y) 1 (long %)) z])]
    (loop [i 0]
      (cond
        (= i top) top
        (passable? (at i)) (recur (inc i))
        :else i))))

(defn- drawn ^long [pos k ^long lo ^long hi]
  (random/between (random/of-key pos :geyser k) lo hi))

(defn- countdown-from ^long [pos ph ^long depth]
  (if (= :dormant ph)
    (+ (* 10 (dec depth)) (drawn pos 0 15 30))
    (+ (dec depth) (drawn pos 1 1 2))))

(defn counted
  "Returns the countdown of the geyser at pos after its step."
  ^long [pos ph ^long depth ^long countdown]
  (let [c (if (<= countdown 0)
            (countdown-from pos ph depth)
            countdown)]
    (if (pos? c) (dec c) c)))

(defn turned
  "Returns the state a geyser takes when its countdown runs out."
  ^long [^long st]
  (let [ph (if (= :dormant (phase st)) :erupting :dormant)]
    (block/with st :potent-sulfur-state ph)))

(defn- counting? [ph] (contains? #{:dormant :erupting} ph))

(defn placed-fx
  "Returns the effects of placing st. A geyser that starts is heard
  and runs its block event."
  [^long st]
  (when (#{:erupting :continuous} (phase st))
    [[:geyser-start st]]))

(defn shaped-fx
  "Returns the effect of a shape update that turns sulfur old into
  st. A new geyser starts its countdown anew."
  [^long old ^long st]
  (when (and (= :potent-sulfur (block/type-of old))
             (not (counting? (phase old)))
             (counting? (phase st)))
    [:reset-countdown]))
